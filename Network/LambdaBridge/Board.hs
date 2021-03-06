-- This needs folded into Bus, which is the same idea.

{-# LANGUAGE KindSignatures, GADTs, FlexibleInstances, RankNTypes, ScopedTypeVariables, DataKinds #-}
module                                    Network.LambdaBridge.Board                                    where

import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Bus hiding (debug)
import Network.LambdaBridge.Logging

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Applicative
import Data.Monoid
import Data.Word
import Data.Bits
import Control.Monad.Writer
import Control.Monad.State
import Data.Map as Map hiding (singleton)
import System.IO

import Data.Array.MArray
import Data.Array.IO (IOUArray)

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Control.Monad.Operational

import Debug.Trace

debug = debugM "lambda-bridge.sim"

newtype Board = Board (forall a . BusM (Remote a) -> IO (Maybe a))

boardToBus :: Float -> Board -> IO Bus
boardToBus t brd = do
        (b1,b2) <- connection
        interpBus (checked b2) brd
        bridgeToBus t (checked b1)

-- | Board is a Monoid (who would have thought)
instance Monoid Board where
    mempty = Board (\ cmd -> return Nothing)
    mappend (Board f1) (Board f2) = Board (fmap runRemote . interp cmdFn)
        where
            cmdFn :: BusCmd a -> IO a
            cmdFn (BusWrite addr val) = do
                    f1 (busWrite addr val >> return (pure ()))
                    f2 (busWrite addr val >> return (pure ()))
                    return ()
            cmdFn (BusRead addr) = do
                    r1 <- f1 (busRead addr)
                    r2 <- f2 (busRead addr)
                    return (optRemote (r1 <|> r2))

-- Move the addresses of a command by a specific
offset :: Word16 -> BusM a -> BusM a
offset n = interp $ \ cmd -> case cmd of
        BusWrite addr val -> busWrite (addr + n) val
        BusRead addr      -> busRead (addr + n)

-- TODO: consider calling this use of Bus/Bus VirtualBus, or simulator.
interpBus :: Bridge Framed Checked -> Board -> IO ()
interpBus bridge (Board cmd) = do

        forkIO $ forever $ do
                frame <- fromBridge bridge
                case readBusFrame frame of
                  Nothing -> return ()
                  Just (BusFrame uq ws) -> do
                        ret <- cmd $ reifyBusCmd ws
                        case ret of
                          Nothing -> return ()
                          Just ws' -> toBridge bridge (showBusFrame $ BusFrame uq ws')
                        return ()
        return ()


busAt :: Word16 -> Board -> Board
busAt n (Board f) = Board (f . offset (-n))

virtualBus :: [(Word16, Board)] -> Board
virtualBus xs = mconcat [ busAt n bus | (n,bus) <- xs ]

stateMachineToBus :: (Show st) => st -> (forall a . BusCmd a -> st -> IO (a, st)) -> IO Board
stateMachineToBus st stateMachine = do
        state <- newMVar st
        tag <- taggart

        let cmdFn :: BusCmd a -> IO a
            cmdFn cmd = do
                    debug $ tag ++ "got instructions {" ++ show cmd ++ "}"
                    st <- takeMVar state
                    debug $ tag ++ "st = " ++ show st
                    (r,st') <- stateMachine cmd st
                    debug $ tag ++ "done instructions, st' = " ++ show st'
                    putMVar state st'
                    debug $ tag ++ "returned value"
                    return r

        return $ Board (fmap runRemote . interp cmdFn)


invalid :: Remote a
invalid = Symbol

data Go = Go | NoGo
        deriving Show

type RxState = (Go,Word8)
--data RxState = RxReady Word8
--             | RxDone Word8
--          deriving (Show)

rxInitialState :: RxState
rxInitialState = (NoGo,0)

rxStateMachine :: MVar Word8 -> BusCmd a -> RxState -> IO (a, RxState)

rxStateMachine _ (BusWrite 0 w) st@(_,x) | w == x    = return ((),(Go,w))
                                         | otherwise = return ((),st)
rxStateMachine v (BusWrite 1 w) (Go,x) = do
                    ok <- tryPutMVar v w
                    return ((),(NoGo,if ok then x + 1
                                           else x))
rxStateMachine _ (BusWrite _ _) st = return ((),st)

rxStateMachine _ (BusRead 0) (_,x) = return (pure x, (NoGo,x))
rxStateMachine _ (BusRead _) st    = return (invalid,st)

type TxState = (Word8,Word8) -- vaild transaction * tag * value

txInitialState :: TxState
txInitialState = (-1,35)

txStateMachine :: MVar Word8 -> BusCmd a -> TxState -> IO (a, TxState)
txStateMachine v (BusWrite 0 w) (t,s)
     | t + 1 == w = do -- try read *next* byte in pipe
            r <- tryTakeMVar v
            case r of
              Nothing -> return ((),(t,s))
              Just v  -> return ((),(t + 1,v))
txStateMachine v (BusWrite 0 w) (t,s)         -- redundent but explicit
     | t == w = return ((),(t,s))
txStateMachine _ (BusWrite _ _) st = return ((),st)

-- request that the next tag be used; it will trigger the pull from the mvar
txStateMachine _ (BusRead 0) (t,s)   = return (pure (t + 1), (t,s))
txStateMachine v (BusRead 1) (t,s)   = return (pure s, (t,s))
txStateMachine _ (BusRead _) st    = return (invalid,st)

type MemState = ()

memStateMachine :: IOUArray Word16 Word8 -> (Word16,Word16) -> BusCmd a -> MemState -> IO (a, MemState)
memStateMachine arr bnds (BusWrite addr dat) st
        | inRange bnds addr = do
                writeArray arr addr dat
                return ((),st)
        | otherwise = return ((),st)
memStateMachine arr bnds (BusRead addr) st
        | inRange bnds addr = do
                r <- readArray arr addr
                return (pure r, st)
        | otherwise = return (invalid,st)

{-
testRX :: IO ()
testRX = do
        hSetBuffering stdout LineBuffering
        hSetBuffering stdout LineBuffering
        print "textRX"
        v1 <- newEmptyMVar
        v2 <- newEmptyMVar

        brd0 <- stateMachineToBus rxInitialState $ rxStateMachine v1
        brd2 <- stateMachineToBus txInitialState $ txStateMachine v1

        let brd = virtualBus
                [ (0, brd0)
                , (2, brd2)
                ]

        var <- newEmptyMVar
        sendToBus var brd 0

        let loop (x:xs) = do
--                print ("sending",x)
                putMVar var (BS.pack [x])
                yield   -- helps simulator
                loop xs

        forkIO $ loop (take 1000 $ cycle [0..255])

        var <- newEmptyMVar
        recvFromBus var brd 2

        let loop n = do
                x <- takeMVar var
                print  ("MVar",x)
{-
                if x == x then print ("MVar",x :: Word8)
                          else return ()
-}
--                if x == n + 1 then loop (n + 1) else print ("Bad Numbers",x,n,n+1)
                loop (n-1)

        loop (-1)
-}

-- memory :: IOUArray Word16 Word8 -> BusM (Remote [Word8]) -> IO (Maybe [Word8])

{-

memory :: IOUArray Word16 Word8 -> BusM (Remote [Word8]) -> IO (Maybe [Word8])

data WriterState = WriterStart | WriterPushed Word8 | WriterTagged Word8 Word8
        deriving (Eq,Show)

writer :: (Word8 -> Word8 -> IO Word8) -> IO (BusM (Remote [Word8]) -> IO (Maybe [Word8]))
writer push = do
        state <- newMVar WriterStart
        let getState m = do
                r <- takeMVar state
                m r
        let newState n = do
                putMVar state n

        let fCmd :: WriterState -> BusCmd a -> IO a
            fCmd WriterStart            (BusWrite addr val)     | addr == 0 = do
                   putMVar state $ WriterPushed val
                   return ()
            fCmd (WriterPushed s1)      (BusWrite addr val)     | addr == 1 = do
                   putMVar state $ WriterTagged s1 val
                   return ()
            fCmd (WriterTagged s1 tag)  (BusRead addr)          | addr == 2 = do
                   ret <- push s1 tag
                   putMVar state $ WriterStart
                   return (pure ret)

            fCmd _ (BusWrite {})      = do
                   putMVar state $ WriterStart
                   return ()

            fCmd _ (BusRead addr)      = do
                   putMVar state $ WriterStart
                   return Symbol        -- should never happen

        return $ \ m -> do
                r <- interp (\ cmd -> do st <- takeMVar state
                                         fCmd st cmd) m
                return Nothing
{-
                (\ cmd -> case cmd of
                        BusWrite addr val  | addr == 0 -> getState $ \ st -> case st of
                                WriterStart -> newState (WriterPushed val)
                                _           -> newState WriterStart
-}
{-
                        BusWrite addr tag  | addr == 1 -> getState $ \ st -> case st of
                                WriterPushed val -> newState (WriterTagged val tag)
                                _           -> newState WriterStart
-}
{-
                        BusRead addr       | addr == 2 -> do
                           getState $ \ st -> case st of
                                WriterTagged val tag -> do
                                        b <- push val
                                        if b then
                                _           -> newState WriterStart
                           return Symbol
-}
--                           ) m


-- NOTE: in the bytecode interpreter, we need to check the tag for legacy, and
-- do not execute out of order blocks, only new ones.

--reader :: IO Word8              -> BusM (Remote [Word8]) -> IO (Maybe [Word8])


{-
interp :: BusM a -> IO a
interp f = eval . view
  where eval :: forall a . ProgramView c a -> m a
        eval (Return a) = return a
        eval (m :>>= i) = do
                r <- f m
-}


--test2 = do
--        frameBridge <- interpBus $ interp $ \ cmd -> case cmd of
--                BusWrite addr val -> return ()
--                BusRead addr      -> return ()
--        return ()
-}

