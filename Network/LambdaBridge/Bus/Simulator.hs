{-# LANGUAGE KindSignatures, GADTs, FlexibleInstances, RankNTypes, ScopedTypeVariables, DataKinds #-}
module Network.LambdaBridge.Bus.Simulator where

import Network.LambdaBridge.Bridge
import Network.LambdaBridge.Bus

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

import Data.Array.MArray
import Data.Array.IO (IOUArray)

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Control.Monad.Operational

import Debug.Trace

-- Move the addresses of a command by a specific
offset :: Word16 -> BusM a -> BusM a
offset n = interp $ \ cmd -> case cmd of
        BusWrite addr val -> busWrite (addr + n) val
        BusRead addr      -> busRead (addr + n)

{-
myBus v1 = virtualBus
        [ (0,   rxMVarToRpc v1 >>= rpcPort)
        ]
-}

busAt :: Word16 -> Board -> Board
busAt n (Board f) = Board (f . offset (-n))

virtualBus :: [(Word16, Board)] -> Board
virtualBus xs = mconcat [ busAt n bus | (n,bus) <- xs ]

stateMachineToBus :: st -> (forall a . BusCmd a -> st -> IO (a, st)) -> IO Board
stateMachineToBus st stateMachine = do
        state <- newMVar st

        let cmdFn :: BusCmd a -> IO a
            cmdFn cmd = do
                    st <- takeMVar state
                    (r,st') <- stateMachine cmd st
                    putMVar state st'
                    return r

        return $ Board (fmap runRemote . interp cmdFn)


invalid :: Remote a
invalid = Symbol

data Go = Go | NoGo

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
                    return ((),(NoGo,if ok then x `xor` 0x01
                                           else x))
rxStateMachine _ (BusWrite _ _) st = return ((),st)

rxStateMachine _ (BusRead 0) (_,x) = return (pure x, (NoGo,x))
rxStateMachine _ (BusRead _) st    = return (invalid,st)

type TxState = (Go,Word8)

txInitialState :: TxState
txInitialState = (NoGo,0)

txStateMachine :: MVar Word8 -> BusCmd a -> TxState -> IO (a, TxState)

txStateMachine _ (BusWrite 0 w) st@(_,x) | w == x    = return ((),(Go,w))
                                         | otherwise = return ((),st)
txStateMachine v (BusWrite 1 w) (Go,x) = do
                    ok <- tryPutMVar v w
                    return ((),(NoGo,if ok then x `xor` 0x01
                                           else x))
txStateMachine _ (BusWrite _ _) st = return ((),st)

txStateMachine _ (BusRead 0) (_,x) = return (pure x, (NoGo,x))
txStateMachine v (BusRead 1) (Go,x) = do
                    r <- tryTakeMVar v
                    case r of
                      Nothing -> return (pure 0, (NoGo,x))
                      Just w  -> return (pure w, (NoGo, x `xor` 0x01))
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

testRX :: IO Word8
testRX = do
        print "textRX"
        v <- newEmptyMVar
        forkIO $ forever $ do
                x <- takeMVar v
                print ("MVar",x :: Word8)
                return ()

        brd <- stateMachineToBus rxInitialState $ rxStateMachine v
        -- starts at 0
        let loop (x:xs) t = do
                Just t' <- send brd $ do
                        busWrite 0 t
                        busWrite 1 x
                        busRead 0

                yield

                send brd $ do
                        busWrite 0 t
                        busWrite 1 x
                        busRead 0

                yield
                if (t /= t') then loop xs t' else loop (x:xs) t'

        loop [0..] 0


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

