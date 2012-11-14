{-# LANGUAGE KindSignatures, GADTs, FlexibleInstances, RankNTypes, ScopedTypeVariables, DataKinds #-}
module Network.LambdaBridge.Bus where

import Network.LambdaBridge.Bridge


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

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Control.Monad.Operational

-------------------------------------------------------------------

-- | 'Board' is a handle into a board
newtype Board = Board
  { send :: forall a . BusM (Remote a) -> IO (Maybe a)      -- ^ run a transaction on the 'Board', please.
  }

send_ :: Board -> BusM a -> IO (Maybe ())
send_ brd m = send brd (m >> return (pure ()))

data Remote a
        = Symbol
        | Remote a

instance Functor Remote where
        fmap f Symbol = Symbol
        fmap f (Remote a) = Remote (f a)

instance Applicative Remote where
        pure a = Remote a
        Symbol <*> _ = Symbol
        _ <*> Symbol = Symbol
        (Remote f) <*> (Remote a) = Remote (f a)

data BusCmd :: * -> * where
        BusWrite :: Word16 -> Word8     -> BusCmd ()
        BusRead  :: Word16              -> BusCmd (Remote Word8)

type BusM a = Program BusCmd a

done :: BusM (Remote ())
done = return (pure ())

busWrite addr val = singleton $ BusWrite addr val
busRead addr      = singleton $ BusRead addr

cmdToRequest :: BusM a -> Writer [Word8] a
cmdToRequest = interp $ \ cmd -> case cmd of
        BusWrite addr val -> do
                tell $ [tagWrite] ++ seq16 addr ++ [val]
                return ()
        BusRead addr -> do
                tell $ [tagRead] ++ seq16 addr
                return Symbol

interp :: forall a c m . (Monad m) => (forall a . c a -> m a) -> Program c a -> m a
interp f = eval . view
  where eval :: forall a . ProgramView c a -> m a
        eval (Return a) = return a
        eval (m :>>= i) = do
                r <- f m
                interp f (i r)

-- reifyBusCmd recreates a monadic program from its Byte sequence
-- which performs the actions of concatinating the results of the reads.

reifyBusCmd :: [Word8] -> BusM (Remote [Word8])
reifyBusCmd (tag:h1:l1:val:rest) | tag == tagWrite = do
        busWrite addr val
        reifyBusCmd rest
  where
          addr = unseq16 [h1,l1]
reifyBusCmd (tag:h1:l1:rest) | tag == tagRead = do
        val <- busRead addr
        rest <- reifyBusCmd rest
        return (liftA2 (:) val rest)
  where
          addr = unseq16 [h1,l1]
reifyBusCmd [] = return (pure [])
reifyBusCmd _ = fail "bad bytes for reifyBus"
{-
prop_reqReify cmd = xs == cmdToRequest (reifyBusCmd xs)
  where xs = cmdToRequest cmd
-}

cmdWithReply :: BusM a -> StateT [Word8] Maybe a
cmdWithReply = interp $ \ cmd -> case cmd of
        BusWrite addr val -> return ()
        BusRead addr -> do
                val <- item
                return (Remote val)

item :: StateT [Word8] Maybe Word8
item = do
        chs <- get
        case chs of
          [] -> fail "item failed"
          (c:cs) -> do put cs
                       return c

tagWrite, tagRead :: Word8
tagWrite = 0x1
tagRead = 0x2

hi, low :: Word16 -> Word8
hi = fromIntegral . (`div` 256)
low = fromIntegral . (`mod` 256)

seq16 :: Word16 -> [Word8]
seq16 v = [hi v,low v]

unseq16 :: [Word8] -> Word16
unseq16 [h,l] = fromIntegral h * 256 + fromIntegral l

----------------------------------------------------------------

-- | connectBoard takes an initial timeout time,
--  and a Bridge Frame to the board, and returns
-- an abstact handle to the physical board.
connectToBoard :: Float -> Bridge Framed Checked -> IO Board
connectToBoard timeoutTime bridge = do

        uniq :: MVar Word16 <- newEmptyMVar
        forkIO $ let loop n = do
                        putMVar uniq n
                        loop (succ n)
                 in loop 0

        callbacks :: Callback Word16 (Maybe [Word8]) <- liftM Callback $ newMVar Map.empty

        forkIO $ forever $ do
                bs0 <- fromBridge bridge
                case readBusFrame bs0 of
                  Just (BusFrame uq rest) -> callback callbacks uq (Just rest)
                  Nothing                 -> return () -- faulty packet?

        return $ Board
          { send = \ cmd -> do
                let (_,req_msg) = runWriter (cmdToRequest cmd)
                print req_msg

                uq <- takeMVar uniq

                rep :: MVar (Maybe [Word8]) <- newEmptyMVar

                -- register the callback
                register callbacks uq $ putMVar rep
                -- set up a delayed timeout response
                forkIO $ do
                        threadDelay (round (timeoutTime * 1000 * 1000))
                        callback callbacks uq Nothing

                toBridge bridge $ showBusFrame $ BusFrame uq req_msg

                -- And wait for the callback
                rep_msg <- takeMVar rep

                case rep_msg of
                  Nothing -> return Nothing
                  Just rep_bs -> case runStateT (cmdWithReply cmd) rep_bs of
                                    Just (Remote a,[]) -> return (Just a)
                                    Just (a,_)  -> return Nothing -- fail "bad format, bad remote, etc"
                                    Nothing     -> return Nothing
          }

-----------------------------------------------------------------------------

-- General code
data Callback k a = Callback (MVar (Map k (a -> IO ())))

register :: (Ord k) => Callback k a -> k -> (a -> IO ()) -> IO ()
register (Callback callbacks) key callback = do
        fm <- takeMVar callbacks
        putMVar callbacks (insert key callback fm)

callback :: (Ord k) => Callback k a -> k -> a -> IO ()
callback (Callback callbacks) key val = do
        fm <- takeMVar callbacks
        case Map.lookup key fm of
          Nothing -> putMVar callbacks fm       -- done
          Just f -> do
                putMVar callbacks (delete key fm)
                f val

-----------------------------------------------------------------------------

data BusFrame = BusFrame Word16 [Word8]
        deriving (Show)

-- TODO: Make these use [Word8], not ByteString
-- Add a BusReply [Word8] as well
readBusFrame :: BS.ByteString -> Maybe BusFrame
readBusFrame bs0 =
        case do (a,bs1) <- BS.uncons bs0
                (b,bs2) <- BS.uncons bs1
                return (unseq16 [a,b],bs2) of
          Just (uq,rest) -> return (BusFrame uq (BS.unpack rest))
          Nothing -> fail "bad packet format"

showBusFrame :: BusFrame -> BS.ByteString
showBusFrame (BusFrame uq msg) = BS.append (BS.pack (seq16 uq)) (BS.pack msg)

-- This is used for testing the serialization of the Board commands

interpBus :: Board -> IO (Bridge Framed Checked)
interpBus (Board cmd) = do
        cmdChan <- newChan
        resChan <- newChan

        forkIO $ forever $ do
                frame <- readChan cmdChan
                case readBusFrame frame of
                  Nothing -> return ()
                  Just (BusFrame uq ws) -> do
                        ret <- cmd $ reifyBusCmd ws
                        case ret of
                          Nothing -> return ()
                          Just ws' -> writeChan resChan (showBusFrame $ BusFrame uq ws')
                        return ()

        return $ Bridge { toBridge = writeChan cmdChan
                        , fromBridge = readChan resChan
                        }


rpc :: ([Word8] -> IO [Word8]) -> IO (BusM (Remote a) -> IO (Maybe a))
rpc f = undefined


-- Move the addresses of a command by a specific
offset :: Word16 -> BusM a -> BusM a
offset n = interp $ \ cmd -> case cmd of
        BusWrite addr val -> busWrite (addr + n) val
        BusRead addr      -> busRead (addr + n)


myBus v1 = virtualBus
        [ (0,   rxMVarToRpc v1 >>= rpcPort)
        ]


virtualBus :: [(Word16, IO VirtualBusCommand)] -> IO VirtualBusCommand
virtualBus = Prelude.foldr fn
           $ return
           $ VirtualBusCommand
           $ \ _ -> return Nothing
  where
          fn (off, fn) rest = do
                  VirtualBusCommand this <- fn
                  VirtualBusCommand that <- rest
                  return $ VirtualBusCommand $ \ cmd -> do
                        r <- this (offset (-off) cmd)   -- normalize command to zero
                        case r of
                          Nothing -> that cmd
                          Just r -> return (Just r)

newtype VirtualBusCommand = VirtualBusCommand (forall a. BusM (Remote a) -> IO (Maybe a))

data Rpc = Rpc
        { push :: Word8 -> IO ()
        , pull :: IO Word8
        , tag  :: IO Word8
        }

nullRpc t = Rpc { push = \ _ -> return ()
                , pull = return 0
                , tag = return t
                }

rxMVarToRpc :: MVar Word8 -> IO (Word8 -> IO Rpc)
rxMVarToRpc mvar = do
        cur_tag <- newMVar (0 :: Word8)

        pushFn <- newEmptyMVar

        let pushVal0 val = do
                b <- tryPutMVar mvar val
                case b of
                  True -> do
                        modifyMVar_ cur_tag (return . (`xor` 0x01))
                        putMVar pushFn pushVal1
                  False -> do
                        putMVar pushFn pushVal1
                return ()
            pushVal1 _ = putMVar pushFn pushVal1

        return $ \ t -> do
           t_s <- readMVar cur_tag
           -- start the ball rolling
           _ <- tryTakeMVar pushFn
           putMVar pushFn pushVal0
           return $ if t == t_s then Rpc { push = \ val -> takeMVar pushFn >>= \ f -> f val
                                         , pull = return 0
                                         , tag  = readMVar cur_tag
                                         }
                    else nullRpc t_s

txMVarToRpc :: MVar Word8 -> IO (Word8 -> IO Rpc)
txMVarToRpc mvar = do
        cur_tag <- newMVar (0 :: Word8)

        pullFn <- newEmptyMVar

        let pullVal0 = do
                b <- tryTakeMVar mvar
                case b of
                  Just val-> do
                        modifyMVar_ cur_tag (return . (`xor` 0x01))
                        putMVar pullFn pullVal1
                        return val
                  Nothing -> do
                        putMVar pullFn pullVal1
                        return 0
            pullVal1 = do
                    putMVar pullFn pullVal1
                    return 0

        return $ \ t -> do
           t_s <- readMVar cur_tag
           -- start the ball rolling
           _ <- tryTakeMVar pullFn
           putMVar pullFn pullVal0
           return $ if t == t_s then Rpc { push = \ _ -> return ()
                                         , pull = takeMVar pullFn >>= \ f -> f
                                         , tag  = readMVar cur_tag
                                         }
                    else nullRpc t_s


rpcPort :: (Word8 -> IO Rpc) -> IO VirtualBusCommand
rpcPort call = do
        state <- newMVar $ nullRpc 0
        let getState m = do
                r <- takeMVar state
                m r
        let newState n = do
                putMVar state n

        let fCmd :: Rpc -> BusCmd a -> IO a
            fCmd _ (BusWrite 0 val) = do
                   rpc <- call val
                   modifyMVar_ state (\ _ -> return rpc)
                   return ()
            fCmd rpc (BusWrite 1 val) = do
                   push rpc val
                   return ()
            fCmd rpc (BusRead 0) = do
                   ret <- tag rpc
                   return (pure ret)
            fCmd rpc (BusRead 1) = do
                   ret <- pull rpc
                   return (pure ret)

        return $ VirtualBusCommand $ \ m -> do
                r <- interp (\ cmd -> do st <- readMVar state
                                         fCmd st cmd) m
                return Nothing



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

