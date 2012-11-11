{-# LANGUAGE KindSignatures, GADTs, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
module Network.LambdaBridge.Bus where

import Network.LambdaBridge.Bridge


import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Applicative
import Data.Monoid
import Data.Word
import Control.Monad.Writer
import Control.Monad.State
import Data.Map as Map hiding (singleton)

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Control.Monad.Operational


{-
data BusCmdPacket = BusCmdPacket U16 [BusCmd]

data BusCmd
        = WriteBus U16 U8               -- single read
        | ReadBus U16                   -- single write
        | ReplyBus U8                   -- Send back a specific msg

data BusReplyPacket = BusReplyPacket U16 [U8]
-}

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

        Pure     :: a                   -> BusCmd a
        Bind     :: BusCmd a
                 -> (a -> BusCmd b)     -> BusCmd b


type BusM a = Program BusCmd a

instance Applicative BusCmd where
        pure = Pure
        b1 <*> b2 = do
                f <- b1
                a <- b2
                return (f a)

instance Monad BusCmd where
        return = Pure
        (>>=) = Bind

instance Monoid (BusCmd ()) where
        mempty = pure ()
        mappend b1 b2 = b1 *> b2

instance Functor BusCmd where
        fmap f a = pure f <*> a

done :: BusM (Remote ())
done = return (pure ())

newtype Board = Board (Frame -> IO (Maybe Frame))       -- Will time out in a Board-specific way
--                      ((Frame -> IO ()) -> IO ())       -- request callback on interupt

-- send, waits for response, can time out
send :: Board -> BusM (Remote a) -> IO (Maybe a)
send (Board fn) cmds = do
        let (_,req_msg) = runWriter (cmdToRequest cmds)
        print req_msg
        rep_msg <- fn (Frame (BS.pack req_msg))
        print rep_msg
        case rep_msg of
          Nothing -> return Nothing
          Just (Frame rep) ->
                    case runStateT (cmdWithReply cmds) (BS.unpack rep) of
                        Just (Remote a,[]) -> return (Just a)
                        Just (a,_)  -> return Nothing -- fail "bad format, bad remote, etc"
                        Nothing     -> return Nothing


walk :: Monad m => (forall a . BusCmd a -> m a) -> BusCmd a -> m a
walk _ (Pure a) = return a
walk fn (Bind m k) = do
        a <- walk fn m
        walk fn (k a)
walk fn other = fn other

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

{-
reifyBusCmd :: [Word8] -> BusCmd (Remote [Word8])
reifyBusCmd (tag:h1:l1:val:rest) | tag == tagWrite =
        BusWrite addr val *> reifyBusCmd rest
  where
          addr = unseq16 [h1,l1]
reifyBusCmd (tag:h1:l1:rest) | tag == tagRead =
        liftA2 (:) (BusRead addr) (reifyBusCmd rest)
  where
          addr = unseq16 [h1,l1]
reifyBusCmd [] = pure []

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

busWrite addr val = singleton $ BusWrite addr val
busRead addr = singleton $ BusRead addr

test = do
        send brd $ do
                busWrite 0 1
                busWrite 2 3
                busRead 5
                busWrite 9 10
                done

brd = Board $ error ""
--boardFromBridgeFrame :: Bridge Frame -> IO Board

----------------------------------------------------------------

-- | connectBoard takes an initial timeout time,
--  and a Bridge Frame to the board, and returns
-- an abstact handle to the physical board.
connectBoard :: Float -> Bridge Frame -> IO Board
connectBoard timeoutTime bridge = do

        uniq :: MVar Word16 <- newEmptyMVar
        forkIO $ let loop n = do
                        putMVar uniq n
                        loop (succ n)
                 in loop 0

        callbacks :: Callback Word16 (Maybe Frame) <- liftM Callback $ newMVar Map.empty

        forkIO $ forever $ do
                Frame bs0 <- fromBridge bridge
                case do (a,bs1) <- BS.uncons bs0
                        (b,bs2) <- BS.uncons bs1
                        return (unseq16 [a,b],bs2) of
                  Just (uq,rest)
                           -- good packet, try respond
                        | BS.length rest > 0 -> callback callbacks uq (Just (Frame rest))
                  Nothing -> return () -- faulty packet?

        let send (Frame msg) = do
                uq <- takeMVar uniq

                rep :: MVar (Maybe Frame) <- newEmptyMVar

                -- register the callback
                register callbacks uq $ putMVar rep
                -- set up a delayed timeout response
                forkIO $ do
                        threadDelay (round (timeoutTime * 1000 * 1000))
                        callback callbacks uq Nothing

                toBridge bridge (Frame (BS.append (BS.pack (seq16 uq)) msg))

                -- And wait for the callback
                takeMVar rep


        return $ Board send

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

readBusFrame :: Frame -> Maybe BusFrame
readBusFrame (Frame bs0) =
        case do (a,bs1) <- BS.uncons bs0
                (b,bs2) <- BS.uncons bs1
                return (unseq16 [a,b],bs2) of
          Just (uq,rest) -> return (BusFrame uq (BS.unpack rest))
          Nothing -> fail "bad packet format"

showBusFrame :: BusFrame -> Frame
showBusFrame (BusFrame uq msg) = Frame (BS.append (BS.pack (seq16 uq)) (BS.pack msg))



interpBus :: (BusCmd [Word8] -> IO (Maybe [Word8])) -> IO (Bridge Frame)
interpBus cmd = do
        cmdChan <- newChan
        resChan <- newChan

        --
        forkIO $ forever $ do
                frame <- readChan cmdChan

                return ()

        return $ Bridge { toBridge = writeChan cmdChan
                        , fromBridge = readChan resChan
                        }