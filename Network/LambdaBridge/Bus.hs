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


type BusM a = Program BusCmd a

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

busWrite addr val = singleton $ BusWrite addr val
busRead addr      = singleton $ BusRead addr

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


-- not sure about remote here
interpBus :: (BusM (Remote [Word8]) -> IO (Maybe [Word8])) -> IO (Bridge Frame)
interpBus cmd = do
        cmdChan <- newChan
        resChan <- newChan

        --
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


--memory :: IOUArray Word16 Word8 -> BusM (Remote [Word8]) -> IO (Maybe [Word8])

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


