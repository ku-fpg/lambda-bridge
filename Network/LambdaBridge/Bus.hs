{-# LANGUAGE KindSignatures, GADTs, FlexibleInstances, RankNTypes, ScopedTypeVariables, DataKinds #-}
module Network.LambdaBridge.Bus where

import Network.LambdaBridge.Bridge
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

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Control.Monad.Operational

debug = debugM "lambda-bridge.bus"

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
        fmap f Symbol     = Symbol
        fmap f (Remote a) = Remote (f a)

instance Applicative Remote where
        pure a                    = Remote a
        Symbol <*> _              = Symbol
        _ <*> Symbol              = Symbol
        (Remote f) <*> (Remote a) = Remote (f a)

instance Alternative Remote where
        empty = Symbol
        (Remote a) <|> _ = Remote a
        _ <|> (Remote a) = Remote a
        _ <|> _          = Symbol

runRemote :: Remote a -> Maybe a
runRemote (Remote a) = Just a
runRemote Symbol     = Nothing

optRemote :: Maybe a -> Remote a
optRemote Nothing = Symbol
optRemote (Just a) = Remote a

data BusCmd :: * -> * where
        BusWrite :: Word16 -> Word8     -> BusCmd ()
        BusRead  :: Word16              -> BusCmd (Remote Word8)

instance Show (BusCmd a) where
        show (BusWrite a d) = "BusWrite " ++ show a ++ " " ++ show d
        show (BusRead a)    = "BusRead " ++ show a

type BusM a = Program BusCmd a

done :: BusM (Remote ())
done = return (pure ())

busWrite addr val = singleton $ BusWrite addr val
busRead addr      = singleton $ BusRead addr

instance Show (Program BusCmd a) where
  show = eval . view
    where eval :: ProgramView BusCmd a -> String
          eval (Return _)  = ""
          eval (op@(BusWrite {}) :>>= i) = show op ++ ";" ++ show (i ())
          eval (op@(BusRead {}) :>>= i) = show op ++ ";" ++ show (i Symbol)


{-n
interp :: forall a c m . (Monad m) => (forall a . c a -> m a) -> Program c a -> m a
interp f = eval . view
  where eval :: forall a . ProgramView c a -> m a
        eval (Return a) = return a
        eval (m :>>= i) = do
                r <- f m
                interp f (i r)
-}

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
                uq <- takeMVar uniq

                debug $ show uq ++ ": send {" ++ show cmd ++ "}"

                let (_,req_msg) = runWriter (cmdToRequest cmd)
                debug $ show uq ++ ": bytes to board " ++ show req_msg

                rep :: MVar (Maybe [Word8]) <- newEmptyMVar

                -- register the callback
                register callbacks uq $ putMVar rep
                -- set up a delayed timeout response
                forkIO $ do
                        threadDelay (round (timeoutTime * 1000 * 1000))
                        debug $ show uq ++ " giving up on packet"
                        callback callbacks uq Nothing

                toBridge bridge $ showBusFrame $ BusFrame uq req_msg

                debug $ show uq ++ ": sent"

                -- And wait for the callback
                rep_msg <- takeMVar rep

                debug $ show uq ++ ": recv'd " ++ show rep_msg


--                case rep_msg of { Nothing -> putStrLn "*" ; _ -> return () }

                case rep_msg of
                  Nothing -> return Nothing
                  Just rep_bs -> case runStateT (cmdWithReply cmd) rep_bs of
                                    Just (Remote a,[]) -> do
                                         debug $ show uq ++ ": returning success"
                                         return (Just a)
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


-- This is used for testing the serialization of the Board commands.
--
-- TODO: consider calling this use of Board/Bus VirtualBus, or simulator.
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

{-

        BusM (Remote a) -> IO (Maybe a)

                f3 cmd = do r <- f1 cmd
                            case r of
                              Just a -> return (Just a)
                              Nothing ->

        let cmdFn :: BusCmd a -> IO a
            cmdFn (BusWrite 0 val) = wt_tag rpc val
            cmdFn (BusWrite 1 val) = push rpc val
            cmdFn (BusWrite _ val) = return ()
            cmdFn (BusRead 0)      = fmap pure (rd_tag rpc)
            cmdFn (BusRead 1)      = fmap pure (rd_tag rpc)
            cmdFn (BusRead _)      = return Symbol

-}

-----------------------------------------------------------------------------

data WritePort = WritePort (MVar ByteString)

writePort :: WritePort -> ByteString -> IO ()
writePort (WritePort v) = putMVar v

busWritePort :: Board -> Word16 -> IO WritePort
busWritePort brd addr = do
        var <- newEmptyMVar

        let loop :: [Word8] -> Word8 -> IO ()
            loop [] t = do
                bs <- takeMVar var
                loop (BS.unpack bs) t
            loop (w:ws) t = do
                o <- send brd $ do
                        busWrite addr t
                        busWrite (addr + 1) w
                        busRead addr
--                print (w,t,o)
                case o of
                  Just t' | t /= t' -> loop ws     t'    -- accepted
                  _                 -> loop (w:ws) t     -- rejected

        forkIO $ loop [] 0

        return $ WritePort $ var

data ReadPort = ReadPort (MVar ByteString)

readPort :: ReadPort -> IO ByteString
readPort (ReadPort v) = takeMVar v

busReadPort :: Board -> Word16 -> IO ReadPort
busReadPort brd addr = do
        var <- newEmptyMVar

        let loop t = do
                o <- send brd $ do
                        busWrite addr t
                        w <- busRead (addr + 1)
                        t' <- busRead addr
                        return ((,) <$> w <*> t')
--                print (o,t)
                case o of
                  Just (w,t') | t /= t' -> do
--                        print (w,t')
                        putMVar var (BS.pack [w])
                        loop t'
                  _ -> loop t

        forkIO $ loop 0

        return $ ReadPort $ var