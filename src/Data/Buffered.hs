{-# LANGUAGE RecordWildCards #-}

module Data.Buffered where

import Data.ByteString (ByteString)
import Data.Word
import Foreign.Ptr
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Comonad.Cofree

-- input stream of bytes
-- buffered
-- -> input stream of chars
-- -> allocate maximum line length, stream input into line (i.e. buffer)
-- - can dispatch line parser (copy to lengthed-line)
-- - or just parse:
-- -> fold over line into parsed-line (allocated to maximum length), which is a mutable vector
-- - copy parsed line to mutable exact-length vector
-- when the end of input is reached, we have a collection of parsed-lines, a matched-stack, and some other state.
-- if the matched-stack is not empty, we return an "unmatched" error
-- otherwise, we have successfully parsed the input.

-- stream into buffered char-streamer
--  (i.e. put chunks into buffer on-demand as outputting chars)


-- fdReadBuf Source#

-- :: Fd
-- -> Ptr Word8
-- Memory in which to put the data

-- -> ByteCount
-- Maximum number of bytes to read

-- -> IO ByteCount
-- Number of bytes read (zero for EOF)

-- ok, so I didn't know that's how `decode` worked..
--   now, I want to modify decode to be completely strict
--   it should accept a single byte, update UTF8 parsing state, and continue
--   it may or may not emit a Char at every byte.
--   this method can effectively be zipped with the bytes and we cut off the end
--   I'm not sure, but that may allow decoding to be branchless..

newtype CofreeF f a = CofreeF { runCofreeF :: f (Cofree f a) }

data Fd

-- | `undefined`
--
-- @
--  allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
-- @
allocaBytes :: Int -> (Ptr Word8 -> IO b) -> IO b
allocaBytes = undefined

-- | `undefined`
unsafePackCStringFinalizer :: Ptr Word8 -> Int -> IO () -> IO ByteString
unsafePackCStringFinalizer = undefined

-- | Use `unsafePackCStringFinalizer` to wrap the result of `allocaBytes`
allocaByteString :: Int -> (ByteString -> IO a) -> IO a
allocaByteString len f = allocaBytes len $ \ptr -> unsafePackCStringFinalizer ptr len (return ()) >>= f

data BufferState m = BufferState
  { buffer :: !ByteString
  , bufferPtr :: !(Ptr Word8)
  , fillBuffer :: m ByteString
  }


-- | `undefined`
initialBufferState :: MonadIO m => Int -> Fd -> m (BufferState m)
initialBufferState = undefined
-- initialBufferState bufSize fd = do
--   bufPtr <- malloc bufSize
--   buffer' <- liftIO $ unsafePackCStringLen (ptr, 0)
--   return $ BufferState
--     { buffer = buffer'
--     , bufferPtr = bufferPtr'
--     , fillBuffer = liftIO $ fdReadBuf fd bufferPtr bufSize
--     }

-- | `undefined`
refillBuffer :: StateT (BufferState m) m ()
refillBuffer = undefined
-- refillBuffer = StateT $ \bs@BufferState{..} -> do
--   refillLen <- fillBuffer
--   buffer' <- liftIO $ unsafePackCStringLen (ptr, refillLen)
--   return $ bs { buffer = buffer' }

-- | `undefined`
buildBuffered :: StateT ByteString m a -> Int -> Fd -> CofreeF (StateT (BufferState m) m) Char
buildBuffered = undefined
-- buildBuffered parser bufSize fd = do
--   bufferPtr <- malloc bufSize
--   bufferByteString <- liftIO $ unsafePackCStringFinalizer bufferPtr
--   let bufState = BufferState
--         { buffer = bufferByteString
--         , fillBuffer = liftIO $ fdReadBuf fd bufferPtr bufSize
--         }
--
--   coiter $ do
--     case decode buffer of
--       Nothing -> do
--         readBytes <- fillBuffer
--         if readBytes == 0
--            then return Nothing
--            else undefined
--       Just (c, sz) -> do
--         undefined c sz
--         -- buffer = unsafeDrop sz buffer
--        -- then _

-- | `undefined`
decode :: ByteString -> Maybe (Char, Int)
decode = undefined

