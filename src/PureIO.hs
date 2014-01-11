{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Pure IO monad, intended for educational use.

module PureIO
  (-- * The IO monad and its machinery
   runIO
  ,IO
  ,Input(..)
  ,Output(..)
  ,Interrupt(..)
  -- * Library of actions
  ,IOException(..)
  ,putStrLn
  ,putStr
  ,getLine
  ,readLn
  ,print
  ,readIO
  ,throw
  ,catch
  )
  where

import Control.Applicative
import Control.Arrow
import Control.Monad.Error
import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Prelude hiding (IO,putStr,putStrLn,getLine,readLn,print,readIO,catch)
import Safe

--------------------------------------------------------------------------------
-- IO monad and machinery

-- | Run the IO monad. This should be called in succession. Depending
-- on the type of interrupt, this function should be re-run with the
-- same action but with additional input.
runIO :: Input -> IO a -> (Either Interrupt a, Output)
runIO input m =
  second snd
         (runState (runErrorT (unIO m)) (input,mempty))

-- | An IO exception.
data IOException = UserError String
  deriving (Show,Read)

-- | User input.
data Input = Input
  { inputStdin :: ![String]
  } deriving (Show)

-- | IO monad output.
data Output = Output
  { outputStdout :: ![String]
  } deriving (Show,Read)

instance Monoid Output where
  mempty = Output []
  (Output a) `mappend` (Output b) = Output (a <> b)

-- | Something that interrupts the flow of the IO monad.
data Interrupt
  = InterruptStdin -- ^ When you receive this interrupt, you should
                   -- get some standard input from somewhere and then
                   -- provide it in the 'Input' value next time you
                   -- call 'runIO'.
  | InterruptException !IOException -- ^ When you receive this
                                    -- interrupt, you should consider
                                    -- the computation as ended.
  deriving (Show,Read)
instance Error Interrupt

-- | A pure IO monad.
newtype IO a = IO
  { unIO :: ErrorT Interrupt (State (Input,Output)) a
  }
  -- We purposely don't derive MonadState and MonadError, while it
  -- would aid programming minutely, such instances are internals that
  -- we don't want to export.
  deriving (Monad,Functor,Applicative)

-- | Interrupt the IO monad. This stops the IO monad computation,
-- allowing for any resumption later.
interrupt :: Interrupt -> IO a
interrupt = IO . throwError

--------------------------------------------------------------------------------
-- Library

-- | The same as 'putStr', but adds a newline character.
putStrLn :: String -> IO ()
putStrLn = putStr . (++ "\n")

-- | Write a string to the standard output device.
putStr :: String -> IO ()
putStr new = IO (modify (\(i,o) -> (i,o <> Output [new])))

-- | Read a line from standard input.
getLine :: IO String
getLine = do
  (Input is,_) <- IO get
  case is of
    [] -> interrupt InterruptStdin
    (i:is') -> do IO (modify (first (const (Input is'))))
                  return i

-- | The 'readIO' function is similar to 'read' except that it signals
-- parse failure to the 'IO' monad instead of terminating the program.
readIO :: Read a => String -> IO a
readIO s =
  case readMay s of
    Nothing -> throw (UserError "readIO: no parse")
    Just r -> return r

-- | The readLn function combines 'getLine' and 'readIO'.
readLn :: Read a => IO a
readLn = getLine >>= readIO

-- | The 'print' function outputs a value of any printable type to the
-- standard output device.
-- Printable types are those that are instances of class 'Show'; 'print'
-- converts values to strings for output using the 'show' operation and
-- adds a newline.
--
-- For example, a program to print the first 20 integers and their
-- powers of 2 could be written as:
--
-- > main = print ([(n, 2^n) | n <- [0..19]])
print :: Show a => a -> IO ()
print = putStrLn . show

-- | Throw an IO exception.
throw :: IOException -> IO a
throw = interrupt . InterruptException

-- | Catch an IO exception.
catch :: IO a -> (IOException -> IO a) -> IO a
catch (IO m) f = IO (catchError m handler)
  where handler i =
          case i of
            InterruptException e ->
              let (IO m') = f e
              in m'
            _ -> throwError i
