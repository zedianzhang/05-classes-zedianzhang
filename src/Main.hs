import qualified Language.Nano.Types  as Nano
import qualified Language.Nano.Eval   as Nano
import           Language.Nano.Repl
import           Text.Printf
import           GHC.IO.Encoding
import Language.Nano.Types (Env)

main :: IO ()                             
main = do
  setLocaleEncoding utf8
  putStrLn welcome
  loop 0  Nano.prelude


loop :: Int ->  Env -> IO ()
loop n env = do 
          putStrFlush (printf "λ [%d] " n)
          cmd <- getLine 
          (case strCmd cmd of
            CEval s -> -- putStrLn  (definitions env)
                       doEval env s
            CRun s ->  -- putStrLn s
                       doRun s 
            CLoad s -> -- putStrLn s
                      (do 
                        des  <- doLoad s
                        putStrFlush "definitions:"
                        putStrLn  (definitions des)
                        loop (n+1) des)
            CQuit -> doQuit 
            CUnknown -> doUnknown)
          loop (n + 1) env


--------------------------------------------------------------------------------
-- | Some useful functions 
--------------------------------------------------------------------------------
-- putStr   :: String -> IO ()
-- hFlush   :: 
-- putStrLn :: String -> IO ()
-- getLine  :: IO String 

