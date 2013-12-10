import Data.IORef

-- readIt :: IO (IORef String)
readIt = do 
  line <- getLine
  putStrLn line
  let str = "hei" ++ " hade"
  putStrLn str
  return line
