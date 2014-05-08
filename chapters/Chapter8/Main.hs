import System.Environment
import Chapter8.FindFactors

main :: IO ()
main = do
  (xStr:yStr:_) <- getArgs
  let x        = read xStr
      y        = read yStr
      (fx, fy) = findTwoFactors x y
  putStrLn $ show fx
  putStrLn $ show fy
