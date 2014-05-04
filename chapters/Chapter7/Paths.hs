module Chapter7.Paths where

import Control.Monad
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS

paths :: [(Int,Int)] -> Int -> Int -> [[Int]]
paths edges start end =
  let e_paths = do (e_start, e_end) <- edges
                   guard $ e_start == start
                   subpath <- paths edges e_end end
                   return $ start:subpath
  in if start == end
     then return [end] `mplus` e_paths
     else e_paths

graph1 :: [(Int, Int)]
graph1 = [(2013,501),(2013,1004),(501,2558),(1004,2558)]

graph2 :: [(Int, Int)]
graph2 = [(2013,501),(501,2558),(501,1004),(1004,501),(2013,2558)]

pathsL :: [(Int,Int)] -> Int -> Int -> Logic [Int]
pathsL edges start end =
  let e_paths = do (e_start, e_end) <- choices edges
                   guard $ e_start == start
                   subpath <- pathsL edges e_end end
                   return $ start:subpath
  in if start == end
     then return [end] `mplus` e_paths
     else e_paths

choices :: [a] -> Logic a
choices = msum . map return

pathsWriter :: [(Int,Int)] -> Int -> Int -> [[Int]]
pathsWriter edges start end = map execWriter (pathsWriter' edges start end)

pathsWriter' :: [(Int,Int)] -> Int -> Int -> [Writer [Int] ()]
pathsWriter' edges start end =
  let e_paths = do (e_start, e_end) <- edges
                   guard $ e_start == start
                   subpath <- pathsWriter' edges e_end end
                   return $ do tell [start]
                               subpath
  in if start == end then tell [start] : e_paths else e_paths

pathsWriterT' :: [(Int,Int)] -> Int -> Int -> WriterT [Int] [] ()
pathsWriterT' edges start end =
  let e_paths = do (e_start,e_end) <- lift edges
                   guard $ e_start == start
                   tell [start]
                   pathsWriterT' edges e_end end
  in if start == end then tell [start] `mplus` e_paths else e_paths

pathsWriterT :: [(Int,Int)] -> Int -> Int -> [[Int]]
pathsWriterT edges start end = execWriterT (pathsWriterT' edges start end)

-- exercise 7-6. Paths on Monad Transformers
pathsMTL' :: Int -> Int -> ReaderT [(Int,Int)] (WriterT [Int] []) ()
-- pathsMTL' :: Int -> Int -> RWST [(Int,Int)] [Int] () [] ()
pathsMTL' start end =
  let e_paths = do edges <- ask
                   (e_start, e_end) <- lift (lift edges)
                   guard $ e_start == start
                   tell [start]
                   pathsMTL' e_end end
  in if start == end then tell [start] `mplus` e_paths else e_paths

pathsMTL :: [(Int,Int)] -> Int -> Int -> [[Int]]
pathsMTL edges start end =
  execWriterT (runReaderT (pathsMTL' start end) edges)
