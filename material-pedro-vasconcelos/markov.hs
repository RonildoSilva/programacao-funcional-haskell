{- 
  Gerador de texto aleatório usando cadeias de Markov
  Pedro Vasconcelos, 2013
  Basedo no Chap.3 de "The Practice of Programming", 
  Brian Kerningham e Rob Pike, Addison-Wesley 
-}


import System.Random
import Data.Map (Map)
import qualified Data.Map as Map

-- tabela de prefixos
-- chaves são os prefixos e os valores são listas de sufixos
type Table = Map [String] [String]

main = do txt <- getContents
          let ws = words txt
          ws' <- markov 2 100 ws  -- 100 palavras, prefixos tamanho 2
          putStrLn (unwords ws')
          
-- gerador de texto
-- k: tamanho de prefixos, nwords: número de palavras a gerar
-- resultado em IO para gerar números aleatórios          
markov :: Int -> Int -> [String] -> IO [String]
markov k nwords ws = generate nwords prefix table
  where prefix = take k ws -- initial prefix
        table = build k ws -- prefixes table


-- partir uma lista em segmentos de tamanho fixo com sobreposições 
-- segments 3 [1..5] = [[1,2,3],[2,3,4],[3,4,5]]
segments :: Int -> [a] -> [[a]]
segments k ws = take (length ws - k + 1) $ 
                map (take k) $ 
                iterate tail ws


-- construir a tabela de prefixos apartir da lista de palavras
build :: Int -> [String] -> Table
build k ws = foldl addPrefix Map.empty $ segments (k+1) ws

-- acrescentar um prefixo e sufixo à tabela
addPrefix :: Table -> [String] -> Table
addPrefix table xs 
  = let prefix = init xs
        suffix = last xs
    in case Map.lookup prefix table of
      Nothing -> Map.insert prefix [suffix] table
      Just us -> Map.insert prefix (suffix:us) table 

-- versão mais sucinta e eficiente usando Map.insertWith
-- addPrefix table xs = Map.insertWith (++) (init xs) [last xs] table

-- gerar n palavras partindo de um prefixo e da tabela
generate :: Int -> [String] -> Table -> IO [String]        
generate n prefix table = do ws <- gen n prefix
                             return (prefix ++ ws)
  where
    gen 0 prefix  = return []
    gen n prefix  = case Map.lookup prefix table of
      Nothing -> return []
      Just us -> do w <- choose us
                    ws<- gen (n-1) (tail prefix ++ [w])
                    return (w:ws)

-- escolher aleatóriamente um elemento de uma lista
choose :: [a] -> IO a
choose xs = do i <- randomRIO (0,length xs-1)
               return (xs!!i)
               