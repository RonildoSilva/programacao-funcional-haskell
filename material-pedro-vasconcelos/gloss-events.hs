{- 
  Demonstração de eventos em Gloss 
  Pedro Vasconcelos, 2014
-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main = play window white fps "" draw react update

window = InWindow "Gloss events" (800,600) (0,0)

-- atualizações por segundo
-- não é importante porque vamos atualizar apenas por eventos
fps :: Int
fps = 1    

-- função para desenhar
draw :: String -> Picture
draw xs = translate (-400) 0 $ scale 0.25 0.25 $ text xs

-- função para reagir a eventos
react :: Event -> String -> String
react ev _ = show ev

-- função para atualizar por tempo (identidade)
update :: Float -> String -> String
update dt xs = xs
