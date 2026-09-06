
module Main where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate


type Ball = (Point, Vector)  -- posição, velocidade
                             -- Point = (Float,Float)
                             -- Vector = (Float,Float)

-- desenhar uma lista de bolas
drawBalls :: [Ball] -> Picture
drawBalls balls = pictures (zipWith drawBall colors balls)
  where
    colors = cycle [red, green, blue, yellow]
    drawBall c ((x,y),(dx,dy)) 
        = translate x y (color c (circleSolid ballRadius))

-- avançar um passo de simulação 
-- por `dt' unidades de tempo
updateBalls :: ViewPort -> Float -> [Ball] -> [Ball]
updateBalls _ dt balls = map (updateBall dt) balls
  
updateBall :: Float -> Ball -> Ball  
updateBall dt ((x,y),(dx,dy)) = ((x',y'), (dx',dy'))
  where (x',dx') = clip x dx (maxX-ballRadius)
        (y',dy') = clip y dy (maxY-ballRadius)
        clip h dh max
          | h' > max = (max, -dh)
          | h' < -max= (-max, -dh)
          | otherwise = (h', dh)
          where h' = h + dt*dh
                
               
-- constantes
-- número de bolas
numBalls :: Int          
numBalls = 100
          
-- raio de cada bola (pixels)           
ballRadius :: Float
ballRadius = 10

-- limites duma "caixa" virtual
maxX, maxY :: Float
maxX = 300
maxY = 300
                     
-- escolher uma bola com posição e velocidade aleatória 
randomBall :: IO Ball
randomBall = do x <- randomRIO (-maxX,maxX) 
                y <- randomRIO (-maxY,maxY) 
                dx <- randomRIO (-200,200) 
                dy <- randomRIO (-200,200)
                return ((x,y),(dx,dy)) 

-- "frames per second"
fps :: Int
fps = 60
          
main = do balls <- sequence [randomBall | _ <- [1..numBalls]]
          simulate window black fps balls drawBalls updateBalls

window = InWindow "Balls"  (2*floor maxX,2*floor maxY) (0,0)
             