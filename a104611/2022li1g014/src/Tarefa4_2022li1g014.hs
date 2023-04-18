{- |
Module      : Tarefa4_2022li1g014
Description : Determinar se o jogo terminou
Copyright   : Gonçalo da Silva Alves <a104079@alunos.uminho.pt>
              João Manuel Machado da Cunha <a104611@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23 .
-}

module Tarefa4_2022li1g014 where

import LI12223

atropelamento :: Jogador -> (Terreno,[Obstaculo]) -> Bool
atropelamento (Jogador(x,y)) (Estrada v,obs) | obs !! x  == Carro = True
                                | carroemmovimento (x,y) v posCar == True = True                               
                                | otherwise = False 
                                where posCar = acomulador Carro obs
atropelamento (Jogador(x,y)) (Rio v,obs) = False
atropelamento (Jogador(x,y)) (Relva,obs) = False

carroemmovimento :: (Coordenadas) -> Int -> [Int] -> Bool
carroemmovimento (x,y) v [] = False
carroemmovimento (x,y) v (h:t) = if (v>0 && h<v && (h+v)>x) then True else 
                                                                if (v<0 && h>x && (h+v)<x) then True else carroemmovimento (x,y) v t 

acomulador :: Eq a => a -> [a] -> [Int]
acomulador x xs = acomuladoraux x xs 0 []
    where 
        acomuladoraux x (y:ys) index indices
                                | x == y = acomuladoraux x ys (index+1) (index:indices)
                                | otherwise = acomuladoraux x ys (index+1) indices
        acomuladoraux _ [] _ indices = indices 

drowned :: Coordenadas -> (Terreno,[Obstaculo]) -> Bool
drowned (x,y) (Rio _, obs) = if obs !! x == Nenhum then True else False
drowned (x,y) (Estrada _,_) = False
drowned (x,y) (Relva,_) = False 

isGameOver :: Mapa -> Jogador -> Bool
isGameOver (Mapa larg terr) (Jogador(x,y)) = drowned (x,y) obs || atropelamento (Jogador(x,y)) obs 
            where obs = terr !! y

foradomapa :: Jogador -> Mapa -> Bool
foradomapa (Jogador (x,y)) (Mapa l t) | x<0 = True
                                            | x >= l = True     
                                            | y<0 = True
                                            | y > ((length t)-3) = True
                                            | otherwise = False

jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l t)) | foradomapa (Jogador(x,y)) (Mapa l t)==True = True
                                                     | otherwise = isGameOver (Mapa l t) (Jogador (x,y+1))