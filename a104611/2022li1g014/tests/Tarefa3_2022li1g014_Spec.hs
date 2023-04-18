module Tarefa3_2022li1g014_Spec where

import LI12223
import Tarefa3_2022li1g014
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste 2" ~: animaJogo (Jogo (Jogador (4,1)) (Mapa 5 [(Rio (-2),[Nenhum, Tronco, Nenhum, Nenhum, Tronco]),(Estrada (-1),[Carro, Nenhum, Nenhum, Carro, Nenhum]),(Rio 1,[Nenhum, Tronco, Nenhum, Nenhum, Tronco])])) (Move Cima)    ~=?  Jogo (Jogador (4,1)) (Mapa 5 [(Rio (-2),[Nenhum,Nenhum,Tronco,Nenhum,Tronco]),(Estrada (-1),[Nenhum,Nenhum,Carro,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum])])]
                                             