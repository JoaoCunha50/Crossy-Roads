module Tarefa5_2022li1g014_Spec where

import LI12223
import Tarefa2_2022li1g014
import Tarefa5_2022li1g014
import Test.HUnit


testsT5::Test
testsT5 = TestLabel "Testes Tarefa 5" $ test ["Teste1"~:deslizaJogo 11 (Jogo (Jogador (3,4)) (Mapa 2 [(Relva , [Arvore,Nenhum]),(Estrada 2 , [Carro,Nenhum])]))~=? Jogo (Jogador (3,5)) (Mapa 2 [(Estrada 0,[Nenhum,Carro]),(Relva,[Arvore,Nenhum])]),
                                              "Teste2"~:deslizaJogo 11 (Jogo (Jogador (3,4)) (Mapa 2 [(Relva , [Nenhum,Nenhum]),(Estrada 2 , [Carro,Nenhum]),(Rio (-1) , [Tronco,Tronco])]))~=? Jogo (Jogador (3,5)) (Mapa 2 [(Estrada 0,[Nenhum,Carro]),(Relva,[Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum])]),
                                              "Teste3"~:deslizaJogo 55 (Jogo (Jogador (3,4)) (Mapa 2 [(Relva , [Nenhum,Nenhum]),(Estrada 2 , [Carro,Nenhum]),(Rio (-1) , [Tronco,Tronco])]))~=? Jogo (Jogador (3,5)) (Mapa 2 [(Rio 0,[Nenhum,Tronco]),(Relva,[Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum])]),
                                              "Teste4"~:deslizaJogo 99 (Jogo (Jogador (3,4)) (Mapa 2 [(Relva , [Arvore,Nenhum]),(Estrada 2 , [Carro,Nenhum])]))~=? Jogo (Jogador (3,5)) (Mapa 2 [(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum])]),
                                              "Teste5"~:deslizaJogo 77 (Jogo (Jogador (6,7))(Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),(Estrada 2 , [Nenhum,Carro,Nenhum]),(Rio (-1) , [Tronco,Tronco,Tronco])]))~=? Jogo (Jogador (6,8)) (Mapa 3 [(Estrada 0,[Nenhum,Carro,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro,Nenhum])])]

