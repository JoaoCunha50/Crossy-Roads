module Tarefa4_2022li1g014_Spec where

import LI12223
import Tarefa4_2022li1g014
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Teste 1" ~: jogoTerminou (Jogo (Jogador (2,0)) (Mapa 5 [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),(Estrada 2,[Carro,Nenhum,Nenhum,Carro,Nenhum]),(Rio 1,[Nenhum,Tronco,Nenhum,Tronco,Nenhum])]))  ~=? False,
                                              "Teste 2" ~: jogoTerminou (Jogo (Jogador (0,1)) (Mapa 5 [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),(Estrada 2,[Carro,Nenhum,Nenhum,Carro,Nenhum]),(Rio 1,[Nenhum,Tronco,Nenhum,Tronco,Nenhum])]))  ~=? True, 
                                              "Teste 3" ~: jogoTerminou (Jogo (Jogador (3,0)) (Mapa 5 [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),(Estrada 2,[Carro,Nenhum,Nenhum,Carro,Nenhum]),(Rio 1,[Nenhum,Tronco,Nenhum,Tronco,Nenhum])]))  ~=? True ,
                                              "Teste 4" ~: jogoTerminou (Jogo (Jogador (1,1)) (Mapa 5 [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),(Estrada 2,[Carro,Nenhum,Nenhum,Carro,Nenhum]),(Rio 1,[Nenhum,Tronco,Nenhum,Tronco,Nenhum])]))  ~=? False,
                                              "Teste 5" ~: jogoTerminou (Jogo (Jogador (5,2)) (Mapa 5 [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),(Estrada 2,[Carro,Nenhum,Nenhum,Carro,Nenhum]),(Rio 1,[Tronco,Nenhum,Nenhum,Tronco,Nenhum])]))  ~=? True ]