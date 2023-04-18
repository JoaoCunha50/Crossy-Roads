module Tarefa1_2022li1g014_Spec where

import LI12223
import Tarefa1_2022li1g014 
import Test.HUnit



testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test ["Teste 1" ~: mapaValido (Mapa 2 [(Rio 2 ,[Nenhum,Tronco]),(Rio 2 ,[Nenhum,Tronco]),(Estrada 2 , [Carro,Carro])]) ~=? False ,
                                              "Teste 2" ~: mapaValido (Mapa 6 [(Rio 2 ,[Nenhum,Tronco,Nenhum,Tronco,Nenhum,Nenhum]),(Rio (-2) ,[Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]),(Estrada 2 , [Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum])]) ~=? True ,
                                              "Teste 3" ~: mapaValido (Mapa 7 [(Rio 1 , [Tronco,Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco]),(Estrada 1 ,[Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Relva , [Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum])]) ~=? False , 
                                              "Teste 4" ~: mapaValido (Mapa 7 [(Estrada 1 , [Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 3 , [Carro,Carro,Carro,Nenhum,Nenhum,Carro]),(Relva , [Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-2),[Nenhum,Tronco,Tronco,Nenhum,Tronco,Nenhum,Nenhum])]) ~=? False ,
                                              "Teste 5" ~: mapaValido (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 2 , [Carro,Nenhum,Carro]),(Estrada 1 , [Nenhum,Carro,Nenhum]),(Estrada 3 , [Nenhum,Nenhum,Carro]),(Estrada 1 ,[Nenhum,Nenhum,Carro]),(Estrada 3 , [Nenhum,Carro,Carro]),(Estrada 1,[Nenhum,Nenhum,Carro]),(Relva , [Nenhum,Arvore,Nenhum])]) ~=? False ,
                                              "Teste 6" ~: mapaValido (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 2 , [Carro,Nenhum,Nenhum]),(Estrada 1 , [Nenhum,Nenhum,Nenhum]),(Estrada 3 , [Carro,Nenhum,Carro]),(Estrada 1 ,[Nenhum,Nenhum,Carro]),(Estrada 3 , [Nenhum,Carro,Carro]),(Rio (-2),[Nenhum,Nenhum,Carro]),(Relva , [Nenhum,Arvore,Nenhum])]) ~=? False ,
                                              "Teste 7" ~: mapaValido (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Rio (-2) , [Tronco,Nenhum,Tronco]),(Estrada 1 , [Nenhum,Carro,Nenhum]),(Estrada 3 , [Nenhum,Nenhum,Carro]),(Relva  ,[Nenhum,Nenhum,Arvore]),(Estrada 3 , [Nenhum,Carro,Nenhum]),(Rio (-2),[Nenhum,Nenhum,Tronco]),(Relva , [Nenhum,Arvore,Arvore])]) ~=? True ,
                                              "Teste 8" ~: mapaValido (Mapa 2 [(Relva , [Carro,Nenhum]),(Rio 2 , [Nenhum,Tronco]),(Rio (-2) ,[Tronco,Nenhum])]) ~=? False ,
                                              "Teste 9" ~: mapaValido (Mapa 3 [(Rio 2,[Nenhum,Nenhum,Nenhum]),(Rio (-2) , [Nenhum,Nenhum,Nenhum]),(Estrada 1 , [Nenhum,Carro,Nenhum]),(Estrada 3 , [Nenhum,Nenhum,Carro]),(Relva  ,[Nenhum,Nenhum,Arvore]),(Estrada 3 , [Nenhum,Carro,Nenhum]),(Rio (-2),[Nenhum,Nenhum,Tronco]),(Relva , [Nenhum,Arvore,Arvore])]) ~=? False ,
                                              "Teste 10" ~: mapaValido (Mapa 1 [(Estrada 2 , [Nenhum,Nenhum]),(Relva ,[Nenhum])]) ~=? False ,
                                              "Teste 11" ~: mapaValido (Mapa 6 [(Rio 2 ,[Nenhum,Tronco,Nenhum,Tronco,Nenhum,Nenhum]),(Rio (-2) ,[Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum]),(Estrada 2 , [Carro,Carro,Carro,Carro,Carro,Carro])]) ~=? False]

