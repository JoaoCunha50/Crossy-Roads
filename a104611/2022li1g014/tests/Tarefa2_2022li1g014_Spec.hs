module Tarefa2_2022li1g014_Spec where

import LI12223
import Tarefa2_2022li1g014
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test ["Teste 1" ~: estendeMapa (Mapa 7 [(Rio 2 , [Tronco,Nenhum ,Nenhum ,Tronco,Nenhum,Tronco,Nenhum ]),(Rio (-2) , [Nenhum ,Tronco ,Nenhum,Nenhum,Nenhum,Nenhum,Tronco ]),(Rio 2 , [Nenhum ,Nenhum ,Nenhum, Nenhum,Nenhum,Tronco,Tronco]),(Rio (-1),[Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco])]) 13 ~=? Mapa 7 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Rio 2,[Tronco,Nenhum,Nenhum,Tronco,Nenhum,Tronco,Nenhum]),(Rio (-2),[Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Rio 2,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Tronco]),(Rio (-1),[Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco])] , 
                                              "Teste 2" ~: estendeMapa (Mapa 7 [(Rio 2 , [Tronco,Nenhum ,Nenhum,Nenhum,Nenhum,Tronco,Nenhum]),(Estrada 1 , [Nenhum ,Carro ,Nenhum,Carro,Nenhum,Nenhum,Carro ]),(Relva , [Nenhum ,Nenhum ,Nenhum,Arvore,Nenhum,Arvore,Arvore])]) 12 ~=? Mapa 7 [(Relva,[Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum]),(Rio 2,[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Nenhum]),(Estrada 1,[Nenhum,Carro,Nenhum,Carro,Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore])] ,
                                              "Teste 3" ~: estendeMapa (Mapa 4 [(Estrada 2 ,[Nenhum,Carro,Nenhum,Carro]),(Estrada 1 ,[Nenhum,Nenhum,Nenhum,Carro]),(Estrada 3 , [Carro ,Nenhum ,Carro,Nenhum]),(Estrada 1 , [Nenhum,Nenhum,Carro,Nenhum]),(Estrada 2 , [Carro,Nenhum,Nenhum,Nenhum])]) 20 ~=? Mapa 4 [(Rio 0,[Nenhum,Tronco,Tronco,Nenhum]),(Estrada 2,[Nenhum,Carro,Nenhum,Carro]),(Estrada 1,[Nenhum,Nenhum,Nenhum,Carro]),(Estrada 3,[Carro,Nenhum,Carro,Nenhum]),(Estrada 1,[Nenhum,Nenhum,Carro,Nenhum]),(Estrada 2,[Carro,Nenhum,Nenhum,Nenhum])] ,
                                              "Teste 4" ~: estendeMapa (Mapa 3 [(Rio 2 , [Tronco ,Nenhum,Nenhum]),(Relva , [Nenhum,Arvore,Nenhum]),(Relva , [Nenhum,Nenhum,Arvore]),(Relva ,[Nenhum,Nenhum,Nenhum]),(Relva , [Arvore,Nenhum,Nenhum]),(Relva , [Arvore,Arvore,Nenhum])]) 99 ~=? Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Rio 2,[Tronco,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore]),(Relva,[Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum])] , 
                                              "Teste 5" ~: estendeMapa (Mapa 7 [(Rio 2 , [Tronco,Nenhum ,Nenhum,Nenhum,Nenhum,Tronco,Nenhum]),(Estrada 1 , [Nenhum ,Carro ,Nenhum,Carro,Nenhum,Nenhum,Carro ]),(Relva , [Nenhum ,Nenhum ,Nenhum,Arvore,Nenhum,Arvore,Arvore])]) 14 ~=? Mapa 7 [(Estrada 0,[Nenhum,Carro,Carro,Nenhum,Carro,Carro,Nenhum]),(Rio 2,[Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Nenhum]),(Estrada 1,[Nenhum,Carro,Nenhum,Carro,Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore])] ,
                                              "Teste 6" ~: estendeMapa (Mapa 5 [(Rio 2 , [Tronco,Nenhum ,Nenhum ,Tronco,Nenhum]),(Estrada 2 ,[Nenhum,Carro,Nenhum,Carro,Nenhum]),(Relva , [Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-2) , [Nenhum ,Tronco ,Nenhum,Nenhum,Nenhum])]) 36 ~?= Mapa 5 [(Relva,[Nenhum,Arvore,Arvore,Arvore,Arvore]),(Rio 2,[Tronco,Nenhum,Nenhum,Tronco,Nenhum]),(Estrada 2,[Nenhum,Carro,Nenhum,Carro,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-2),[Nenhum,Tronco,Nenhum,Nenhum,Nenhum])] , 
                                              "Teste 7" ~: estendeMapa (Mapa 4 [(Estrada 1 , [Carro,Nenhum,Carro,Nenhum]),(Rio 2 , [Tronco ,Nenhum,Nenhum,Tronco]),(Relva , [Nenhum,Arvore,Nenhum,Arvore]),(Relva , [Nenhum,Nenhum,Arvore,Nenhum])]) 69 ~=? Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Arvore]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum]),(Rio 2,[Tronco,Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum,Arvore]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum])]]


                                       