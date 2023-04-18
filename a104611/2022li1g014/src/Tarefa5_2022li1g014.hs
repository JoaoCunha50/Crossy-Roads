{- |
Module      : Tarefa5_2022li1g014
Description : Geração contínua de um mapa
Copyright   : Gonçalo da Silva Alves <a104079@alunos.uminho.pt>
              João Manuel Machado da Cunha <a104611@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.

Para realizar a seguinte tarefa criámos apenas uma função auxiliar .
-}

module Tarefa5_2022li1g014 where 

import LI12223
import Tarefa2_2022li1g014

{- | A função deslizaJogo faz com que novas linhas do mapa sejam geradas através da função estendeMapa realizada e documentada na Tarefa 2.

Esta função pode ser definida por :

@

deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo a (Jogo (Jogador (x,y)) (Mapa c l)) = (Jogo (Jogador (x,y+1)) (aux (estendeMapa (Mapa c l) a)))
                                                                   where aux (Mapa c ob)= Mapa c (init ob)
@

==Exemplos de utilização :

>>>deslizaJogo 11 (Jogo (Jogador (3,4)) (Mapa 2 [(Relva , [Arvore,Nenhum]),(Estrada 2 , [Carro,Nenhum])]))
Jogo (Jogador (3,5)) (Mapa 2 [(Estrada 0,[Nenhum,Carro]),(Relva,[Arvore,Nenhum])])

>>>deslizaJogo 11 (Jogo (Jogador (3,4)) (Mapa 2 [(Relva , [Nenhum,Nenhum]),(Estrada 2 , [Carro,Nenhum]),(Rio (-1) , [Tronco,Tronco])]))
Jogo (Jogador (3,5)) (Mapa 2 [(Estrada 0,[Nenhum,Carro]),(Relva,[Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum])])
-}

deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo a (Jogo (Jogador (x,y)) (Mapa c l)) = (Jogo (Jogador (x,y+1)) (aux (estendeMapa (Mapa c l) a)))
                                                                   where aux (Mapa c ob)= Mapa c (init ob)


