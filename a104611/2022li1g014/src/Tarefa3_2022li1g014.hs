{- |
Module      : Tarefa3_2022li1g014
Description : Movimentação do personagem e obstáculos
Copyright   : Gonçalo da Silva Alves <a104079@alunos.uminho.pt>
              João Manuel Machado da Cunha <a104611@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.

Para realizar a seguinte tarefa criamos várias funções auxiliares que seguiam as condicionantes do enunciado , tais como,

-Numa estrada ou rio com velocidade v, os obstáculos devem mover-se |v| unidades na direcção determinada;
-As jogadas Move Cima, Move Baixo, etc. fazem com que o jogador se mova 1 unidade para cima, baixo, etc, respectivamente;
-Mesmo quando o jogador não efectua qualquer movimento (i.e. a sua jogada é Parado), se o personagem se encontrar em cima de um tronco, o jogador acompanha o movimento tronco;
-O jogador não consegue escapar do mapa através dos seus movimentos. Por exemplo, se o jogador se encontrar na linha de topo do mapa, então mover-se para cima não tem qualquer efeito, uma vez que já se encontra no limite do mapa;
-Ao deslocar os obstáculos de uma linha, lembre-se que estes, assim que desaparecerem por um dos lados do mapa, devem reaparecer no lado oposto;
-O efeito de deslize do mapa não é para ser implementado nesta função. Por outras palavras, as dimensões do mapa não devem sofrer alterações após invocar esta função.
-}
module Tarefa3_2022li1g014 where

import LI12223

{- | A função 'moveJogadorTronco' recebe a posição do jogador, o terreno onde este se encontra e o obstáculo presente na sua posição e devolve a posição do Jogador. Caso o jogador se encontre no Rio em cima de um Tronco,esta irá
devolver a posição do jogador de acordo com o movimento do tronco, caso contrário devolve a posição onde este se encontrava. Esta função é utilizada como auxiliar da função moveJogador no caso em que
o jogador se encontra Parado.

Esta função pode ser definida por :

@

moveJogadorTronco :: Jogador -> Terreno -> Obstaculo -> Jogador
moveJogadorTronco (Jogador (x,y)) (Rio v) Tronco | v > 0 = moveJogadorTronco (Jogador (x + 1,y)) (Rio (v - 1)) Tronco 
                                                 | v < 0 = moveJogadorTronco (Jogador (x-1,y)) (Rio (v + 1)) Tronco
                                                 | otherwise = Jogador (x,y)
moveJogadorTronco (Jogador (x,y)) (Rio v) Nenhum = Jogador (x,y)
moveJogadorTronco (Jogador (x,y)) (Estrada v) o = Jogador (x,y)
moveJogadorTronco (Jogador (x,y)) Relva o = Jogador (x,y)
@

==Exemplos de utilização :

>>>moveJogadorTronco (Jogador (3,1)) (Rio 2) Tronco
Jogador (5,1)

>>>moveJogadorTronco (Jogador (3,1)) (Rio (-2)) Tronco
Jogador (1,1)

>>>moveJogadorTronco (Jogador (3,1)) (Rio 2) Nenhum
Jogador (3,1)
-}

moveJogadorTronco :: Jogador -> Terreno -> Obstaculo -> Jogada -> Jogador
moveJogadorTronco (Jogador (x,y)) (Rio v) Tronco (Move Direita)| v > 0 = moveJogadorTronco (Jogador (x + 1,y)) (Rio (v - 1)) Tronco (Move Direita)
                                                               | v < 0 = moveJogadorTronco (Jogador (x-1,y)) (Rio (v + 1)) Tronco (Move Direita)
                                                               | otherwise = Jogador (x + 1,y)
moveJogadorTronco (Jogador (x,y)) (Rio v) Tronco (Move Esquerda)| v > 0 = moveJogadorTronco (Jogador (x + 1,y)) (Rio (v - 1)) Tronco (Move Esquerda)
                                                                | v < 0 = moveJogadorTronco (Jogador (x-1,y)) (Rio (v + 1)) Tronco (Move Esquerda)
                                                                | otherwise = Jogador (x - 1,y)
moveJogadorTronco (Jogador (x,y)) (Rio v) Tronco Parado | v > 0 = moveJogadorTronco (Jogador (x + 1,y)) (Rio (v - 1)) Tronco Parado
                                                        | v < 0 = moveJogadorTronco (Jogador (x-1,y)) (Rio (v + 1)) Tronco Parado
                                                        | otherwise = Jogador (x,y) 
moveJogadorTronco (Jogador (x,y)) (Rio v) Nenhum _ = Jogador (x,y)
moveJogadorTronco (Jogador (x,y)) (Estrada v) o _ = Jogador (x,y)
moveJogadorTronco (Jogador (x,y)) Relva o _ = Jogador (x,y)


{- | A função 'moveJogador' recebe o Jogo e a Jogada e devolve a posição do jogador de acordo com a sua jogada. Caso a sua jogada for para fora do mapa, devolve a mesma posição.

Esta função está definida por :

@

moveJogador :: Jogo -> Jogada -> Jogador
moveJogador (Jogo (Jogador (x,y)) (Mapa l l1)) (Move Baixo)  = if y > 0 then Jogador (x,y + 1)
                                                                       else Jogador (x,y)
moveJogador (Jogo (Jogador (x,y)) (Mapa l l1)) (Move Cima) = Jogador (x,y - 1)
moveJogador (Jogo (Jogador (x,y)) (Mapa l l1)) (Move Direita) = if x < (l - 1) then Jogador (x + 1,y)
                                                                               else Jogador (x,y)
moveJogador (Jogo (Jogador (x,y)) (Mapa l l1)) (Move Esquerda) = if x > 0 then Jogador (x - 1,y)
                                                                          else Jogador (x,y)
moveJogador (Jogo (Jogador (x,y)) (Mapa l l1)) Parado = moveJogadorTronco (Jogador (x,y)) (fst ((!!) l1 y)) (((!!) (snd ((!!) l1 y)) x))
@

==Exemplos de utilização :

>>>moveJogador (Jogo (Jogador (2,1)) (Mapa 5 [(Rio (-2),[Nenhum, Tronco, Nenhum, Nenhum, Tronco]),(Estrada (-1),[Carro, Nenhum, Nenhum, Carro, Nenhum]),(Rio 1,[Nenhum, Tronco, Nenhum, Nenhum, Tronco])])) (Move Baixo)
Jogador (2,0)

>>>moveJogador (Jogo (Jogador (2,1)) (Mapa 5 [(Rio (-2),[Nenhum, Tronco, Nenhum, Nenhum, Tronco]),(Estrada (-1),[Carro, Nenhum, Nenhum, Carro, Nenhum]),(Rio 1,[Nenhum, Tronco, Nenhum, Nenhum, Tronco])])) Parado
Jogador (2,1)

>>>moveJogador (Jogo (Jogador (4,0)) (Mapa 5 [(Rio (-2),[Nenhum, Tronco, Nenhum, Nenhum, Tronco]),(Estrada (-1),[Carro, Nenhum, Nenhum, Carro, Nenhum]),(Rio 1,[Nenhum, Tronco, Nenhum, Nenhum, Tronco])])) Parado
Jogador (2,0)

>>>moveJogador (Jogo (Jogador (2,1)) (Mapa 5 [(Rio (-2),[Nenhum, Tronco, Nenhum, Nenhum, Tronco]),(Estrada (-1),[Carro, Nenhum, Nenhum, Carro, Nenhum]),(Rio 1,[Nenhum, Tronco, Nenhum, Nenhum, Tronco])])) (Move Cima)
Jogador (2,2)

>>>moveJogador (Jogo (Jogador (2,1)) (Mapa 5 [(Rio (-2),[Nenhum, Tronco, Nenhum, Nenhum, Tronco]),(Estrada (-1),[Carro, Nenhum, Nenhum, Carro, Nenhum]),(Rio 1,[Nenhum, Tronco, Nenhum, Nenhum, Tronco])])) (Move Esquerda)
Jogador (1,1)

>>>moveJogador (Jogo (Jogador (2,1)) (Mapa 5 [(Rio (-2),[Nenhum, Tronco, Nenhum, Nenhum, Tronco]),(Estrada (-1),[Carro, Nenhum, Nenhum, Carro, Nenhum]),(Rio 1,[Nenhum, Tronco, Nenhum, Nenhum, Tronco])])) (Move Direita)
Jogador (3,1)
-}

moveJogador :: Jogo -> Jogada -> Jogador
moveJogador (Jogo (Jogador (x,y)) (Mapa l l1)) (Move Cima) | y > 0 = if arvore (((!!) (snd ((!!) l1 y)) x)) then Jogador (x,y) else Jogador (x,y - 1)
                                                           | otherwise = Jogador (x,y)
moveJogador (Jogo (Jogador (x,y)) (Mapa l l1)) (Move Baixo) = if arvore (((!!) (snd ((!!) l1 (y+2))) x)) then Jogador (x,y) else Jogador (x,y + 1)
moveJogador (Jogo (Jogador (x,y)) (Mapa l l1)) (Move Direita) | x < (l - 1) = if arvore ((!!) (snd ((!!) l1 (y+1))) (x+1)) then Jogador (x,y) else Jogador (x + 1,y)
                                                              | otherwise = Jogador (x,y)
moveJogador (Jogo (Jogador (x,y)) (Mapa l l1)) (Move Esquerda) | x > 0 = if arvore ((!!) (snd ((!!) l1 (y+1))) (x-1)) then Jogador (x,y) else  Jogador (x - 1,y)
                                                               | otherwise = Jogador (x,y)
moveJogador (Jogo (Jogador (x,y)) (Mapa l l1)) Parado = moveJogadorTronco (Jogador (x,y-1)) (fst ((!!) l1 y)) (((!!) (snd ((!!) l1 y)) x)) Parado

arvore :: Obstaculo -> Bool
arvore Arvore = True 
arvore _ = False

tronco :: Obstaculo -> Bool
tronco Tronco = True 
tronco _ = False

{- | A função 'vObstacAux' é a função que dado um Terreno e a respetiva lista de Obstáculos movimenta a lista de obstáculos de acordo com a sua velocidade.

Esta função está definida por :

@

vObstacAux :: (Terreno, [Obstaculo]) -> [Obstaculo]
vObstacAux ((Estrada v),(h:t)) | v > 0 = vObstacAux ((Estrada (v - 1)),((last t):h:(init t)))
                               | v == 0 = (h:t)
                               | otherwise = vObstacAux ((Estrada (v + 1)),(t ++ [h]))
vObstacAux ((Rio v),(h:t)) | v > 0 = vObstacAux ((Rio (v - 1)),((last t):h:(init t)))
                           | v == 0 = (h:t)
                           | otherwise = vObstacAux ((Rio (v + 1)),(t ++ [h]))
vObstacAux ((Relva),(h:t)) = (h:t) 
@

==Exemplos de utilização :

>>>vObstacAux (Rio (-2),[Nenhum, Tronco, Nenhum, Nenhum, Tronco])
[Nenhum,Nenhum,Tronco,Nenhum,Tronco]

>>>vObstacAux (Estrada 1,[Nenhum, Carro, Nenhum, Carro, Nenhum])
[Nenhum,Nenhum,Carro,Nenhum,Carro]

>>>vObstacAux (Relva,[Nenhum, Arvore, Nenhum, Nenhum, Arvore])
[Nenhum, Arvore, Nenhum, Nenhum, Arvore]
-}

vObstacAux :: Jogador -> (Terreno, [Obstaculo]) -> [Obstaculo]
vObstacAux (Jogador (x,y)) ((Estrada v),l) -- | --v > 0 = --if atropelamento ((!!) l x) then l else)
                                           | v > 0 = vObstacAux (Jogador (x,y)) ((Estrada (v - 1)),((last l):(init l)))
                                           | v == 0 = l
                                           -- | --otherwise = if atropelamento ((!!) l x) then l else vObstacAux (Jogador (x,y)) ((Estrada (v + 1)),((drop 1 l) ++ [head l]))
                                           |otherwise = vObstacAux (Jogador (x,y)) ((Estrada (v + 1)),((drop 1 l) ++ [head l]))
vObstacAux (Jogador (x,y)) ((Rio v),l) | v > 0 = vObstacAux (Jogador (x,y)) ((Rio (v - 1)),((last l):(init l)))
                                       | v == 0 = l
                                       | otherwise = vObstacAux (Jogador (x,y)) ((Rio (v + 1)),((drop 1 l) ++ [head l]))
vObstacAux (Jogador (x,y)) ((Relva),l) = l 

atropelamento :: Obstaculo -> Bool
atropelamento Carro = True
atropelamento Nenhum = False

{- |  A função 'vObstac' recebe uma lista de Terrenos e respetivas listas de obstáculos e que através da função 'vObstacAux' movimenta as listas de Obstáculos, de acordo com a velocidade de cada uma.

Esta função está definida por:

@

vObstac :: [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
vObstac [] = []
vObstac ((t,l):xs) = (t,vObstacAux (t,l)) : vObstac xs 
@

==Exemplos de utilização :

>>>vObstac ([(Rio 2,[Nenhum, Tronco, Nenhum, Nenhum, Tronco]),(Estrada (-1),[Carro, Nenhum, Nenhum, Carro, Nenhum]),(Relva,[Nenhum, Arvore, Nenhum, Nenhum, Arvore])])
[(Rio 2,[Nenhum,Tronco,Nenhum,Tronco,Nenhum]),(Estrada (-1),[Nenhum,Nenhum,Carro,Nenhum,Carro]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Arvore])]

>>>vObstac ([(Rio (-1),[Nenhum, Tronco, Nenhum, Tronco, Nenhum]),(Relva,[Nenhum, Arvore, Nenhum, Nenhum, Arvore]),(Estrada 1,[Carro, Nenhum, Carro, Nenhum, Nenhum])])
[(Rio (-1),[Tronco,Nenhum,Tronco,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Arvore]),(Estrada 1,[Nenhum,Carro,Nenhum,Carro,Nenhum])]
-}

vObstac :: Jogador -> [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
vObstac (Jogador (x,y)) [] = []
vObstac (Jogador (x,y)) ((t,l):xs) = (t,vObstacAux (Jogador (x,y)) (t,l)) : vObstac (Jogador (x,y)) xs
{- | Por fim a função principal, que junta todas as outras e movimenta os obstáculos (de acordo com a velocidade) do terreno em que se encontram, e o personagem, de acordo com a jogada dada .

Esta função está definida por :

@


animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa l l1)) j = (Jogo (moveJogador (Jogo (Jogador (x,y)) (Mapa l l1)) j) (Mapa l (vObstac (moveJogador (Jogo (Jogador (x,y)) (Mapa l l1)) j) l1)))
@

==Exemplos de utilização :

>>>animaJogo (Jogo (Jogador (2,1)) (Mapa 5 [(Rio (-2),[Nenhum, Tronco, Nenhum, Nenhum, Tronco]),(Estrada (-1),[Carro, Nenhum, Nenhum, Carro, Nenhum]),(Rio 1,[Nenhum, Tronco, Nenhum, Nenhum, Tronco])])) (Move Esquerda)
Jogo (Jogador (1,1)) (Mapa 5 [(Rio (-2),[Nenhum,Nenhum,Tronco,Nenhum,Tronco]),(Estrada (-1),[Nenhum,Nenhum,Carro,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum])])

>>>animaJogo (Jogo (Jogador (4,0)) (Mapa 5 [(Rio (-2),[Nenhum, Tronco, Nenhum, Nenhum, Tronco]),(Estrada (-1),[Carro, Nenhum, Nenhum, Carro, Nenhum]),(Rio 1,[Nenhum, Tronco, Nenhum, Nenhum, Tronco])])) Parado
Jogo (Jogador (2,0)) (Mapa 5 [(Rio (-2),[Nenhum,Nenhum,Tronco,Nenhum,Tronco]),(Estrada (-1),[Nenhum,Nenhum,Carro,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum])])

>>>animaJogo (Jogo (Jogador (4,1)) (Mapa 5 [(Rio (-2),[Nenhum, Tronco, Nenhum, Nenhum, Tronco]),(Estrada (-1),[Carro, Nenhum, Nenhum, Carro, Nenhum]),(Rio 1,[Nenhum, Tronco, Nenhum, Nenhum, Tronco])])) Parado
Jogo (Jogador (4,1)) (Mapa 5 [(Rio (-2),[Nenhum,Nenhum,Tronco,Nenhum,Tronco]),(Estrada (-1),[Nenhum,Nenhum,Carro,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum])])

>>>animaJogo (Jogo (Jogador (4,1)) (Mapa 5 [(Rio (-2),[Nenhum, Tronco, Nenhum, Nenhum, Tronco]),(Estrada (-1),[Carro, Nenhum, Nenhum, Carro, Nenhum]),(Rio 1,[Nenhum, Tronco, Nenhum, Nenhum, Tronco])])) (Move Baixo)
Jogo (Jogador (4,0)) (Mapa 5 [(Rio (-2),[Nenhum,Nenhum,Tronco,Nenhum,Tronco]),(Estrada (-1),[Nenhum,Nenhum,Carro,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum])])

>>>animaJogo (Jogo (Jogador (4,1)) (Mapa 5 [(Rio (-2),[Nenhum, Tronco, Nenhum, Nenhum, Tronco]),(Estrada (-1),[Carro, Nenhum, Nenhum, Carro, Nenhum]),(Rio 1,[Nenhum, Tronco, Nenhum, Nenhum, Tronco])])) (Move Cima)
Jogo (Jogador (4,2)) (Mapa 5 [(Rio (-2),[Nenhum,Nenhum,Tronco,Nenhum,Tronco]),(Estrada (-1),[Nenhum,Nenhum,Carro,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum])])

>>>animaJogo (Jogo (Jogador (1,1)) (Mapa 5 [(Rio (-2),[Nenhum, Tronco, Nenhum, Nenhum, Tronco]),(Estrada (-1),[Carro, Nenhum, Nenhum, Carro, Nenhum]),(Rio 1,[Nenhum, Tronco, Nenhum, Nenhum, Tronco])])) (Move Direita)
Jogo (Jogador (2,1)) (Mapa 5 [(Rio (-2),[Nenhum,Nenhum,Tronco,Nenhum,Tronco]),(Estrada (-1),[Nenhum,Nenhum,Carro,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum])])
-}

animaJogador :: Jogo -> Jogada -> Jogo
animaJogador (Jogo (Jogador (x,y)) (Mapa l l1)) j = (Jogo (moveJogador (Jogo (Jogador (x,y)) (Mapa l l1)) j) (Mapa l l1))

animaMapa :: Jogo -> Jogo
animaMapa (Jogo (Jogador (x,y)) (Mapa l l1)) = (Jogo (Jogador (x,y)) ( Mapa l (vObstac (Jogador (x,y)) l1)))

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa l l1)) j = (Jogo (moveJogador (Jogo (Jogador (x,y+1)) (Mapa l l1)) j) (Mapa l (vObstac (moveJogador (Jogo (Jogador (x,y)) (Mapa l l1)) j) l1)))