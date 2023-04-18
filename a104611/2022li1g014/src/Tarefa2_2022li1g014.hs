{- |
Module      : Tarefa2_2022li1g014
Description : Geração contínua de um mapa
Copyright   : Gonçalo da Silva Alves <a104079@alunos.uminho.pt>
              João Manuel Machado da Cunha <a104611@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.

Na realização desta tarefa utilizei as 2 funções auxiliares indicadas no enunciado e mais duas para dar aleatoriedade á função principal , 'estendeMapa' .
-}
module Tarefa2_2022li1g014 where

import LI12223

{- | A função auxiliar 'proximosTerrenosValidos' recebe um mapa e dá-nos a lista de terrenos possíveis de colocar numa próxima linha do mapa .

Esta função está definida por :

@

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa l [])=[Relva,Rio 0 ,Estrada 0]
proximosTerrenosValidos (Mapa l [(Rio v, a),(Rio v1, a1),(Rio v2, a2),(Rio v3, a3)]) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa l ((Rio v, a):(Rio v1, a1):(Rio v2, a2):(Rio v3, a3):t)) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa l [(Estrada v ,a),(Estrada v1 ,a1),(Estrada v2 ,a2),(Estrada v3 ,a3),(Estrada v4 ,a4)])=[Rio 0,Relva]
proximosTerrenosValidos (Mapa l ((Estrada  v, a):(Estrada  v1, a1):(Estrada v2, a2):(Estrada v3, a3):(Estrada a4, v4):t))=[Rio 0, Relva]
proximosTerrenosValidos (Mapa l [(Relva ,a),(Relva, a1),(Relva , a2),(Relva , a3),(Relva , a4)])=[Estrada 0 , Rio 0]
proximosTerrenosValidos (Mapa l ((Relva ,a):(Relva, a1):(Relva , a2):(Relva , a3):(Relva , a4):t))=[Estrada 0 , Rio 0]
proximosTerrenosValidos (Mapa l t)=[Relva,Rio 0 ,Estrada 0]
@

==Exemplos de utilização :
>>>proximosTerrenosValidos (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada 1,[Nenhum,Carro,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum])])
[Relva,Rio 0,Estrada 0]

>>>proximosTerrenosValidos (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Rio (-2),[Nenhum,Tronco,Nenhum]),(Rio 2,[Nenhum,Tronco,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco])])
[Estrada 0,Relva]

>>>proximosTerrenosValidos (Mapa 3 [(Estrada 2,[Carro,Nenhum,Nenhum]),(Estrada (-2),[Nenhum,Carro,Nenhum]),(Estrada 2,[Nenhum,Carro,Nenhum]),(Estrada (-1),[Nenhum,Carro,Carro]),(Estrada 2 , [Nenhum,Nenhum,Nenhum])])
[Rio 0,Relva]
-}

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa l [])=[Relva,Rio 0 ,Estrada 0]
proximosTerrenosValidos (Mapa l [(Rio v, a),(Rio v1, a1),(Rio v2, a2),(Rio v3, a3)]) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa l ((Rio v, a):(Rio v1, a1):(Rio v2, a2):(Rio v3, a3):t)) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa l [(Estrada v ,a),(Estrada v1 ,a1),(Estrada v2 ,a2),(Estrada v3 ,a3),(Estrada v4 ,a4)])=[Rio 0,Relva]
proximosTerrenosValidos (Mapa l ((Estrada  v, a):(Estrada  v1, a1):(Estrada v2, a2):(Estrada v3, a3):(Estrada a4, v4):t))=[Rio 0, Relva]
proximosTerrenosValidos (Mapa l [(Relva ,a),(Relva, a1),(Relva , a2),(Relva , a3),(Relva , a4)])=[Estrada 0 , Rio 0]
proximosTerrenosValidos (Mapa l ((Relva ,a):(Relva, a1):(Relva , a2):(Relva , a3):(Relva , a4):t))=[Estrada 0 , Rio 0]
proximosTerrenosValidos (Mapa l t)=[Relva,Rio 0 ,Estrada 0]

{- | A função auxiliar 'proximosObstaculosValidos' recebe um tuplo com um terreno e uma lista de obstáculos , e um inteiro que corresponde a largura do mapa , e dá-nos a lista de obstáculos possíveis de juntar a lista de obstáculos anterior com ajuda da função auxiliar proximosObstaculosAux .
Ps- "Faltam os casos do limite de obstáculos seguidos pois estes estão contido na função 'element2'"

Esta função está definida por :

@

proximosObstaculosValidos :: Int-> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos a (x,y)                                                                                                                                                                                                                                                                                                                                                                                                                         
 |length y == (a-1) && (elem Nenhum y) == False = [Nenhum]
 |length y== a = []
 |otherwise = proximosObstaculosValidosAux a (x,y)

proximosObstaculosValidosAux::Int-> (Terreno,[Obstaculo])->[Obstaculo]
proximosObstaculosValidosAux a (Rio b, [])=[Nenhum, Tronco]
proximosObstaculosValidosAux a (Estrada b,[])=[Nenhum, Carro]
proximosObstaculosValidosAux a (Relva, [])=[Nenhum, Arvore]
proximosObstaculosValidosAux a (Rio b,l)=[Nenhum,Tronco]
proximosObstaculosValidosAux a (Estrada b,l)=[Nenhum,Carro]
proximosObstaculosValidosAux a (Relva , l)=[Nenhum,Arvore]
@

==Exemplos de utilização :

>>>proximosObstaculosValidos 2 (Relva,[Nenhum,Arvore])
[]

>>>proximosObstaculosValidos 3 (Relva , [Nenhum,Arvore])
[Nenhum,Arvore]
-}


proximosObstaculosValidos :: Int-> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos a (x,y)                                                                                                                                                                                                                                                                                                                                                                                                                         
 |length y == (a-1) && (elem Nenhum y) == False = [Nenhum]
 |length y== a = []
 |otherwise = proximosObstaculosValidosAux a (x,y)

proximosObstaculosValidosAux::Int-> (Terreno,[Obstaculo])->[Obstaculo]
proximosObstaculosValidosAux a (Rio b, [])=[Nenhum, Tronco]
proximosObstaculosValidosAux a (Estrada b,[])=[Nenhum, Carro]
proximosObstaculosValidosAux a (Relva, [])=[Nenhum, Arvore]
proximosObstaculosValidosAux a (Rio b,l)=[Nenhum,Tronco]
proximosObstaculosValidosAux a (Estrada b,l)=[Nenhum,Carro]
proximosObstaculosValidosAux a (Relva , l)=[Nenhum,Arvore]

{- | A função 'element2' recebe dois inteiros correspondentes a largura e á seed de aleatoriedade , e uma lista de obstáculos , dando-nos uma lista pseudo-aleatória com os obstáculos da lista anterior . 

Esta função está definida por :

@

element2::Int->Int->[Obstaculo]->[Obstaculo]
element2 x a [] = []
element2 x a (h:[Carro,Carro,Carro]) = element2 x a ((h:[Carro,Carro,Carro])++[Nenhum])
element2 x a (h:[Tronco,Tronco,Tronco,Tronco,Tronco]) = element2 x a ((h:[Tronco,Tronco,Tronco,Tronco,Tronco])++[Nenhum])
element2 x a l |x==length l = l
               |length l==(x-1) && elem Carro (head (group l)) && elem Carro (last (group l)) && length (head (group l)) + length (last (group l)) >= 3 = l++[Nenhum]
               |length l==(x-1) && elem Tronco (head (group l)) && elem Tronco (last (group l)) && length (head (group l)) + length (last (group l)) >= 5 = l++[Nenhum]
               |length l==(x+1) = [Nenhum]
               |otherwise = element2 x (a+3) (l++[l !! mod (a+1) (length l)])

group :: Eq a => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (h:t)
    | elem h (head r) = (h : (head r)) : tail r
    | otherwise = [h] : r
    where r = group t
@

==Exemplos de utilização :

>>>element2 2 3 [Nenhum,Carro]
[Nenhum,Carro]

>>>element2 8 3 [Carro,Nenhum]
[Carro,Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Nenhum]
-}

element2::Int->Int->[Obstaculo]->[Obstaculo]
element2 x a [] = []
element2 x a (h:[Carro,Carro,Carro]) = element2 x a ((h:[Carro,Carro,Carro])++[Nenhum,Nenhum])
element2 x a (h:[Tronco,Tronco,Tronco,Tronco,Tronco]) = element2 x a ((h:[Tronco,Tronco,Tronco,Tronco,Tronco])++[Nenhum])
element2 x a (h:[Arvore,Arvore,Arvore,Arvore,Arvore]) = element2 x a ((h:[Arvore,Arvore,Arvore,Arvore,Arvore])++[Nenhum])
element2 x a l |x==length l = l
               |length l==(x-1) && elem Carro (head (group l)) && elem Carro (last (group l)) && length (head (group l)) + length (last (group l)) >= 3 = l++[Nenhum]
               |length l==(x-1) && elem Tronco (head (group l)) && elem Tronco (last (group l)) && length (head (group l)) + length (last (group l)) >= 5 = l++[Nenhum]
               |length l==(x+1) = [Nenhum]
               |otherwise = element2 x (a+3) (l++[l !! mod (a+1) (length l)])

group :: Eq a => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (h:t)
    | elem h (head r) = (h : (head r)) : tail r
    | otherwise = [h] : r
    where r = group t

{- | A função 'element1' recebe um mapa e um inteiro aleatorio, e dá-nos um terreno válido aleatório .

Esta função está definida por :

@

element1::Mapa->Int->Terreno
element1 (Mapa x l) a = proximosTerrenosValidos (Mapa x l) !! mod a (length (proximosTerrenosValidos (Mapa x l)))
@

==Exemplos de Utilização :

>>>element1 2 (Mapa 2 [(Rio 2 , [Nenhum,Tronco])]) 
Estrada 0

>>>element1 4 (Mapa 2 [(Rio 2 , [Nenhum,Tronco])]) 
Rio 0
-}

element1::Int->Mapa->Terreno
element1 a (Mapa x l) = proximosTerrenosValidos (Mapa x l) !! mod a (length (proximosTerrenosValidos (Mapa x l)))

{- | Por fim , a função principal 'estendeMapa' que adiciona uma linha nova ao Mapa dado . 

Esta função está definida por :

@

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa x l) a = Mapa x (l ++ [(element1 a (Mapa x l)  ,element2 x a (proximosObstaculosValidos x (element1 a (Mapa x l), [])))])
@

==Exemplos de utilização :

>>>estendeMapa (Mapa 7 [(Rio 2 , [Tronco,Nenhum ,Nenhum,Nenhum,Nenhum,Tronco,Nenhum]),(Estrada 1 , [Nenhum ,Carro ,Nenhum,Carro,Nenhum,Nenhum,Carro ]),(Relva , [Nenhum ,Nenhum ,Nenhum,Arvore,Nenhum,Arvore,Arvore])]) 12
Mapa 7 [(Rio 2 , [Tronco,Nenhum ,Nenhum,Nenhum,Nenhum,Tronco,Nenhum]),(Estrada 1 , [Nenhum ,Carro ,Nenhum,Carro,Nenhum,Nenhum,Carro ]),(Relva , [Nenhum ,Nenhum ,Nenhum,Arvore,Nenhum,Arvore,Arvore]),(Relva,[Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore])]

>>>estendeMapa (Mapa 7 [(Rio 2 , [Tronco,Nenhum ,Nenhum ,Tronco,Nenhum,tronco,Nenhum ]),(Estrada 1 , [Nenhum ,Carro ,Nenhum,Nenhum,Nenhum,Nenhum,Carro ]),(Relva , [Nenhum ,Nenhum ,Nenhum, Nenhum,Nenhum,Arvore,Arvore])]) 13
Mapa 7 [(Rio 2 , [Tronco,Nenhum ,Nenhum ,Tronco,Nenhum,tronco,Nenhum ]),(Estrada 1 , [Nenhum ,Carro ,Nenhum,Nenhum,Nenhum,Nenhum,Carro ]),(Relva , [Nenhum ,Nenhum ,Nenhum, Nenhum,Nenhum,Arvore,Arvore]),(Rio 0,[Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])]

>>>estendeMapa (Mapa 7 [(Rio 2 , [Tronco,Nenhum ,Nenhum ,Tronco,Nenhum,Tronco,Nenhum ]),(Rio 0 , [Nenhum ,Tronco ,Nenhum,Nenhum,Nenhum,Nenhum,Tronco ]),(Rio 2 , [Nenhum ,Nenhum ,Nenhum, Nenhum,Nenhum,Tronco,Tronco]),(Rio 0,[Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco])]) 13
Mapa 7 [(Rio 2,[Tronco,Nenhum,Nenhum,Tronco,Nenhum,Tronco,Nenhum]),(Rio 0,[Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Rio 2,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Tronco]),(Rio 0,[Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])]
-}

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa x l) a = Mapa x ([(element1 a (Mapa x l)  ,element2 x a (proximosObstaculosValidos x (element1 a (Mapa x l), [])))]++ l)

