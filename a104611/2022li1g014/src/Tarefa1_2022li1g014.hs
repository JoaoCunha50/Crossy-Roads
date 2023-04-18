{- |
Module      : Tarefa1_2022li1g014
Description : Validação de um mapa
Copyright   : Gonçalo da Silva Alves <a104079@alunos.uminho.pt>
              João Manuel Machado da Cunha <a104611@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.

Para realizar a seguinte tarefa criamos várias funções auxiliares que seguiam as condicionantes do enunciado , tais como,

-Não existem obstáculos em terrenos impróprios, e.g. troncos em estradas ou relvas,  ́arvores em rios ou estradas, etc;
-Rios contíguos têm direções opostas;
-Rios têm de ter obrigatoriamente um tronco;
-Troncos têm, no máximo, 5 unidades de comprimento;
-Carros têm, no máximo, 3 unidades de comprimento;
-Em qualquer linha existe, no mínimo, um “obstáculo” Nenhum. Ou seja, uma linha não pode ser composta exclusivamente por obstáculos ,precisando de haver pelo menos um espaço livre;
-O comprimento da lista de obstáculos de cada linha corresponde exatamente `a largura do mapa;
-Contiguamente, não devem existir mais do que 4 rios, nem 5 estradas ou relvas .
-}
module Tarefa1_2022li1g014 where

import LI12223

{- | A função 'obstaculosMapa' recebe um mapa e considera-o verdadeiro se seguir a primeira condição , ou falso caso não seja esse o caso .

Esta função pode ser definida por :

@

obstaculosMapa::Mapa ->Bool
obstaculosMapa (Mapa x [])=True
obstaculosMapa (Mapa x ((Rio v, l):t)) |elem Arvore l || elem Carro l = False
                                       |otherwise = obstaculosMapa (Mapa x t)
obstaculosMapa (Mapa x ((Estrada v , l):t)) |elem Arvore l || elem Tronco l = False
                                            |otherwise = obstaculosMapa (Mapa x t) 
obstaculosMapa (Mapa x ((Relva , l):t)) |elem Carro l || elem Tronco l = False
                                        |otherwise = obstaculosMapa (Mapa x t)
@

==Exemplos de utilização :

>>>obstaculosMapa (Mapa 2 [(Rio 2,[Arvore,Nenhum])])
False

>>>obstaculosMapa (Mapa 2 [(Relva,[Arvore,Nenhum])])
True
-}

obstaculosMapa::Mapa ->Bool
obstaculosMapa (Mapa x [])=True
obstaculosMapa (Mapa x ((Rio v, l):t)) |elem Arvore l || elem Carro l = False
                                       |otherwise = obstaculosMapa (Mapa x t)
obstaculosMapa (Mapa x ((Estrada v , l):t)) |elem Arvore l || elem Tronco l = False
                                            |otherwise = obstaculosMapa (Mapa x t) 
obstaculosMapa (Mapa x ((Relva , l):t)) |elem Carro l || elem Tronco l = False
                                        |otherwise = obstaculosMapa (Mapa x t)

{- | A função 'riosContiguos' recebe um mapa e considera-o verdadeiro caso siga a condição 2 , não sendo esse o caso considera-o falso .

Esta função está definida por :

@

riosContiguos::Mapa -> Bool
riosContiguos (Mapa x [(a,l)])=True
riosContiguos (Mapa x ((Rio v1 ,l1):(Rio v2 ,l2):t)) |v1>0 && v2>0 = False
                                                     |v1<0 && v2<0 = False
                                                     |otherwise = riosContiguos (Mapa x ((Rio v2,l2):t))
riosContiguos (Mapa x ((Rio v1 ,l1):(Estrada v,l):t))=riosContiguos (Mapa x ((Estrada v,l):t))
riosContiguos (Mapa x ((Rio v1 ,l1):(Relva,l):t))=riosContiguos (Mapa x ((Relva,l):t))
riosContiguos (Mapa x ((Estrada v,l):t))=riosContiguos (Mapa x t)
riosContiguos (Mapa x ((Relva,l):t))=riosContiguos (Mapa x t)
@

==Exemplos de utilização :

>>>riosContiguos (Mapa 2 [(Rio (-1),[Nenhum,Tronco]),(Rio 1,[Tronco,Nenhum])])
True

>>>riosContiguos (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Rio 1,[Tronco,Nenhum])])
False
-}

riosContiguos::Mapa -> Bool
riosContiguos (Mapa x [(a,l)])=True
riosContiguos (Mapa x ((Rio v1 ,l1):(Rio v2 ,l2):t)) |v1>0 && v2>0 = False
                                                     |v1<0 && v2<0 = False
                                                     |otherwise = riosContiguos (Mapa x ((Rio v2,l2):t))
riosContiguos (Mapa x ((Rio v1 ,l1):(Estrada v,l):t))=riosContiguos (Mapa x ((Estrada v,l):t))
riosContiguos (Mapa x ((Rio v1 ,l1):(Relva,l):t))=riosContiguos (Mapa x ((Relva,l):t))
riosContiguos (Mapa x ((Estrada v,l):t))=riosContiguos (Mapa x t)
riosContiguos (Mapa x ((Relva,l):t))=riosContiguos (Mapa x t)

{- | A função 'rioTronco' classifica um mapa como verdadeiro ou falso tendo como critério a condição 3 .

Esta função esta definida por :

@
rioTronco::Mapa->Bool
rioTronco (Mapa x [])=True
rioTronco (Mapa x ((Rio v,l):t)) |elem Tronco l == False = False
                                 |otherwise = rioTronco (Mapa x t)
rioTronco (Mapa x ((Estrada v ,l):t))=rioTronco (Mapa x t)
rioTronco (Mapa x ((Relva , l):t))=rioTronco (Mapa x t)
@

==Exemplos de utilização :

>>>rioTronco (Mapa 2 [(Rio 2 ,[Nenhum,Nenhum])])
False

>>>rioTronco (Mapa 2 [(Rio 2 ,[Tronco,Nenhum])])
True
-}

rioTronco::Mapa->Bool
rioTronco (Mapa x [])=True
rioTronco (Mapa x ((Rio v,l):t)) |elem Tronco l == False = False
                                 |otherwise = rioTronco (Mapa x t)
rioTronco (Mapa x ((Estrada v ,l):t))=rioTronco (Mapa x t)
rioTronco (Mapa x ((Relva , l):t))=rioTronco (Mapa x t)

{- | A função 'troncoValido' classifica um mapa como verdadeiro ou falso tendo como critério a condição 4 , através de 3 funções auxiliares , a 'troncoValidoAux' e a 'troncoValidoAux2' .
Ps : "A função 'group' agrupa os elementos consecutivos iguais de uma lista , numa lista de listas".

Estas funções estão definidas por :

@

troncoValido::Mapa->Bool
troncoValido (Mapa x [])=True
troncoValido (Mapa x (h:t))|troncoValidoAux h == False ||troncoValidoAux2 h == False = False
                           |otherwise = troncoValido (Mapa x t)

group :: Eq a => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (h:t)
    | elem h (head r) = (h : (head r)) : tail r
    | otherwise = [h] : r
    where r = group t

troncoValidoAux :: (Terreno,[Obstaculo])->Bool
troncoValidoAux (Rio v , [])=True
troncoValidoAux (Rio v,[x1])=True
troncoValidoAux (Rio v,[x1,x2])=True
troncoValidoAux (Rio v,[x1,x2,x3])=True
troncoValidoAux (Rio v,[x1,x2,x3,x4])=True
troncoValidoAux (Rio v,[x1,x2,x3,x4,x5])=True
troncoValidoAux (Rio v , (x1:x2:x3:x4:x5:x6:t)) |x1==Tronco && x1==x2 && x1==x3 && x1==x4 && x1==x5 && x1==x6 = False
                                                |otherwise = troncoValidoAux (Rio v, x2:x3:x4:x5:x6:t)
troncoValidoAux (Estrada v,x)=True
troncoValidoAux (Relva , x)=True

troncoValidoAux2 :: (Terreno, [Obstaculo]) -> Bool
troncoValidoAux2 (Rio v , l) |elem Tronco (head(group l)) && elem Tronco (last(group l)) && (length (head (group l))+length (last (group l))>5) = False
                             |otherwise = True
troncoValidoAux2 (Estrada v,l)=True
troncoValidoAux2 (Relva,l)=True
@
 
==Exemplos de utilização :
>>>troncoValido (Mapa 7 ,[(Rio 2 , [Tronco,Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco])])
False

>>>troncoValido (Mapa 7 ,[(Rio 2 , [Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum,Nenhum])])
 True

>>>troncoValido (Mapa 8 ,[(Rio 2 , [Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum])])
False
-}

troncoValido::Mapa->Bool
troncoValido (Mapa x [])=True
troncoValido (Mapa x (h:t))|troncoValidoAux h == False ||troncoValidoAux2 h == False = False
                           |otherwise = troncoValido (Mapa x t)

group :: Eq a => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (h:t)
    | elem h (head r) = (h : (head r)) : tail r
    | otherwise = [h] : r
    where r = group t

troncoValidoAux :: (Terreno,[Obstaculo])->Bool
troncoValidoAux (Rio v , [])=True
troncoValidoAux (Rio v,[x1])=True
troncoValidoAux (Rio v,[x1,x2])=True
troncoValidoAux (Rio v,[x1,x2,x3])=True
troncoValidoAux (Rio v,[x1,x2,x3,x4])=True
troncoValidoAux (Rio v,[x1,x2,x3,x4,x5])=True
troncoValidoAux (Rio v , (x1:x2:x3:x4:x5:x6:t)) |x1==Tronco && x1==x2 && x1==x3 && x1==x4 && x1==x5 && x1==x6 = False
                                                |otherwise = troncoValidoAux (Rio v, x2:x3:x4:x5:x6:t)
troncoValidoAux (Estrada v,x)=True
troncoValidoAux (Relva , x)=True

troncoValidoAux2 :: (Terreno, [Obstaculo]) -> Bool
troncoValidoAux2 (Rio v , l) |elem Tronco (head(group l)) && elem Tronco (last(group l)) && (length (head (group l))+length (last (group l))>5) = False
                             |otherwise = True
troncoValidoAux2 (Estrada v,l)=True
troncoValidoAux2 (Relva,l)=True

{- | A função 'carroValido' classifica um mapa como verdadeiro ou falso tendo como critério a condição 5, utilizando as funções auxiliares 'carroValidoAux' e 'carroValidoAux2' .

Esta função está definida por :

@

carroValidoAux :: (Terreno,[Obstaculo])->Bool
carroValidoAux (Estrada v,[x1])=True
carroValidoAux (Estrada v,[x1,x2])=True
carroValidoAux (Estrada v,[x1,x2,x3])=True
carroValidoAux (Estrada v , (x1:x2:x3:x4:t)) |x1==Carro && x1==x2 && x1==x3 && x1==x4 = False
                                             |otherwise = carroValidoAux (Estrada v,x2:x3:x4:t)
carroValidoAux (Relva , l)=True
carroValidoAux (Rio v,l)=True

carroValidoAux2 :: (Terreno, [Obstaculo]) -> Bool
carroValidoAux2 (Estrada v,l)  |elem Carro (head (group l)) && elem Carro (last (group l)) && (length (head (group l))+length (last (group l))>3) = False
                               |otherwise = True
carroValidoAux2 (Rio v,l)=True
carroValidoAux2 (Relva,l)=True

carroValido :: Mapa->Bool
carroValido (Mapa x [])=True
carroValido (Mapa x (h:t))|carroValidoAux h == False || carroValidoAux2 h ==False  = False
                          |otherwise = carroValido (Mapa x t) 
@

==Exemplos de utilização :

>>>carroValido (Mapa 5 [(Estrada 2 ,[Carro,Carro,Carro,Nenhum,Carro])])
False

>>>carroValido (Mapa 5 [(Estrada 4 ,[Carro,Carro,Carro,Nenhum,Nenhum])])
True

>>>carroValido (Mapa 6 [(Estrada 2 ,[Carro,Carro,Carro,Carro,Nenhum,Nenhum])])
False
-}

carroValidoAux :: (Terreno,[Obstaculo])->Bool
carroValidoAux (Estrada v,[x1])=True
carroValidoAux (Estrada v,[x1,x2])=True
carroValidoAux (Estrada v,[x1,x2,x3])=True
carroValidoAux (Estrada v , (x1:x2:x3:x4:t)) |x1==Carro && x1==x2 && x1==x3 && x1==x4 = False
                                             |otherwise = carroValidoAux (Estrada v,x2:x3:x4:t)
carroValidoAux (Relva , l)=True
carroValidoAux (Rio v,l)=True

carroValidoAux2 :: (Terreno, [Obstaculo]) -> Bool
carroValidoAux2 (Estrada v,l)  |elem Carro (head (group l)) && elem Carro (last (group l)) && (length (head (group l))+length (last (group l))>3) = False
                               |otherwise = True
carroValidoAux2 (Rio v,l)=True
carroValidoAux2 (Relva,l)=True

carroValido :: Mapa->Bool
carroValido (Mapa x [])=True
carroValido (Mapa x (h:t))|carroValidoAux h == False || carroValidoAux2 h ==False  = False
                          |otherwise = carroValido (Mapa x t) 

{- | A função 'obsSeguidos' classifica um mapa como verdadeiro ou falso tendo como critério a condição 6 . 

Esta função está definida por :

@

obsSeguidos::Mapa->Bool
obsSeguidos (Mapa x [])=True
obsSeguidos (Mapa x ((a,l):t)) |elem Nenhum l == False =False
                               |otherwise = obsSeguidos (Mapa x t)
@

==Exemplos de utilização :

>>>obsSeguidos (Mapa 2 [(Estrada 3 ,[Carro,Carro])])
False

>>>obsSeguidos (Mapa 3 [(Estrada 2 ,[Nenhum,Nenhum,Carro])])
True
-}


obsSeguidos::Mapa->Bool
obsSeguidos (Mapa x [])=True
obsSeguidos (Mapa x ((a,l):t)) |elem Nenhum l == False =False
                               |otherwise = obsSeguidos (Mapa x t)

{- | A função 'larguraObs' classifica um mapa como verdadeiro ou falso tendo como critério a condição 7 . 

Esta função está definida por :

@

larguraObs::Mapa->Bool
larguraObs (Mapa x [])=True
larguraObs (Mapa x ((te,l):t)) |x<=0 = False
                               |x/=length l = False 
                               |otherwise = larguraObs (Mapa x t)
@

==Exemplos de utilização : 

>>>larguraObs (Mapa 2 [(Rio 2 ,[Tronco])])
False

>>>larguraObs (Mapa 2 [(Rio 2 ,[Tronco,Nenhum])])
True
-}

larguraObs::Mapa->Bool
larguraObs (Mapa x [])=True
larguraObs (Mapa x ((te,l):t)) |x<=0 = False
                               |x/=length l = False 
                               |otherwise = larguraObs (Mapa x t)

{- | A função 'terrenosContiguos' classifica um mapa como verdadeiro ou falso tendo como critério a última condição .

Esta função está definida por :

@

terrenosContiguos::Mapa->Bool
terrenosContiguos (Mapa x [])=True
terrenosContiguos (Mapa x ((Estrada v,l):(Estrada v2,l2):(Estrada v3,l3):(Estrada v4 ,l4):(Estrada v5,l5):(Estrada v6,l6):t))=False
terrenosContiguos (Mapa x ((Rio v,l):(Rio v2,l2):(Rio v3,l3):(Rio v4 ,l4):(Rio v5,l5):t))=False
terrenosContiguos (Mapa x ((Relva,l):(Relva,l2):(Relva,l3):(Relva,l4):(Relva,l5):(Relva,l6):t))=False
terrenosContiguos (Mapa x (h:t)) = terrenosContiguos (Mapa x t)
@

==Exemplos de utilização :

>>>terrenosContiguos (Mapa 2 [(Rio 2 ,[Nenhum,Tronco]),(Rio 3 ,[Tronco,Nenhum]),(Rio 1 ,[Tronco,Nenhum]),(Rio (-1),[Nenhum,Tronco]),(Rio 2 ,[Nenhum,Tronco])])
False

>>>terrenosContiguos (Mapa 2 [(Rio 2 ,[Nenhum,Tronco]),(Rio 3 ,[Tronco,Nenhum]),(Rio 1 ,[Tronco,Nenhum]),(Rio (-1),[Nenhum,Tronco]),(Estrada 2 ,[Nenhum,Nenhum])])
True
-}

terrenosContiguos::Mapa->Bool
terrenosContiguos (Mapa x [])=True
terrenosContiguos (Mapa x ((Estrada v,l):(Estrada v2,l2):(Estrada v3,l3):(Estrada v4 ,l4):(Estrada v5,l5):(Estrada v6,l6):t))=False
terrenosContiguos (Mapa x ((Rio v,l):(Rio v2,l2):(Rio v3,l3):(Rio v4 ,l4):(Rio v5,l5):t))=False
terrenosContiguos (Mapa x ((Relva,l):(Relva,l2):(Relva,l3):(Relva,l4):(Relva,l5):(Relva,l6):t))=False
terrenosContiguos (Mapa x (h:t)) = terrenosContiguos (Mapa x t)

{- | Por fim a função principal , que junta todas as outras e classifica um mapa tendo em conta todas as condições .

Esta função está definida por :

@

mapaValido :: Mapa -> Bool
mapaValido (Mapa x l)= if obstaculosMapa (Mapa x l) && riosContiguos (Mapa x l) && rioTronco (Mapa x l) && troncoValido (Mapa x l) && carroValido (Mapa x l) && obsSeguidos (Mapa x l) && larguraObs (Mapa x l) && terrenosContiguos (Mapa x l) then True else False
@

==Exemplos de utilização :

>>>mapaValido (Mapa 2 [(Rio 2 ,[Nenhum,Tronco]),(Rio 2 ,[Nenhum,Tronco]),(Estrada 2 , [Carro,Carro])])
False

>>>mapaValido (Mapa 2 [(Rio 2 ,[Nenhum,Tronco]),(Rio (-2) ,[Nenhum,Tronco]),(Estrada 2 , [Carro,Carro])])
False

>>>mapaValido (Mapa 2 [(Rio 2 ,[Nenhum,Tronco]),(Rio (-2) ,[Nenhum,Tronco]),(Estrada 2 , [Nenhum,Carro])])
True

>>>mapaValido (Mapa 2 [(Rio 2 ,[Nenhum,Tronco]),(Rio (-2) ,[Nenhum,Nenhum]),(Estrada 2 , [Carro,Nenhum])])
False

>>>mapaValido (Mapa 6 [(Rio 2 ,[Nenhum,Tronco,Nenhum,Tronco,Nenhum,Nenhum]),(Rio (-2) ,[Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco]),(Estrada 2 , [Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum])])
False

>>>mapaValido (Mapa 6 [(Rio 2 ,[Nenhum,Tronco,Nenhum,Tronco,Nenhum,Nenhum]),(Rio (-2) ,[Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]),(Estrada 2 , [Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum])])
True
-}

mapaValido :: Mapa -> Bool
mapaValido (Mapa x l)= if obstaculosMapa (Mapa x l) && riosContiguos (Mapa x l) && rioTronco (Mapa x l) && troncoValido (Mapa x l) && carroValido (Mapa x l) && obsSeguidos (Mapa x l) && larguraObs (Mapa x l) && terrenosContiguos (Mapa x l) then True else False
