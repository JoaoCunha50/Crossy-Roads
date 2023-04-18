module Main where

import LI12223
import Tarefa1_2022li1g014
import Tarefa2_2022li1g014
import Tarefa3_2022li1g014
import Tarefa4_2022li1g014
import Tarefa5_2022li1g014
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit 
import System.Directory
import System.Console.Terminfo (Color(White))

data Menu = Save
            |MenuSkins
            |ModoJogo
            |Lose 
            |Opçoes Opções deriving (Show,Read,Eq)

data Opções = Play | Skins | Quit deriving (Show,Read,Eq)
          
          
 
type World = (Menu,Jogo,Jogada,Skin,Score, Pics, Float)
type Pics = [Picture]

type Score = Int
data Skin = Default | Warrior | Mage deriving (Show,Read)

estadoGlossInicial :: Pics-> World
estadoGlossInicial pics  = (Opçoes Play, initMap , Parado , Default , 0 , pics,0)

initMap = Jogo (Jogador (4,5)) (Mapa 9 [(Relva , [n,n,n,n,n,n,n,n,n]),(Relva , [n,n,n,n,n,n,n,n,n]),(Relva ,  [n,n,Arvore,n,n,n,Arvore,n,n]),(Relva , [n,n,Arvore,n,n,n,Arvore,n,n]),(Relva ,  [n,n,Arvore,n,n,n,Arvore,n,n]),(Relva ,  [n,n,Arvore,n,n,n,Arvore,n,n]),(Relva ,  [n,n,Arvore,n,n,n,Arvore,n,n]),(Relva , [n,n,Arvore,n,n,n,Arvore,n,n]),(Relva , [n,n,Arvore,n,n,n,Arvore,n,n]),(Relva , [n,n,Arvore,n,n,n,Arvore,n,n])]) where n=Nenhum 
                                                                                                                                                                                                                                                                                                                                                                                                                                                car=Carro           
                                                                                                                                                                                                                                                                                                                                                                                                 


reageEventoGloss :: Event -> World -> IO World
reageEventoGloss (EventKey (Char 'q') Down _ _) (m, Jogo (Jogador (x,y))(Mapa c l) , jogada , skin , score,  pics ,t) =     
      do putStrLn "It Was Great To Have You" 
         exitSuccess

reageEventoGloss  (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo, Jogo (Jogador (x,y))(Mapa c l) , jogada , skin , score,  pics ,t) =
      do return (Save, Jogo (Jogador (x,y))(Mapa c l) , jogada , skin , score,  pics ,t)
reageEventoGloss  (EventKey (SpecialKey KeySpace) Down _ _) (Save, Jogo (Jogador (x,y))(Mapa c l) , jogada , skin , score,  pics ,t) =
      return (ModoJogo, Jogo (Jogador (x,y))(Mapa c l) , jogada , skin , score,  pics ,t)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)  (ModoJogo, Jogo (Jogador (x,y))(Mapa c l) , jogada , skin , score,  pics ,t) = return (ModoJogo,animaJogador (Jogo (Jogador (x,y))(Mapa c l))(Move Cima),jogada ,skin,score,pics,t)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)  (ModoJogo, Jogo (Jogador (x,y))(Mapa c l) , jogada , skin , score,  pics ,t) = return (ModoJogo,animaJogador (Jogo (Jogador (x,y))(Mapa c l))(Move Baixo),jogada ,skin,score,pics,t)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _)  (ModoJogo, Jogo (Jogador (x,y))(Mapa c l) , jogada , skin , score,  pics ,t) = return (ModoJogo,animaJogador (Jogo (Jogador (x,y))(Mapa c l))(Move Esquerda),jogada ,skin,score,pics,t)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _)  (ModoJogo, Jogo (Jogador (x,y))(Mapa c l) , jogada , skin , score,  pics ,t)= return (ModoJogo,animaJogador (Jogo (Jogador (x,y))(Mapa c l))(Move Direita),jogada ,skin,score,pics,t)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Lose, jogo, jogada , skin , score , pics,t) = return (Opçoes Play , jogo , jogada , skin , 0 , pics ,t)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opçoes Play, jogo, jogada , skin , score , pics,t) = return  (ModoJogo, jogo, jogada , Default , score , pics,t) 
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opçoes Skins, jogo, jogada , skin , score , pics,t) = return  (MenuSkins, jogo, jogada , Default , score , pics,t) 
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (MenuSkins , jogo , jogada , Default , score , pics , t) = return (MenuSkins , jogo , jogada , Warrior , score , pics , t)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (MenuSkins , jogo , jogada , Mage , score , pics , t) = return (MenuSkins , jogo , jogada , Default , score , pics , t)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (MenuSkins , jogo , jogada , Warrior , score , pics , t) = return (MenuSkins , jogo , jogada , Mage , score , pics , t)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (MenuSkins , jogo , jogada , Default , score , pics , t) = return (MenuSkins , jogo , jogada , Mage , score , pics , t)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (MenuSkins , jogo , jogada , Warrior , score , pics , t) = return (MenuSkins , jogo , jogada , Default, score , pics , t)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (MenuSkins , jogo , jogada , Mage , score , pics , t) = return (MenuSkins , jogo , jogada , Warrior , score , pics , t)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuSkins , jogo , jogada , Default , score , pics , t) = return (ModoJogo , jogo , jogada , Default , score , pics , t)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuSkins , jogo , jogada , Mage , score , pics , t) = return (ModoJogo , jogo , jogada , Mage , score , pics , t)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuSkins , jogo , jogada , Warrior , score , pics , t) = return (ModoJogo , jogo , jogada , Warrior , score , pics , t)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opçoes Play, jogo, jogada , skin , score , pics,t) = return (Opçoes Skins, jogo ,jogada , skin , score,  pics,t)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opçoes Quit, jogo, jogada , skin , score , pics,t) = return (Opçoes Play, jogo ,jogada , skin , score,pics,t)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opçoes Quit, jogo, jogada , skin , score , pics,t) = return (Opçoes Skins,jogo,jogada , skin , score,pics,t)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opçoes Play, jogo, jogada , skin , score , pics,t) = return (Opçoes Quit, jogo,jogada,skin,score, pics,t)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opçoes Skins, jogo, jogada , skin , score , pics,t) = return (Opçoes Play, jogo,jogada,skin,score, pics,t)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opçoes Skins, jogo, jogada , skin , score , pics,t) = return (Opçoes Quit, jogo ,jogada , skin , score,pics,t)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opçoes Quit, jogo, jogada , skin , score , pics,t) = 
    do putStrLn "It Was Great To Have You" 
       exitSuccess 
reageEventoGloss _ w = return w


reageTempoGloss :: Float -> World -> IO World
reageTempoGloss n (ModoJogo, Jogo (Jogador (x,y))(Mapa c l),jogada,skin,score, pics, 99) =
      if jogoTerminou (Jogo (Jogador (x,y))(Mapa c l)) then return (Lose, initMap , Parado , skin , score , pics,0) else return $ (ModoJogo, caminhoevelocidadeLinha (score+x+y)  (animaJogo (deslizaJogo (score-x-y) (Jogo (Jogador (x,y))(Mapa c l)))jogada),Parado,skin, score+1, pics,0) 
reageTempoGloss n (ModoJogo, jogo,jogada,skin,score, pics, t) =
     if jogoTerminou jogo then return $ (Lose, initMap , Parado , skin , score , pics,0) else return $ (ModoJogo,jogo,jogada,skin, score + 1, pics, t+1)
reageTempoGloss _ w = return $ w 

fr :: Int 
fr = 50

dm :: Display
dm = InWindow "Crossy Roads da wish" (900, 900) (0, 0)
 

desenhaEstadoGloss :: World -> IO Picture
desenhaEstadoGloss (Opçoes Play, jogo,jogada,skin,score, pics, t)= return $ scale 0.7 0.7 $ Pictures [Scale 1.8 1.8 $ (!!) pics 1 , Color blue $ Translate (-305) 80 $ Text (show Play),Translate (-305) (-220) $ Text (show Quit),Translate (-305) (-70) $ Text (show Skins)]
desenhaEstadoGloss (Opçoes Quit, jogo, jogada,skin,score,pics, t)= return $ scale 0.7 0.7 $ Pictures [Scale 1.8 1.8 $ (!!) pics 1 ,Translate (-305) 80 $ Text (show Play),Color red $ Translate (-305) (-220) $ Text (show Quit),Translate (-305) (-70) $ Text (show Skins)]
desenhaEstadoGloss (Opçoes Skins, jogo, jogada,skin,score,pics, t)= return $ scale 0.7 0.7 $ Pictures [Scale 1.8 1.8 $ (!!) pics 1 ,Translate (-305) 80 $ Text (show Play), Translate (-305) (-220) $ Text (show Quit),Color green $ Translate (-305) (-70) $ Text (show Skins)]
desenhaEstadoGloss (MenuSkins, jogo, jogada,Default,score,pics, t)= return $ Pictures [Scale 3.5 3.5 $ Translate (-100) 0 $ (!!) pics 5 , Scale 3.5 3.5 $ Translate 100 0 $ (!!) pics 8, Scale 3.5 3.5 $ Translate 0 0 $ (!!) pics 9 ,Scale 0.5 0.5 $ Translate (-200) 280 $ Text (show Warrior), Color blue $ Scale 0.5 0.5 $ Translate (-850) 280 $ Text (show Default), Scale 0.5 0.5 $ Translate 550 280 $ Text (show Mage)]
desenhaEstadoGloss (MenuSkins, jogo, jogada,Mage,score,pics, t)= return $ Pictures [Scale 3.5 3.5 $ Translate (-100) 0 $ (!!) pics 5 , Scale 3.5 3.5 $ Translate 100 0 $ (!!) pics 8,Scale 3.5 3.5 $ Translate 0 0 $ (!!) pics 9 , Scale 0.5 0.5 $ Translate (-200) 280 $ Text (show Warrior), Scale 0.5 0.5 $ Translate (-850) 280 $ Text (show Default), Color blue $ Scale 0.5 0.5 $ Translate 550 280 $ Text (show Mage)]
desenhaEstadoGloss (MenuSkins, jogo, jogada,Warrior,score,pics, t)= return $ Pictures [Scale 3.5 3.5 $ Translate (-100) 0 $ (!!) pics 5 , Scale 3.5 3.5 $ Translate 100 0 $ (!!) pics 8, Scale 3.5 3.5 $ Translate 0 0 $ (!!) pics 9 , Color blue $ Scale 0.5 0.5 $ Translate (-200) 280 $ Text (show Warrior), Scale 0.5 0.5 $ Translate (-850) 280 $ Text (show Default),Scale 0.5 0.5 $ Translate 550 280 $ Text (show Mage)]
desenhaEstadoGloss (ModoJogo,Jogo (Jogador (x,y)) (Mapa c l),jogada, Default ,score,pics,t)= return  (Pictures [Pictures (drawTerrenosMapa (terrenoPos  (Mapa c l) (t,500))),Pictures (drawObs (drawlinhaObs (Mapa c l) ((-400,500),t) pics)),(player x y t Default pics),mostraScore score])
desenhaEstadoGloss (ModoJogo,Jogo (Jogador (x,y)) (Mapa c l),jogada, Mage ,score,pics,t)= return  (Pictures [Pictures (drawTerrenosMapa (terrenoPos  (Mapa c l) (t,500))),Pictures (drawObs (drawlinhaObs (Mapa c l) ((-400,500),t) pics)),(player2 x y t Mage pics),mostraScore score])
desenhaEstadoGloss (ModoJogo,Jogo (Jogador (x,y)) (Mapa c l),jogada, Warrior ,score,pics,t)= return  (Pictures [Pictures (drawTerrenosMapa (terrenoPos  (Mapa c l) (t,500))),Pictures (drawObs (drawlinhaObs (Mapa c l) ((-400,500),t) pics)),(player3 x y t Mage pics),mostraScore score])
desenhaEstadoGloss (Lose,Jogo (Jogador (x,y)) (Mapa c l),jogada,skin,score,pics,t) = return  (Pictures [Scale 1.5 1.5 $ (!!) pics 2 , Scale 0.5 0.5 $ Color white $ Translate (-800) 150 (Text "Good Luck Next Time" ),Scale 0.3 0.3 $ Color white $ Translate (-355) 50 $ Text ("EndScore :"++ (show score)),Scale 0.5 0.5 $ Color white $ Translate 390 (-500) $ Text ("Enter"), Color white $ Translate 380 (-225) $ Polygon [(0 ,(-30)),(0,30),(40,0)]])
desenhaEstadoGloss (Save , Jogo (Jogador (x,y)) (Mapa c l),jogada,skin,score,pics,t) = return (Pictures [Scale 1.5 1.5 $ (!!) pics 1 ,Scale 0.5 0.5 $ Translate (-600) 150 (Text "Game Paused"), Scale 0.3 0.3 $ Translate (-355) 50 $ Text ("YourScore :"++ (show score))])

player::Int->Int->Float->Skin->[Picture]->Picture
player x y t skin pics = Translate ((i*100)-400) (400-(j*100)-t) $ playeraux t pics 
                       where playeraux t pics = if t>75 || (t>25 && t<50) then scale 1.3 1.3 $ pics !! 0 else scale 1.3 1.3 $ last pics    
                             i=fromIntegral x 
                             j=fromIntegral y 
player2::Int->Int->Float->Skin->[Picture]->Picture
player2 x y t skin pics = Translate ((i*100)-400) (400-(j*100)-t) $ playeraux t pics 
                       where playeraux t pics = if t>75 || (t>25 && t<50) then pics !! 6 else pics !! 7    
                             i=fromIntegral x 
                             j=fromIntegral y
player3::Int->Int->Float->Skin->[Picture]->Picture
player3 x y t skin pics = Translate ((i*100)-400) (400-(j*100)-t) $ playeraux t pics 
                       where playeraux t pics = if t>75 || (t>25 && t<50) then pics !! 10 else pics !! 11    
                             i=fromIntegral x 
                             j=fromIntegral y                       
mostraScore:: Score -> Picture
mostraScore score = Translate 380 410 $ scale 0.2 0.2 $ Text (show score)
           

terrenoPos::Mapa->(Float,Float)->[(Terreno,Float,Float)]
terrenoPos (Mapa c []) (_,_) = []
terrenoPos (Mapa c ((ter,_):l)) (t,y) = (ter,t,y):terrenoPos (Mapa c l) (t,(y-100))

drawTerrenosMapa :: [(Terreno,Float,Float)]->[Picture]
drawTerrenosMapa [] = []
drawTerrenosMapa ((ter,t,y):l) = fPos (ter,t,y) : drawTerrenosMapa l

fPos::(Terreno,Float,Float)->Picture 
fPos (ter,t,y) = Translate 0 (y-t) $ drawTileTerreno ter
                 where drawTileTerreno :: Terreno->Picture 
                       drawTileTerreno (Rio v) = Color blue $ rectangleSolid 900 100 
                       drawTileTerreno Relva = Color green $ rectangleSolid 900 100
                       drawTileTerreno (Estrada v) = Color (greyN 0.3) $ rectangleSolid 900 100   

drawlinhaObs::Mapa->((Float,Float),Float)->Pics->[(Terreno,Obstaculo,Float,Float,Pics)]
drawlinhaObs (Mapa c []) ((x,y),t) pics = []
drawlinhaObs (Mapa c ((ter,[]):l)) ((x,y),t) pics = drawlinhaObs (Mapa c l) ((-400,y-100),t) pics
drawlinhaObs (Mapa c ((ter,(o1:tf)):l)) ((x,y),t) pics = (ter,o1,x,y-t,pics):drawlinhaObs (Mapa c ((ter , tf):l)) ((x+100,y),t) pics 

drawObs::[(Terreno,Obstaculo,Float,Float,Pics)]->[Picture]
drawObs [] = []
drawObs ((ter,ob,x,y,pics):l) = fPos2 (ter ,ob ,x ,y ,pics):drawObs l
                   where fPos2 (ter,ob,x,y,pics) = Translate x y $ desenhaobstaculo ter ob pics

desenhaobstaculo (Estrada v1) Carro pics = if v1 >0 then scale 0.5 0.5  $ (!!) pics 3 else scale 0.5 0.5 $ (!!) pics 4
desenhaobstaculo (Rio v2) Tronco pics = scale 0.3 0.3 $ (!!) pics 13
desenhaobstaculo Relva Arvore pics = scale 0.35 0.35 $ Translate 0 50 $ (!!) pics 12
desenhaobstaculo _ _ _ = Blank   

caminhoevelocidadeLinha::Int->Jogo->Jogo
caminhovelocidadeLinha r (Jogo j (Mapa l ((Rio v1 , ob):(Rio v2 , ob2):t))) = Jogo j (Mapa l ((Rio (proximasVelocidadesValidas v2 !! mod r (length(proximasVelocidadesValidas v2))) , ob):(Rio v2 , ob2):t))    
caminhoevelocidadeLinha a (Jogo j (Mapa l ((Rio v1,ob):t)))=Jogo j (Mapa l ((Rio (proximasVelocidadesValidas 0 !! mod a (length(proximasVelocidadesValidas 0))) ,ob):t))
caminhoevelocidadeLinha a (Jogo j (Mapa l ((Estrada v1,ob):t)))=Jogo j (Mapa l ((Estrada (proximasVelocidadesValidas 0 !! mod a (length(proximasVelocidadesValidas 0))),ob):t))
caminhoevelocidadeLinha a (Jogo j (Mapa l ((Relva,ob1):(Relva,ob2):t))) = if caminhopossivel ob1 ob2 ==False then Jogo j (estendeMapa (Mapa l ((Relva,ob2):t)) (a+1)) else (Jogo j (Mapa l ((Relva,ob1):(Relva,ob2):t)))
caminhoevelocidadeLinha _ j = j

caminhopossivel::[Obstaculo]->[Obstaculo]->Bool
caminhopossivel [] [] = False 
caminhopossivel [Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum] (x:y) = False 
caminhopossivel (x:y) [Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum] = False 
caminhopossivel [Carro,Carro,Nenhum,Carro,Carro,Carro,Carro,Nenhum,Carro] (x:y) = False
caminhopossivel (Nenhum:obs1) (Nenhum:ob2) = True 
caminhopossivel (x:y) (w:z) = caminhopossivel y z 


proximasVelocidadesValidas::Int -> [Int]
proximasVelocidadesValidas v | v>0 = [-1,-2]
                             | v<0 = [1,2]
                             |otherwise = [-1,-2,1,2]


main :: IO ()
main = do  
    warrior <- loadBMP "Warrior.bmp"
    warrior2 <- loadBMP "Warrior2.bmp"
    warrior1 <- loadBMP "Warrior1.bmp"
    mage <- loadBMP "Mage.bmp"
    mage1 <- loadBMP "mage1.bmp"
    mage2 <- loadBMP "mage2.bmp"   
    defaultskin <- loadBMP "Default.bmp" 
    default1 <- loadBMP "Untitled.bmp"
    default2 <- loadBMP "Untitled1.bmp"
    car1 <- loadBMP "carro1.bmp"
    car2 <- loadBMP "carro2.bmp"
    arv1 <- loadBMP "arv1.bmp"
    tronco1 <- loadBMP "tronco1.bmp"
    fundo <- loadBMP "fundo.bmp"
    fundo1 <- loadBMP "gameover.bmp"
    let pics = [default1 ,fundo,fundo1, scale 0.75 0.75 $ car1 ,scale 0.66 0.66 car2,defaultskin ,scale 1.3 1.3 mage1,scale 1.3 1.3 mage2,mage,warrior,scale 1.3 1.3 warrior1,scale 1.3 1.3 warrior2, arv1 ,tronco1, default2]  
    playIO dm                     
           white
           fr                        
           (estadoGlossInicial pics)
           desenhaEstadoGloss   
           reageEventoGloss  
           reageTempoGloss  