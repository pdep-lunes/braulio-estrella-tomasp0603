module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data Personaje = unPersonaje {
    nombre :: String,
    poder :: String,
    superPoder :: String,
    superPoderActivo :: Bool,
    vida :: Int
}

agregarEspinaNombre :: Personaje->Personaje
agregarEspinaNombre unPersonaje = nombre unPersonaje + "Espina estuvo aqui"

desactivarSuper :: Personaje->Personaje
desactivarSuper unPersonaje 
  | superPoderActivo unPersonaje = not.superPoderActivo unPersonaje
  | otherwise = superPoderActivo unPersonaje

bolaEspinosa :: Personaje->Personaje
bolaEspinosa unPersonaje
  | vida unPersonaje >= 1000 = vida unPersonaje - 1000
  | otherwise = vida unPersonaje - vida unPersonaje

lluviaDeTuercas :: Personaje->Bool->Personaje
lluviaDeTuercas unPersonaje esSanadora 
  | esSanadora = vida unPersonaje + 800
  | otherwise = vida unPersonaje - div (vida unPersonaje) 2



granadaDeEspinas :: Int->Personaje->Personaje
granadaDeEspinas radioExplosion unPersonaje
  | radioExplosion > 3 && unPersonaje vida < 800 = desactivarSuper unPersonaje vida unPersonaje - vida unPersonaje  
  | radioExplosion > 3 = 