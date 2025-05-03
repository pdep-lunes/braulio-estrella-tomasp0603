module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data Personaje = UnPersonaje {
    nombre :: String,
    poder :: (Personaje->Personaje),
    superPoder :: (Personaje->Personaje),
    superPoderActivo :: Bool,
    vida :: Int
} deriving (Show)

agregarEspinaNombre :: String->String
agregarEspinaNombre unNombre =  unNombre ++ "Espina estuvo aqui"

desactivarSuper :: Bool->Bool
desactivarSuper super 
  | super = not super
  | otherwise = super

bolaEspinosa :: Personaje->Personaje
bolaEspinosa unPersonaje
  | vida unPersonaje >= 1000 = unPersonaje {vida = vida unPersonaje - 1000}
  | otherwise = unPersonaje { vida = vida unPersonaje - vida unPersonaje}

lluviaDeTuercas :: Bool->Personaje->Personaje
lluviaDeTuercas esSanadora unPersonaje
  | esSanadora = unPersonaje {vida = vida unPersonaje + 800}
  | otherwise = unPersonaje {vida = vida unPersonaje - div (vida unPersonaje) 2}

granadaDeEspinas :: Int->Personaje->Personaje
granadaDeEspinas radioExplosion unPersonaje
  | radioExplosion > 3 && vida unPersonaje < 800 = unPersonaje {nombre = agregarEspinaNombre $ nombre unPersonaje, superPoderActivo = desactivarSuper $ superPoderActivo unPersonaje, vida =  vida unPersonaje - vida unPersonaje}  
  | radioExplosion > 3 = unPersonaje { nombre = agregarEspinaNombre $ nombre unPersonaje}
  | otherwise = bolaEspinosa unPersonaje

torretaCurativa :: Personaje->Personaje
torretaCurativa unPersonaje = unPersonaje {vida = (vida unPersonaje) * 2, superPoderActivo = not (desactivarSuper $ superPoderActivo unPersonaje)}

atacarPoderEspecial :: Personaje->Personaje->Personaje
atacarPoderEspecial atacante defensor
  | superPoderActivo atacante = ((poder atacante).(superPoder atacante)) defensor
  | otherwise = defensor

estaEnLasUltimas :: Personaje->Bool
estaEnLasUltimas unPersonaje = vida unPersonaje < 800

espina :: Personaje
espina = UnPersonaje "Espina" bolaEspinosa (granadaDeEspinas 5) True 4800

pamela :: Personaje
pamela = UnPersonaje "Pamela" (lluviaDeTuercas True) torretaCurativa False 9600





