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



agregarEspinaNombre :: Personaje->Personaje
agregarEspinaNombre unPersonaje = unPersonaje {nombre = nombre unPersonaje + "Espina estuvo aqui"}

desactivarSuper :: Personaje->Personaje
desactivarSuper unPersonaje 
  | superPoderActivo unPersonaje = unPersonaje {superPoderActivo = not.superPoderActivo unPersonaje}
  | otherwise = unPersonaje

activarSuper :: Personaje->Personaje
activarSuper unPersonaje
  | superPoderActivo unPersonaje = unPersonaje
  | otherwise = unPersonaje {superPoderActivo = not.superPoderActivo unPersonaje}

bolaEspinosa :: Personaje->Personaje
bolaEspinosa unPersonaje
  | vida unPersonaje >= 1000 = unPersonaje {vida = vida unPersonaje - 1000}
  | otherwise = unPersonaje { vida = vida unPersonaje - vida unPersonaje}

lluviaDeTuercas :: Personaje->Bool->Personaje
lluviaDeTuercas unPersonaje esSanadora 
  | esSanadora = unPersonaje {vida = vida unPersonaje + 800}
  | otherwise = unPersonaje {vida = vida unPersonaje - div (vida unPersonaje) 2}

granadaDeEspinas :: Int->Personaje->Personaje
granadaDeEspinas radioExplosion unPersonaje
  | radioExplosion > 3 && unPersonaje vida < 800 = unPersonaje {nombre = agregarEspinaNombre unPersonaje, superPoderActivo = desactivarSuper unPersonaje, vida =  vida unPersonaje - vida unPersonaje}  
  | radioExplosion > 3 = unPersonaje { nombre = agregarEspinaNombre unPersonaje}

torretaCurativa :: Personaje->Personaje
torretaCurativa unPersonaje = unPersonaje {vida = (vida unPersonaje) * 2, superPoderActivo = activarSuper unPersonaje}

atacarPoderEspecial :: Personaje->Personaje->Personaje
atacarPoderEspecial atacante defensor
  | superPoderActivo atacante = poder atacante defensor
  | otherwise = defensor

estaEnLasUltimas :: Personaje->String
estaEnLasUltimas unPersonaje = unPersonaje.vida < 800

Espina :: Personaje
Espina = UnPersonaje "Espina" bolaEspinosa (granadaDeEspinas 5) true 4800

Pamela :: Personaje
Pamela = UnPersonaje "Pamela" lluviaDeTuercas torretaCurativa false 9600





