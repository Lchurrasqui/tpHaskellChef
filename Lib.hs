


data Participante = Participante {
    nombre :: String,
    trucos :: [Truco],
    especialidad :: Plato
} 

data Plato = Plato {
    nombrePlato :: String,
    dificultad :: Int,
    componentes :: [Componente]
} deriving (Show, Eq)

type Componente = (String, Int)

type Truco = Plato -> Plato

---------
--Parte A
---------

endulzar :: Int -> Plato -> Plato
endulzar unaCantidad unPlato = unPlato { componentes = actualizarLista "azucar" unaCantidad (componentes unPlato)}

salar :: Int -> Plato -> Plato
salar unaCantidad unPlato = unPlato { componentes = actualizarLista "sal" unaCantidad (componentes unPlato)}

darSabor :: Int -> Int -> Plato -> Plato
darSabor cantSal cantAzucar unPlato = endulzar cantAzucar (salar cantSal unPlato)

duplicarPorcion :: Plato -> Plato
duplicarPorcion unPlato = unPlato { componentes = map duplicarComponente (componentes unPlato) }

simplificar :: Plato -> Plato
simplificar unPlato
    | esUnBardo unPlato = quitarMenoresA10 (unPlato { dificultad = 5 })
    | otherwise         = unPlato

-- 
esUnBardo :: Plato -> Bool
esUnBardo unPlato = dificultad unPlato > 7 && length (componentes unPlato) > 5

quitarMenoresA10 :: Plato -> Plato
quitarMenoresA10 unPlato = unPlato { componentes = filter cantidadValida (componentes unPlato) }

cantidadValida :: Componente -> Bool
cantidadValida (_, cantidad) = cantidad >= 10    
--
duplicarComponente :: Componente -> Componente
duplicarComponente (nombre, cantidad) = (nombre, cantidad * 2)
--
actualizarLista :: String -> Int -> [Componente] -> [Componente]

-- Caso base
actualizarLista nombreNuevo cantNueva [] = [(nombreNuevo, cantNueva)]

-- Caso recursivo
actualizarLista nombreNuevo cantNueva ((nombreActual, cantActual):restoComponentes)
    | nombreNuevo == nombreActual = (nombreActual, cantActual + cantNueva) : restoComponentes
    | otherwise                   = (nombreActual, cantActual) : actualizarLista nombreNuevo cantNueva restoComponentes

--
esVegano :: Plato -> Bool
esVegano unPlato = not (any esIngredienteAnimal (componentes unPlato))

esIngredienteAnimal :: Componente -> Bool
esIngredienteAnimal (nombre, _) = nombre `elem` ["carne", "huevo", "leche", "queso", "miel", "manteca"]

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not (any (\(nombre, _) -> nombre == "harina") (componentes unPlato))

-- esComplejo = esUnBardo 

noAptoHipertensos :: Plato -> Bool
noAptoHipertensos unPlato = any (\(nombre, cantidad) -> nombre == "sal" && cantidad > 2) (componentes unPlato)

-------------

pepeRoncino :: Participante
pepeRoncino = Participante {
    nombre = "Pepe Roncino",
    trucos = [darSabor 2 5, simplificar, duplicarPorcion],
    especialidad = Plato {
        nombrePlato = "Carne con papas",
        dificultad = 8,
        componentes = [("carne", 200), ("sal", 3), ("azucar", 1), ("harina", 100), ("huevo", 2), ("leche", 500)]
    }
}

--------------
--Parte C
--------------

cocinar :: Participante -> Plato
cocinar unParticipante = foldl (\platoActual truco -> truco platoActual) (especialidad unParticipante) (trucos unParticipante)

esMejorQue :: Plato -> Plato -> Bool
esMejorQue plato1 plato2 = dificultad plato1 > dificultad plato2 && sumarPesoComponentes plato1 < sumarPesoComponentes plato2

sumarPesoComponentes :: Plato -> Int
sumarPesoComponentes unPlato = sum (map snd (componentes unPlato))

participanteEstrella :: [Participante] -> Participante
participanteEstrella [] = error "No hay participantes"

participanteEstrella [participante] = participante
participanteEstrella (participante1:participante2:restoParticipantes)
    | esMejorQue (cocinar participante1) (cocinar participante2) = participanteEstrella (participante1:restoParticipantes)
    | otherwise = participanteEstrella (participante2:restoParticipantes)


---------
--Parte D
---------
platinum :: Plato
platinum = Plato {
    nombrePlato = "Platinum",
    dificultad = 10,
    componentes = map (\n -> ("Ingrediente " ++ show n, n)) [1..]
}

{-
Las funciones que no se van a poder aplicar al platinum son; salar, endulzar, darSabor, simplificar, noAptoHipertensos, esIngredienteAnimal, esSinTaccy esVegano.
Porque lenght tiene que contar todos los elementos de la lista de componentes, y como en este caso es infinita, no se va a poder calcular.
Las funciones que necesitan comparar un string con el nombre de un ingrediente, tampoco se van a poder aplicar.
Las que si se pueden aplicar son; duplicarPorcion, esUnBardo, esComplejo, esMejorQue, sumarPesoComponentes, participanteEstrella.
-}
