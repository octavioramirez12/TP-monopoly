data Participante =
    Participante
    {
        nombre :: String,
        dinero :: Float,
        tactica :: Tactica,
        propiedadesCompradas :: [Propiedad],
        acciones :: [Accion]
    }

type Propiedad = (String, Float)
type Accion = (Participante->Participante)
type Tactica = String

modificarTactica :: Tactica -> Participante -> Participante
modificarTactica nuevaTactica unParticipante = unParticipante {tactica = nuevaTactica}

accionista :: Tactica
accionista = "accionista"

oferenteSingular :: Tactica
oferenteSingular = "oferente singular"

compradorCompulsivo :: Tactica
compradorCompulsivo = "comprador compulsivo"

gritar :: Accion
gritar unParticipante = unParticipante {nombre = (nombre unParticipante) ++ "AHHHH"}

pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = ((modificarTactica compradorCompulsivo).(modificarDinero (+40))) unParticipante

pagarAAccionistas :: Accion
pagarAAccionistas unParticipante | ((tactica unParticipante) == accionista) = (modificarDinero (+200)) unParticipante
                                 | otherwise                            = (modificarDinero (subtract 100)) unParticipante

enojarse :: Accion
enojarse unParticipante = modificarDinero (+50) unParticipante {acciones = (acciones unParticipante) ++ [gritar]}

subastar :: Propiedad->Participante->Participante
subastar propiedadSubastada (Participante unNombre unDinero accionista unasPropiedades unasAcciones) = (Participante unNombre (unDinero - (snd propiedadSubastada)) accionista (unasPropiedades ++ [propiedadSubastada]) unasAcciones)
subastar propiedadSubastada (Participante unNombre unDinero oferenteSingular unasPropiedades unasAcciones) = (Participante unNombre (unDinero - (snd propiedadSubastada)) oferenteSingular (unasPropiedades ++ [propiedadSubastada]) unasAcciones)
subastar propiedadSubastada (Participante unNombre unDinero unaTactica unasPropiedades unasAcciones) = (Participante unNombre unDinero unaTactica unasPropiedades unasAcciones)

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = unParticipante {dinero = dinero unParticipante + calcularGanancias (propiedadesCompradas unParticipante)}


calcularGanancias :: [Propiedad] -> Float
calcularGanancias unasPropiedades = sum (map precioAlquiler unasPropiedades)

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unParticipante | notElem unaPropiedad (propiedadesCompradas unParticipante) = ((modificarDinero (+10)).gritar) unParticipante
                                              | otherwise = unParticipante

modificarDinero :: (Float -> Float) -> Participante -> Participante
modificarDinero funcion unParticipante = unParticipante {dinero = funcion (dinero unParticipante)}

precioAlquiler :: Propiedad -> Float
precioAlquiler unaPropiedad | esPropiedadBarata unaPropiedad = 10
                            | otherwise = 20

esPropiedadBarata :: Propiedad->Bool
esPropiedadBarata = ((<150).snd)

inicializarParticipante :: String->Tactica->Accion->Participante
inicializarParticipante unNombre unaTactica accionInicial= Participante unNombre 500.0 unaTactica [] ([pasarPorElBanco] ++ [accionInicial])

carolina :: Participante
carolina = inicializarParticipante "Carolina" accionista pagarAAccionistas

manuel :: Participante
manuel = inicializarParticipante "Manuel" oferenteSingular enojarse

ultimaRonda :: Participante -> Accion
ultimaRonda unParticipante = foldr1 (.) (acciones unParticipante)

dineroFinal :: Participante -> Float
dineroFinal unParticipante= (dinero.(ultimaRonda unParticipante)) unParticipante

juegoFinal :: Participante -> Participante -> Participante
juegoFinal participante1 participante2 | (dineroFinal participante1) > (dineroFinal participante2) = participante1
                                       | otherwise = participante2