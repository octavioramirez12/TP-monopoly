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

actualizarTactica :: Tactica -> Participante -> Participante
actualizarTactica nuevaTactica unParticipante = unParticipante {tactica = nuevaTactica}

accionista :: Tactica
accionista = "accionista"

oferenteSingular :: Tactica
oferenteSingular = "oferente singular"

compradorCompulsivo :: Tactica
compradorCompulsivo = "comprador compulsivo"

gritar :: Accion
gritar unParticipante = unParticipante {nombre = (nombre unParticipante) ++ "AHHHH"}

pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = (actualizarTactica compradorCompulsivo) unParticipante {dinero = dinero unParticipante + 40}

pagarAAccionistas :: Accion
pagarAAccionistas unParticipante | ((tactica unParticipante) == accionista) = unParticipante {dinero = dinero unParticipante + 200.0}
                                 | otherwise                            = unParticipante {dinero = dinero unParticipante - 100}

enojarse :: Accion
enojarse unParticipante = unParticipante {dinero = dinero unParticipante + 50, acciones = (acciones unParticipante) ++ [gritar]}

subastar :: Propiedad->Participante->Participante
subastar unaPropiedad unParticipante| tactica unParticipante == "accionista" || tactica unParticipante == "oferente singular" = unParticipante {dinero = dinero unParticipante + (snd unaPropiedad), propiedadesCompradas = (propiedadesCompradas unParticipante) ++ [unaPropiedad]}
                                    | otherwise                                                                               = unParticipante

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = unParticipante {dinero = dinero unParticipante + calcularGanancias (propiedadesCompradas unParticipante)}


calcularGanancias :: [Propiedad] -> Float
calcularGanancias unasPropiedades = sum (map precioAlquiler unasPropiedades)

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