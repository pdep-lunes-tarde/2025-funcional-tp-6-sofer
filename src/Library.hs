module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | BaconDeTofu | Papas | PatiVegano | PanIntegral 
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente BaconDeTofu = 12
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

cuartoDeLibra :: Hamburguesa 
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa | ingredienteBase hamburguesa == Carne = agregarIngrediente Carne hamburguesa
                     | ingredienteBase hamburguesa == Pollo = agregarIngrediente Pollo hamburguesa
                     | ingredienteBase hamburguesa == PatiVegano = agregarIngrediente PatiVegano hamburguesa

ingredienteBase :: Hamburguesa -> Ingrediente
ingredienteBase (Hamburguesa _ ingredientes) | elem Pollo ingredientes = Pollo
                                             | elem PatiVegano ingredientes = PatiVegano
                                             | otherwise = Carne

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente (Hamburguesa precioBase ingredientes) 
    = Hamburguesa precioBase (ingrediente:ingredientes)

cambiarPrecio :: Number -> Ingrediente -> Number
cambiarPrecio precioBase ingrediente =  precioBase + precioIngrediente ingrediente

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa 
    = modificarPrecioBase (calcularDescuento porcentaje(precioBase hamburguesa)) hamburguesa

calcularDescuento :: Number -> Number -> Number
calcularDescuento descuento precioBase = (1- descuento/100) * precioBase 

modificarPrecioBase :: Number -> Hamburguesa -> Hamburguesa
modificarPrecioBase nuevoPrecio (Hamburguesa precioViejo ingredientes) 
    = Hamburguesa nuevoPrecio ingredientes

pdepBurger :: Hamburguesa 
pdepBurger = (descuento 20 .agregarIngrediente Cheddar .agregarIngrediente Panceta . agrandar . agrandar)cuartoDeLibra

dobleCuarto :: Hamburguesa 
dobleCuarto = (agregarIngrediente Cheddar .agrandar)cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia  = descuento 30 .agregarIngrediente Papas  

--parte 3

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie (Hamburguesa precioBase ingredientes) 
    = Hamburguesa precioBase (map hacerVeggieIngredientes ingredientes)

hacerVeggieIngredientes :: Ingrediente -> Ingrediente
hacerVeggieIngredientes Pollo = PatiVegano
hacerVeggieIngredientes Carne = PatiVegano
hacerVeggieIngredientes Cheddar = QuesoDeAlmendras
hacerVeggieIngredientes Panceta = BaconDeTofu
hacerVeggieIngredientes otro = otro

precioFinal :: Hamburguesa -> Number
precioFinal (Hamburguesa precioBase ingredientes) 
    = sum (map precioIngrediente ingredientes) + precioBase

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati (Hamburguesa precioBase ingredientes) = 
    Hamburguesa precioBase (map cambiarPan ingredientes)

cambiarPan :: Ingrediente -> Ingrediente
cambiarPan Pan = PanIntegral
cambiarPan otraCosa = otraCosa

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = (cambiarPanDePati . hacerVeggie ) dobleCuarto

