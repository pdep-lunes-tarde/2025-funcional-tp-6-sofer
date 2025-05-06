module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | BaconDeTofu
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente BaconDeTofu = 12

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

cuartoDeLibra :: Hamburguesa 
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa | ingredienteBase hamburguesa == Carne = agregarIngrediente Carne hamburguesa
                     | ingredienteBase hamburguesa == Pollo = agregarIngrediente Pollo hamburguesa

ingredienteBase :: Hamburguesa -> Ingrediente
ingredienteBase (Hamburguesa _ ingredientes) | elem Pollo ingredientes = Pollo
                                             | otherwise = Carne

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente (Hamburguesa precioBase ingredientes) 
    = Hamburguesa (cambiarPrecio precioBase ingrediente) (ingrediente:ingredientes)

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
pdepBurger = (agregarIngrediente Cheddar .agregarIngrediente Panceta .agregarIngrediente Panceta . agrandar . agrandar)cuartoDeLibra



  