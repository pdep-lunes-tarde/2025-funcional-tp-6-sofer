module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 6: Hamburguesas" $ do 
        describe "**Agrandar**. Cada vez que se agranda una hamburgusea ponemos otro ingrediente base" $ do
            it "Agrandamos un Cuarto de Libra" $ do
                let cuartoDeLibraPrueba = Hamburguesa 20 [Carne,Pan,Carne,Cheddar,Pan] 
                agrandar cuartoDeLibra `shouldBe` cuartoDeLibraPrueba
            it "Agrandamos un Doble Cuarto Vegano" $ do
                let dobleCuartoVeganoPrueba = Hamburguesa 20 [PatiVegano,QuesoDeAlmendras,PatiVegano,PanIntegral,PatiVegano,QuesoDeAlmendras,PanIntegral] 
                agrandar dobleCuartoVegano `shouldBe` dobleCuartoVeganoPrueba

        describe "**agregarIngrediente**: recibe un ingrediente y una hambrugesa lo agrega a la hamburguesa." $ do
            it "Agregamos papas a un Cuarto de Libra" $ do
                let cuartoDeLibraPrueba = Hamburguesa 20 [Papas,Pan,Carne,Cheddar,Pan]
                agregarIngrediente Papas cuartoDeLibra `shouldBe` cuartoDeLibraPrueba
        
        describe "**descuento**: recibe un % de descuento, y devuelve la hamburguesa con ese descuento aplicado al precio base." $ do
            it "Aplicamos 15% descuento a un Cuarto de Libra" $ do
                let cuartoDeLibraPrueba = Hamburguesa 17 [Pan, Carne, Cheddar, Pan]
                descuento 15 cuartoDeLibra `shouldBe` cuartoDeLibraPrueba

        describe "**pdepBurger**" $ do
            it "agrandamos dos veces un cuarto de libra con panceta y cheddar y 20% de descuento" $ do
                let pdepBurgerPrueba = Hamburguesa 16 [Cheddar,Panceta, Carne, Carne,Pan, Carne,Cheddar, Pan]
                pdepBurger `shouldBe` pdepBurgerPrueba 
        
        describe "**dobleCuarto**" $ do
            it "es un cuarto de libra con carne y cheddar." $ do
                let dobleCuartoPrueba = Hamburguesa 20 [Cheddar, Carne,Pan, Carne, Cheddar, Pan] 
                dobleCuarto `shouldBe` dobleCuartoPrueba 

        describe "*bigPdep**" $ do 
            it "Es un doble cuarto con curry" $ do
                let bigPdepPrueba = Hamburguesa 20 [Curry, Cheddar, Carne,Pan, Carne, Cheddar, Pan]
                bigPdep `shouldBe` bigPdepPrueba
        
        describe "**delDia** dada una hamburguesa, le agrega Papas y un descuento del 30%" $ do
            it "una big pdep del dia y debería ser como una big pdep (doble cuarto con curry) pero con papas y el descuento del 30%." $ do
                let bigPdepDelDia = Hamburguesa 14 [Papas, Curry, Cheddar, Carne,Pan, Carne, Cheddar, Pan]
                delDia bigPdep `shouldBe` bigPdepDelDia

        describe "**hacerVeggie** : cambia todos los ingredientes base que hayan en la hamburguesa por PatiVegano, el cheddar lo cambia por queso de almendras y la panceta por bacon de tofu." $ do
            it "Hacemos veggie un cuarto de libra" $ do
                let cuartoLibraVeggie = Hamburguesa 20 [Pan, PatiVegano, QuesoDeAlmendras, Pan]
                hacerVeggie cuartoDeLibra `shouldBe` cuartoLibraVeggie    
        
        describe "**cambiarPanDePati** :cambia el Pan que haya en la hamburguesa por PanIntegral" $ do
            it "Cambiamos el pan de un Cuarto de Libra" $ do 
                let cambiarPanCuartoDeLibra = Hamburguesa 20 [PanIntegral, Carne, Cheddar, PanIntegral]
                cambiarPanDePati cuartoDeLibra `shouldBe` cambiarPanCuartoDeLibra
        
        describe "**dobleCuartoVegano**" $ do
            it "es un dobleCuarto veggie con pan integral" $ do
                let dobleCuartoVeggie = Hamburguesa 20 [QuesoDeAlmendras, PatiVegano, PanIntegral, PatiVegano, QuesoDeAlmendras, PanIntegral]
                dobleCuartoVegano `shouldBe` dobleCuartoVeggie
                