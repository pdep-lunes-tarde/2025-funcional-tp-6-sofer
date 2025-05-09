module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 6: Hamburguesas" $ do 
        describe "Agrandar" $ do
            it "cada vez que se agranda una hamburgusea ponemos otro ingrediente base" $ do
                
                agrandar cuartoDeLibra `shouldBe` cuartoDeLibraPrueba
                 