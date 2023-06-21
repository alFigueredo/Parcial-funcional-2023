module Spec where
import PdePreludat
import Library
import Test.Hspec

dorothy :: Animal
dorothy = Animal {
  nombreAnimal = "Dorothy",
  tipoAnimal = "Vaca",
  peso = 690,
  edad = 20,
  estaEnfermo = False,
  visitasMedicas = [VisitaMedica 20 500, VisitaMedica 40 800]
}

pachi :: Animal
pachi = Animal {
  nombreAnimal = "Pachi",
  tipoAnimal = "Vaca",
  peso = 560,
  edad = 5,
  estaEnfermo = True,
  visitasMedicas = []
}

gachi :: Animal
gachi = Animal {
  nombreAnimal = "Gachi",
  tipoAnimal = "Vaca",
  peso = 560,
  edad = 5,
  estaEnfermo = False,
  visitasMedicas = []
}

proceso1 :: Proceso
proceso1 = [engorde 4, revisacion (VisitaMedica 20 400), chequeoDePeso 500]

proceso2 :: Proceso
proceso2 = [revisacion (VisitaMedica 20 400), engorde 8, chequeoDePeso 500]

correrTests :: IO ()
correrTests = hspec $ do

  -- Punto 1

  describe "Tests punto 1" $ do
    it "Función laPasoMal" $ do
      dorothy `shouldSatisfy` laPasoMal
      pachi `shouldNotSatisfy` laPasoMal
    it "Función nombreFalopa" $ do
      dorothy `shouldNotSatisfy` nombreFalopa
      pachi `shouldSatisfy` nombreFalopa
  
  -- Punto 2

  describe "Tests punto 2" $ do
    it "Actividad engorde" $ do
      (peso . engorde 12) dorothy `shouldBe` 695
      (peso . engorde 4) dorothy `shouldBe` 692
    it "Actividad revisacion si el animal no está enfermo" $ do
      (peso . revisacion (VisitaMedica 20 400)) dorothy `shouldBe` 690
      (visitasMedicas . revisacion (VisitaMedica 20 400)) dorothy `shouldMatchList` [VisitaMedica 20 500, VisitaMedica 40 800]
    it "Actividad revisacion si el animal está enfermo" $ do
      (peso . revisacion (VisitaMedica 20 400)) pachi `shouldBe` 561
      (visitasMedicas . revisacion (VisitaMedica 20 400)) pachi `shouldMatchList` [VisitaMedica 20 400]
    it "Actividad festejoCumple" $ do
      (edad . festejoCumple) dorothy `shouldBe` 21
      (peso . festejoCumple) dorothy `shouldBe` 689
    it "Actividad chequeoDePeso" $ do
      (estaEnfermo . chequeoDePeso 500) dorothy `shouldBe` False
      (estaEnfermo . chequeoDePeso 700) dorothy `shouldBe` True
      (estaEnfermo . chequeoDePeso 500) pachi `shouldBe` True
      (estaEnfermo . chequeoDePeso 700) pachi `shouldBe` True

  -- Punto 3

  describe "Tests punto 3" $ do
    it "Función aplicarProceso" $ do
      (peso . aplicarProceso ejemploProceso) dorothy `shouldBe` 691
      (visitasMedicas . aplicarProceso ejemploProceso) dorothy `shouldMatchList` [VisitaMedica 20 500, VisitaMedica 40 800]
      (edad . aplicarProceso ejemploProceso) dorothy `shouldBe` 21
      (estaEnfermo . aplicarProceso ejemploProceso) dorothy `shouldBe` False

  -- Punto 4

  describe "Tests punto 4" $ do
    it "Función mejoraONoMejora" $ do
      dorothy `shouldNotSatisfy` mejoraONoMejora ejemploProceso
      dorothy `shouldSatisfy` mejoraONoMejora proceso1
      dorothy `shouldNotSatisfy` mejoraONoMejora proceso2
  
  -- Punto 5

  describe "Tests punto 5" $ do
    it "Función giveMeThree" $ do
      giveMeThree [dorothy, pachi, dorothy, gachi, dorothy, pachi, gachi, dorothy] `shouldMatchList` [pachi, gachi, pachi]
      giveMeThree [pachi, dorothy, dorothy, gachi] `shouldMatchList` [pachi, gachi]
