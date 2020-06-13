import PdePreludat
import Library
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Como entrenar a tu dragón" $ do
    describe "nivel de ferocidad" $ do
      context "dado un puntaje calculado como la resistencia del dragon + 10 por cada punto de habilidad del mismo" $ do
        it "un dragon cuyo puntaje es menor o igual a 200 tiene nivel 1" $ do
          clasificarSegunFerocidad (UnDragon 200 200 Espanto [] 0) `shouldBe` 1 -- por ejemplo, si tiene 200 de resistencia y no tiene puntos de habilidad
        it "un dragon cuyo puntaje esta entre 201 y 500 (incluido) tiene nivel 2" $ do
          clasificarSegunFerocidad (UnDragon 200 200 Espanto [] 1) `shouldBe` 2 -- por ejemplo, si tiene 200 de resistencia y tiene 1 punto de habilidad
        it "un dragon cuyo puntaje esta entre 501 y 1000 (incluido) tiene nivel 3" $ do
          clasificarSegunFerocidad (UnDragon 502 502 Espanto [] 0) `shouldBe` 3 -- por ejemplo, si tiene 500 de resistencia y tiene 1 punto de habilidad
        it "un dragon cuyo puntaje esta entre 1001 y 2000 (incluido) tiene nivel 4" $ do
          clasificarSegunFerocidad (UnDragon 800 800 Espanto [] 70) `shouldBe` 4
        it "por arriba de nivel 4, el nivel del dragon aumenta en 1 cada 1000 puntos" $ do
          clasificarSegunFerocidad (UnDragon 1700 1700 Espanto [] 50) `shouldBe` 5



    describe "puede usar la habilidad" $ do
      context "dado un dragon que conoce la habilidad" $ do
        context "y no tiene puntos de habilidad" $ do
          it "no puede usarla" $ do
            escribime -- si chimuelo no tuviese puntos de habilidad no podria usar Control sobre Dragones aunque la conoce
        context "y tiene al menos un punto de habilidad" $ do
          it "puede usarla" $ do
            escribime -- chimuelo puede usar Control sobre Dragones
      context "dado un dragon que no conoce la habilidad" $ do
        it "no puede usarla" $ do
          escribime -- chimuelo no puede usar endurecer piel aunque tuviese puntos de habilidad
    describe "usar habilidad" $ do
      it "al usar disparo plasma el dragon pierde 3 puntos de habilidad" $ do
        escribime
      it "al usar control sobre dragones el dragon pierde 2 puntos de habilidad" $ do
        escribime
      describe "al usar endurecer piel" $ do
        it "si el dragon es de clase piedra pierde 1 punto de habilidad" $ do
          escribime
        it "si el dragon no es de clase piedra pierde 5 puntos de habilidad" $ do
          escribime
      describe "al usar escupir lava escupir" $ do
        it "si el dragon tiene mas de 10 de nivel de ferocidad pierde 1 punto de habilidad" $ do
          escribime
        it "si el dragon tiene menos de 10 de nivel de ferocidad, pierde tantos puntos de habilidad como la diferencia entre 10 y su nivel de ferocidad" $ do
          escribime
      it "al usar aliento de hielo el dragon pierde tantos puntos como la diferencia entre su resistencia actual y su resistencia maxima" $ do
        escribime
      it "si un dragon usa una hablidad que le consume mas puntos de habilidad de los que tiene, queda en 0 puntos de habilidad" $ do
        escribime




    describe "tareas" $ do
      describe "transportar mercaderia" $ do
        it "el dragon gasta tanta resistencia como el producto del peso que carga (en kg) por la distancia (en km) que recorre" $ do
          escribime
      describe "descansar" $ do
        it "la resistencia actual del dragon sube hasta su resistencia maxima y sus puntos de habilidad aumentan en 5" $ do
          escribime
      describe "derribar torre" $ do
        it "si el dragon puede usar la habilidad disparo plasma usa esa habilidad, perdiendo 3 puntos de habilidad" $ do
          escribime
        it "si el dragon no puede usar la habilidad disparo plasma, gasta 50 puntos de resistencia" $ do
          escribime
      describe "apagar incendio" $ do
        it "si el dragon puede usar la habilidad aliento de hielo usa esa habilidad, perdiendo tantos puntos como diferencia entre su resistencia maxima y resistencia actual" $ do
          escribime
        it "si el dragon no puede usar la habilidad aliento de hielo, gasta 40 puntos de resistencia" $ do
          escribime
      describe "ahuyentar dragones" $ do
        it "si el dragon puede usar control sobre dragones, usa esa habilidad, perdiendo 2 puntos de habilidad" $ do
          escribime
        context "si el dragon no puede usar control sobre dragones" $ do
          it "pero su nivel de ferocidad es mayor al de todos los dragones, queda igual" $ do
            escribime
          it "pierde tanta resistencia como 10 por la cantidad de dragones mas feroces que el" $ do
            escribime
      describe "cazar recompensa" $ do
        it "un dragon con control sobre dragones pierde solo la resistencia de transportar el tesoro y 2 puntos de habilidad" $ do
          escribime
        it "un dragon sin control sobre dragones pierde resistencia por transportar el tesoro y por ahuyentar a los dragones" $ do
          escribime

    describe "misiones" $ do
      it "al hacer una, el dragon sufre los efectos de hacer todas las tareas en sucesion" $ do
        escribime
      it "al hacer una, si el dragon al final cumple los criterios de aprobacion, aumenta su resistencia maxima en la cantidad de tareas y aprende la habilidad de la mision" $ do
        escribime
      it "al hacer una, si el dragon no cumple los criterios de aprobacion, mantendria la resistencia maxima y habilidades que tenia tras realizar las tareas" $ do
        escribime
    
    describe "el mas apto" $ do
      it "si solo un dragon no termina exhausto tras terminar la mision, ese es el mas apto" $ do
        escribime
      it "si hay varios dragones que terminaron la mision sin estar exhaustos, aquel con mayor nivel de ferocidad es el mas apto" $ do
        escribime


   
escribime :: Expectation
escribime = implementame