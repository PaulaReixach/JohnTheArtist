module Artist where

import UdGraphic
import Test.QuickCheck
import Debug.Trace

-- Problema 1
-- Transforma una comanda a una llista de comandas que no compté :#: ni el constructor Para.
separa :: Comanda -> [Comanda] 
separa (x :#: xs) = separa x ++ separa xs                                                       -- Aplica el separa a una comanda i la concatena amb el separa de la seguent/s comanda/es
separa Para = []                                                                                -- Si la comanda es Para retorna una llista buida
separa comanda = [comanda]                                                                      -- Si només hi hi ha una comanda ho passa a una llista amb una sola comanda


-- Problema 2
-- Transforma una llista de comandes a una sola comanda
ajunta :: [Comanda] -> Comanda
ajunta [] = Para                                                                                -- Ajunta si troba la lista buida ens crea la comanda Para 
ajunta [x] = x :#: Para                                                                         -- Si nomes hi ha un element a la llista crea la comanda composta x :#: Para 
ajunta (x:xs) = x :#: ajunta xs                                                                 -- Agafa el primer element de la llista, el posa com a element de la primera comanda i recusivament fa el mateix concatenent amb el l'operador d'unió de les comandes, la següent comanda


-- Problema 3
-- Han de retornar la mateixa llista donada dues comandes siguin equivalents (escrites de diferent manera)
prop_equivalent :: Comanda -> Comanda -> Bool
prop_equivalent com1 com2 = separa com1 == separa com2                                          -- Es separa la primera comanda i es fa el mateix amb la segona comanda passada, i retorna true si les dues llistes són iguals

-- Ha de mirar que ajunta (separa c) es equivalent a c, on c es una comanda qualsevol
prop_split_join :: Comanda -> Bool
prop_split_join c = ajunta (separa c) == c                                                      -- Separem i ajuntem la comanda passada i comprovem que sigui igual a la que ens han donat

-- Aquesta comanda mira que desprès de fer el separa no hi hagi cap comanda composta ni el Para, si és aixì retorna true
-- All és una funció d'ordre superior que agafa un predicat i una llista i retorna true si el predicat es cert per a tots els elements de la llista i fals en cas contrari, 
-- És a dir en aquest cas s'utilitza per verificar que totes les subcomandes son simples i no estan unides per :#: no són la comanda Para
prop_split :: Comanda -> Bool
prop_split c = all isSimple $ separa c                                                          -- Separa divideix la comanda, en subcomandes (llista) i verifica mitjançant la funció isSimple que totes les comandes siguin simples i no n'hi hagi cap de composta ni cap que sigui Para
    where isSimple Para = False                                                                 -- Comprova que la comanda que s'està comprovant no sigui Para
          isSimple (com1 :#: com2) = False                                                      -- Comprova que la comanda que s'està comprovant no sigui una comanda composta
          isSimple _ = True                                                                     -- En cas que no sigui qualsevol de les anteriors, vol dir que és simple, retorna true


-- Problema 4
-- Donat un número n i una comanda, generi una nova comanda amb el nombre n de copies de la comanda
copia :: Int -> Comanda -> Comanda
copia 1 x = x                                                                                   -- Quan el número de còpies es 1 mostrem, la comanda
copia n x = x :#: copia (n-1) x                                                                 -- Quan el número de còpies és > 1 mostrem la comanda, la concatenem amb l'operador de comandes i apliquem recursivament còpia a la comanda


-- Problema 5
-- Retorna la comanda que genera un pentagon, tots els costats del pentagon tenen la mateixa mida
pentagon :: Distancia -> Comanda
pentagon d = copia 5 (Avança d :#: Gira 72.0)                                                   -- Fent ús de la comanda copia, aquesta replica 5 vegades el la comanda (Avança d :#: Gira 72.0) amb la distància desitjada


-- Problema 6
-- Aquest problema el que fa és generar la comanda que fa que el llapis traci una ruta amb el nombre de costats especificat, la longitud especificada i l’angle especificat
-- La funció concat uneix varies llistes en una sola llista. 
-- En aquest cas, concat unirà les subllistes de comandes retornades pel map en una sola llista
poligon :: Distancia -> Int -> Angle -> Comanda
poligon d x a = ajunta2 $ concat $ map (replicate x) [Avança d :#: Gira a]                      -- Amb el map el que fem és usar la funció replaicate per aplicar-ho a la llista on hi tenim la comanda a 
                                                                                                -- Replicar i aquesta és replicada i concatenada amb la resta de comandes replicades i al final les ajuntem
                                                                                                -- A una sola comanda fent ús de la comnda ajunta

-- S'encarrega de comprovar que la funció anterior funciona correctament, és a dir, ens crea l'estructura de manera correcte i això ho comprovem (amb pentagon) mirant si les dues comandes són iguals
prop_poligon_pentagon :: Distancia -> Int -> Angle -> Distancia -> Bool
prop_poligon_pentagon distPo num angle distPe  = poligon distPo num angle == pentagon distPe    -- Generem les dues comandes, una amb poligon i l'altre amb pentagon i comprovem l'igualtat


-- FUNCIÓ AUXILIAR EXTRA SENSE PARA
-- transforma una llista de comandes a una sola comanda
ajunta2 :: [Comanda] -> Comanda
ajunta2 [x] = x                                                                     -- Si nomes hi ha un element a la llista crea la comanda composta per x
ajunta2 (x:xs) = x :#: ajunta2 xs                                                   -- Agafa el primer element de la llista, el posa com a element de la primera comanda i recusivament fa el mateix concatenent amb el l'operador d'unió de les comandes, la següent comanda


-- Problema 7
-- Crea un espiral, fent que el llapis viatgi a distàncies cada vegada més llargues (o més curtes) i girant una mica entre elles
espiral :: Distancia -> Int -> Distancia -> Angle -> Comanda
espiral dist 1 sum angle = Avança dist :#: Gira angle                                           -- Quan el número de vegades recursives és 1 mostrem, la comanda composta
espiral dist n sum angle = Avança dist :#: Gira angle :#: espiral (dist+sum) (n-1) sum angle    -- Quan el número de recursions és > 1 mostrem la comanda composta  la concatenem amb l'operador de comandes i 
                                                                                                -- Apliquem recursivament espiral a la comanda però modificant els paràmetres, és a dir,
                                                                                                -- Incrementem la distància i decrementem el número de vegades de la recurssió


-- Problema 9
-- Donada una comanda, retorna una comanda que dibuixa la mateixa imatge, pero de manera optimitzada 
optimitza :: Comanda -> Comanda
optimitza comanda = optimitza1 $ separa $ comanda           -- crida a la funció secundària, fent ús del separa per poder passar-li una llista de comandes com a paràmetre

optimitza1 :: [Comanda] -> Comanda                          -- Retorna la Comanda final de la simplificació que es va fent amb l'acomulació del foldr        
optimitza1 comanda = foldr funcio Para comanda              -- usem el foldr amb una funcio creada per nosaltres que es diu funcio (definieix varies regles per combinar i simplifpicar el resultat), l'inicialitzem amb el Para
  where
    funcio x Para = x                                       -- Si el segon element es un Para, retorna el primer element
    funcio (Gira 0) (Gira 0) = Para                         -- Si els dos elements són Gira 0, ho tranforma amb un Para
    funcio x (Gira 0) = x                                   -- Si tenim dos elements i el segon es un Para, retorna el primer element
    funcio x (Gira 0 :#: comandes) = (x :#: comandes)       -- Si tenim el primer element i el segon element es un Gira 0 seguit de més comanda/es retorna l'element amb el seguit de comandes
    funcio (Gira 0) x = x                                   -- Si el primer element es un Gira 0, retornem el segon element 
    funcio (Avança 0) (Avança 0) = Para                     -- Si els dos elements són Avança 0, ho tranforma amb un Para
    funcio x (Avança 0) = x                                 -- Si tenim dos elements i el segon es un Para, retorna el primer element
    funcio x (Avança 0 :#: comandes) = (x :#: comandes)     -- Si tenim el primer element i el segon element es un Avança 0 seguit de més comanda/es retorna l'element amb el seguit de comandes
    funcio (Avança 0) x = x                                 -- Si el primer element es un Avança 0, retornem el segon element
    funcio (Avança d) (Avança d1)                           -- Els dos elements són Avança amb diferents números 
      | (d+d1) == 0 = Para                                  -- Si la suma d'aquests dona 0, ho convertim amb un Para
      | otherwise = Avança (d+d1)                           -- En qualsevola altre cas, retornem Avança amb la suma dels dos elements
    funcio (Avança d) (Avança d1 :#: x)                     -- Si el primer element és un Avança i en el segon element tenim un altre Avança i la resta de la comanda 
      | (d+d1) == 0 = x                                     -- Si la suma d'aquests dona 0, retornem la resta de les comandes
      | otherwise = (Avança (d+d1) :#: x)                   -- En qualsevola altre cas, retornem Avança amb la suma dels dos elements + la resta de les comandes
    funcio (Gira g) (Gira g1)                               -- Els dos elements són Gira amb diferents números
      | (g+g1) == 0 = Para                                  -- Si la suma d'aquests dona 0, ho convertim amb un Para
      |otherwise = Gira (g+g1)                              -- En qualsevola altre cas, retornem Gira amb la suma dels dos elements
    funcio (Gira g) (Gira g1 :#: x)                         -- Si el primer element és un Gira i en el segon element tenim un altre Gira i la resta de la comanda
      | (g+g1) == 0 = x                                     -- Si la suma d'aquests dona 0, retornem la resta de les comandes
      | otherwise = (Gira (g+g1) :#: x )                    -- En qualsevola altre cas, retornem Gira amb la suma dels dos elements + la resta de les comandes
    funcio x (Gira g) = (Gira g :#: x)                      -- Si el primer element es un element qualsevol, i el segon element es un Gira retorna Gira + l'element
    funcio x (Avança d) = (Avança d :#: x)                  -- Si el primer element es un element qualsevol, i el segon element es un Avança retorna Gira + l'element



-- Problema 10
-- Retorna la comanda que representa un triangle, a través de la gramàtica establerta
triangle :: Int -> Comanda
triangle n = giraPos :#: f n                                                        -- Defineix la implementació, comença amb un gira positiu, i crida la funció f amb l'argument n
  where giraPos = Gira 90                                                           -- Definició dels dos Gira, amb 90 graus positius, i 90 graus negatius
        giraNeg = Gira $ -90
        f 0 = Avança 10                                                             -- El cas base de f, quan n sigui 0, es realitzarà un avança 10
        f n = f (n-1) :#: giraPos :#: f (n-1) :#: giraNeg :#: f (n-1) :#: giraNeg :#: f (n-1) :#: giraPos :#: f(n-1)  -- La regla recursiva de la funció f, sempre que n sigui > 0.	   
                                                                                    -- Aquesta regla, només utilitza la recursivitat, i els gira negatius i positius
        

-- Problema 11
-- Retorna la comanda que representa una fulla a través de la gramàtica establerta 
fulla :: Int -> Comanda
fulla n = f n                                                                        -- Defineix la implementació, crida la funció f amb l'argument n
  where giraPos = Gira 45                                                            -- Definició dels dos Gira, amb 45 graus positius, i 45 graus negatius
        giraNeg = Gira $ -45
        f 0 = CanviaColor vermell :#: Avança 5                                       -- El cas base de f, quan n sigui 0, es farà un canvi de color a vermell, i un avança 5
        f n = g (n-1) :#: Branca (giraNeg :#: f (n-1))                               -- La regla recursiva de la funció f, sempre que n sigui > 0.	   
                      :#: Branca (giraPos :#: f (n-1)) 
                      :#: Branca (g (n-1) :#: f (n-1))                               -- Aquesta regla, només utilitza la recursivitat, el branca, els gira negatius i positius, l'ús de la funció g
        g 0 = CanviaColor blau :#: Avança 5                                          -- El cas base de g, quan n sigui 0, es farà un canvi de color a blau, i un avança 5
        g n = g (n-1) :#: g (n-1)                                                    -- La regla recursiva de la funció g, sempre que n sigui > 0.	   
                                                                                     -- Aquesta regla, només utilitza la recursivitat, i la concatenació
 
-- Problema 12
-- Retorna la comanda que representa un tipus de cuadrats a través de la gramàtica establerta 
hilbert :: Int -> Comanda
hilbert n = Gira 90 :#: l n                                                           -- Defineix la implementació, comença amb un Gira 90, i crida la funció l amb l'argument n
  where giraPos = Gira 90                                                             -- Definició dels dos Gira, amb 90 graus positius, i 90 graus negatius
        giraNeg = Gira $ -90
        l 0 = Para                                                                    -- El cas base de l, quan n sigui 0, es realitzarà un Para
        l n = giraPos :#: r (n-1) :#: f :#: giraNeg :#: l (n-1) :#: f :#: l (n-1) :#: giraNeg :#: f :#: r (n-1) :#: giraPos       -- La regla recursiva de la funció l, sempre que n sigui > 0
                                                                                                                                  -- Aquesta regla, només utilitza la recursivitat, els gira positius, l'ús de la funció r i l'ús de f, que representa un avança 10
        r 0 = Para                                                                                                                -- El cas base de r, quan n sigui 0, es realitzarà un Para																						
        r n = giraNeg :#: l (n-1) :#: f :#: giraPos :#: r (n-1) :#: f :#: r (n-1) :#: giraPos :#: f :#: l (n-1) :#: giraNeg       -- La regla recursiva de la funció r, sempre que n sigui > 0
                                                                                                                                  -- Aquesta regla, només utilitza la recursivitat, els gira positius, l'ús de la funció l i l'ús de f, que representa un avança 10
        f = Avança 10                                                                 -- Defineix f com Avança 10

-- Problema 13
-- Retorna la comanda que representa una fletxa a través de la gramàtica establerta 
fletxa :: Int -> Comanda
fletxa n = Gira 90 :#: f n                                                            -- Defineix la implementació, comença amb un Gira 90, i crida la funció f amb l'argument n
  where giraPos = Gira 60                                                             -- Definició dels dos Gira, amb 60 graus positius, i 60 graus negatius
        giraNeg = Gira $ -60
        f 0 = Avança 5                                                                -- El cas base de f, quan n sigui 0, es realitzarà un Avança 5
        f n = g (n-1) :#: giraPos :#: f(n-1) :#: giraPos :#: g (n-1)                  -- La regla recursiva de la funció f, sempre que n sigui > 0
                                                                                      -- Aquesta regla, només utilitza la recursivitat, els gira positius i l'ús de la funció g
        g 0 = Avança 5                                                                -- El cas base de g, quan n sigui 0, es realitzarà un Avança 5
        g n = f (n-1) :#: giraNeg :#: g (n-1) :#: giraNeg :#: f (n-1)                 -- La regla recursiva de la funció g, sempre que n sigui > 0
                                                                                      -- Aquesta regla, només utilitza la recursivitat, els gira negatius i l'ús de la funció f 

-- Problema 14
--Retorna la comanda que representa una branca a través de la gramàtica establerta 
branca :: Int -> Comanda
branca n = g n                                                                        -- Defineix la implementació, crida la funció g amb l'argument n
  where giraPos = Gira 22.5                                                           -- Definició dels dos Gira, amb 22.5 graus positius i 22.5 graus negatius
        giraNeg = Gira $ -22.5
        g 0 = CanviaColor verd :#: Avança 10                                          -- El cas base de g, quan n sigui 0, es canviarà de color a verd, i es realitzarà un Avança 10
        g n = f (n-1) :#: giraNeg :#: Branca (Branca (g (n-1)) :#: giraPos :#: g (n-1)) :#: giraPos :#: f (n-1) 
                                  :#: Branca (giraPos :#: f (n-1) :#: g (n-1)) :#: giraNeg :#: g (n-1) --La regla recursiva de la funció g, sempre que n sigui > 0.  
                                                                                      -- Aquesta regla, només utilitza la recursivitat, els gira tant positius com negatius, el branca i l'ús de la funció f
        f 0 = Avança 10                                                               -- El cas base de f, quan n sigui 0, es realitzarà un Avança 10
        f n = f (n-1) :#: f(n-1)                                                      -- La regla recursiva de la funció g, sempre que n sigui > 0.  
                                                                                      -- Aqusta regla, nomes utilitza la recursivitat i la concatenació

-- angle: 35
-- inici: f
-- reescriptura: f[+ff][-ff]f[-f][+f]f

-- Retorna la comanda que representa una arbre a través de la gramàtica establerta 
extra :: Int -> Comanda
extra n = f n                                                                         -- Defineix la implementació, crida la funció f amb l'argument n
  where giraPos = Gira 35                                                             -- Definició dels dos Gira, amb 35 graus positius i 35 graus negatius
        giraNeg = Gira $ -35
        f 0 = Avança 10                                                               -- El cas base, quan n sigui 0, es realitzarà un Avança 10
        f n = f (n-1) :#: Branca(giraPos :#: f (n-1) :#: f (n-1)) :#: Branca(giraNeg :#: f (n-1) :#:  f (n-1)) 
                      :#: f (n-1) :#: Branca(giraNeg :#: f (n-1)) :#: Branca(giraPos :#: f(n-1)) :#: f (n-1) 
                                                                                      -- La regla recursiva de la funció f, sempre que n sigui > 0.
                                                                                      -- Aquesta regla, utilitza tant la recursivitat, Branca, i tant GiraPos com GiraNeg, segons la reescriptura establerta