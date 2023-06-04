module UdGraphic (
    Comanda(..),
    Distancia,
    Angle, 
    execute, 
    display, 
    blau, vermell,
    negre, verd
    )
    where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)
import Data.IORef
import Data.List
import Control.Monad( liftM, liftM2, liftM3 )
import System.Random
import Test.QuickCheck

infixr 5 :#:

-- Punts

data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational

scalar :: Float -> Pnt
scalar x  =  Pnt x x

scalarMin :: Pnt -> Pnt
scalarMin (Pnt x y)  =  scalar (x `min` y)

scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)

dimensions :: Pnt -> (Int,Int)
dimensions (Pnt x y)  =  (ceiling x, ceiling y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')

pointToSize :: Pnt -> Size
pointToSize (Pnt x y) = Size (ceiling x) (ceiling y)

sizeToPoint :: Size -> Pnt
sizeToPoint (Size x y) = Pnt (fromIntegral x) (fromIntegral y)

-- Colors

data Llapis = Color' GL.GLfloat GL.GLfloat GL.GLfloat
            | Transparent
            | Inkless
            deriving (Eq, Ord, Show)

pencilToRGB :: Llapis -> GL.Color3 GL.GLfloat
pencilToRGB (Color' r g b)  =  GL.Color3 r g b
pencilToRGB Transparent  =  error "pencilToRGB: transparent"

blanc, negre, vermell, verd, blau :: Llapis
blanc   = Color' 1.0 1.0 1.0
negre   = Color' 0.0 0.0 0.0
vermell = Color' 1.0 0.0 0.0
verd    = Color' 0.0 1.0 0.0
blau    = Color' 0.0 0.0 1.0

-- Lines

data Ln = Ln Llapis Pnt Pnt
  deriving (Eq,Ord,Show)


-- Window parameters

theCanvas :: Pnt
theCanvas  =  Pnt 800 800

theBGcolor :: GL.Color3 GL.GLfloat
theBGcolor = pencilToRGB blanc



-- Main drawing and window functions

display :: Comanda -> IO ()
display c = do
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= pointToSize theCanvas
  getArgsAndInitialize
  w <- createWindow "pencilcil Graphics"
  displayCallback $= draw c
  reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, x)))
  --actionOnWindowClose $= ContinueExectuion
  draw c
  mainLoop

draw :: Comanda -> IO ()
draw c = do clear [ColorBuffer]
            loadIdentity
            background
            toGraphic $ rescale $ execute c
            swapBuffers

toGraphic :: [Ln] -> IO ()
toGraphic lines  = sequence_ (map f lines)
  where
  f (Ln pencil startP endP)  =
    GL.color (pencilToRGB pencil) >>
    GL.renderPrimitive GL.LineStrip (toVertex startP >> toVertex endP)

background :: IO ()
background = do GL.color theBGcolor
                GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex
                      [GL.Vertex3 (-1) (-1) 0,
                       GL.Vertex3   1  (-1) 0,
                       GL.Vertex3   1    1  0,
                       GL.Vertex3 (-1)   1 (0::GL.GLfloat) ]


toVertex (Pnt x y)  =  GL.vertex $ GL.Vertex3
 (realToFrac x) (realToFrac y) (0::GL.GLfloat)



-- Definició de les comandes per moure el llapis

type Angle     = Float
type Distancia = Float
data Comanda   = Avança Distancia
               | Gira Angle
               | Comanda :#: Comanda
               | Para
               | Branca Comanda
               | CanviaColor Llapis
  deriving (Show,Eq,Ord)

-- Problema 8
-- Pas de comandes a lines a pintar per GL graphics (veure per pantalla les comandes)
-- genera les linies a pintar per UdGraphic
execute :: Comanda -> [Ln]
execute c = execute2 c 0.0 0.0 0                            -- crida a una funció secundaria execute amb la comanda i les inicialitzacions dels punts i l'alngle inicial

-- Funció secunadria 
-- Aquesta funció crea les línies  amb els càlculs dels punts interpol·lats, que mostrarà el display
-- En els següents comenteris quan posem tenim, volem dir com a head de la Comanda
execute2 :: Comanda -> Float -> Float -> Angle-> [Ln]       
execute2 (Gira angle :#: xs) x y angl = execute2 xs x y (angl+angle)                                                                      -- Si tenim un Gira + la resta de la comanda, incrementem l'angle 
execute2 (CanviaColor color :#: Avança dist :#: xs) x y angl =  let xNou =(x+(dist*cos(degToRad (-angl))))                                -- Si tenim un Canvi de color i un avança (i la resta de la comanda) aixo ens indica que hem de canviar el color del llapis i avançar la linia segons l'angle que teniem
                                                                    yNou = (y+(dist*sin(degToRad ((-angl)))))                             -- Calculem la x i la y nova dels punts interpol·lats
                                                                in  [Ln (color) (Pnt x y) (Pnt xNou yNou)] ++ execute2 xs xNou yNou angl  -- Creació de la linia amb el color, el punt anterior i el següent punt (novament calculat) i apliquem execute a la resta de la comanda amb els nou calculs

execute2 (Avança dist :#: xs) x y angl =  let xNou =(x+(dist*cos(degToRad (-angl))))                                                      -- Si tenim un Avança, una distancia i la resta de la comanda 
                                              yNou = (y+(dist*sin(degToRad ((-angl)))))                                                   -- Calculem la x i la y nova dels punts interpol·lats
                                          in  [Ln (negre) (Pnt x y) (Pnt xNou yNou)] ++ execute2 xs xNou yNou angl                        -- Creació de la linia amb el color (negre/default), el punt anterior i el següent punt (novament calculat) i apliquem execute a la resta de la comanda amb els nou calculs

execute2 (CanviaColor color :#: Avança dist) x y angl = let xNou =(x+(dist*cos(degToRad (-angl))))                                        -- Si tenim un Canvi de color i un avança aixo ens indica que hem de canviar el color del llapis i avançar la linia segons l'angle que teniem
                                                            yNou = (y+(dist*sin(degToRad ((-angl)))))                                     -- Calculem la x i la y nova dels punts interpol·lats
                                                        in  [Ln (color) (Pnt x y) (Pnt xNou yNou)]                                        -- Creació de la linia amb el color, el punt anterior i el següent punt (novament calculat) i apliquem execute a la resta de la comanda amb els nou calculs

execute2 (Avança dist) x y angl = let xNou =(x+(dist*cos(degToRad (-angl))))                                                              -- Si tenim un Avança només
                                      yNou = (y+(dist*sin(degToRad ((-angl)))))                                                           -- Calculem la x i la y nova dels punts interpol·lats
                                  in  [Ln (negre) (Pnt x y) (Pnt xNou yNou)]                                                              -- Creació de la linia amb el color (negre/default), el punt anterior i el següent punt (novament calculat) i apliquem execute a la resta de la comanda amb els nou calculs
                                  
execute2 (Branca c :#: xs) x y angl = (execute2 c x y angl) ++ (execute2 xs x y angl)                                                     -- Si tenim una Branca com a comanda significa que a dins tindrem mes comandes per lo tant fem una crida recursiva de l'execute2 amb la comanda de l'interior de la branca (on s'aniran modificant els valors) i amb els punts originals també els passem a la resta de la comanda
execute2 (Branca c) x y angl = execute2 c x y angl                                                                                        -- Si nomes tenim Brnca sola, farem la crida recursiva a la comanda interna de la Branca amb els mateixos valors
execute2 (Para :#: xs) x y angl = execute2 xs x y angl                                                                                    -- Si tenim un Para i la resta de la comanda, ignorarem el Para i aplicarem la recursió a la resta de la comanda "com si no hi fos"
execute2 ((c1 :#: c2) :#: c3) x y angl =  execute2 (c1 :#: c2 :#: c3) x y angl                                                            -- Si tenim més parèntiesis perquè tenim una comanda dins una altre comanda (varies comandes (Branca)), aplicarem l'execute2 recursivament a les x comandes obviant els parentesisj
execute2 (_) x y angl = []                                                                                                                -- En cas de tenir qualsevol altre cosa, l'execute 2 retornara una llista buida

-- Pels punts d'interpol·lació hem fet servir la seguent expressions aritmètiques:
-- x' = x1 + r * cos(α + θ)
-- y' = y1 + r * sin(α + θ)

-- Funció que ens permet passar els radiants en format graus
degToRad :: Float -> Float                                                                                                                
degToRad degrees = degrees * pi / 180.0                                                                                                   -- un cop ens han passat els radiants apliquem la formula per passar a graus i retornem els graus en tipus Float                                                                                               -- 

-- Rescales all points in a list of lines
--  from an arbitrary scale
--  to (-1.-1) - (1.1)

rescale :: [Ln] -> [Ln]
rescale lines | points == [] = []
              | otherwise    = map f lines
  where
  f (Ln pencil p q)  =  Ln pencil (g p) (g q)
  g p             =  swap ((p - p0) / s)
  points          =  [ r | Ln pencil p q <- lines, r <- [p, q] ]
  hi              =  foldr1 lub points
  lo              =  foldr1 glb points
  s               =  scalarMax (hi - lo) * scalar (0.55)
  p0              =  (hi + lo) * scalar (0.5)
  swap (Pnt x y)  =  Pnt y x


-- Generators for QuickCheck

instance Arbitrary Llapis where
    arbitrary  =  sized pencil
        where
          pencil n  =  elements [negre,vermell,verd,blau,blanc,Transparent]


instance Arbitrary Comanda where
    arbitrary  =  sized cmd
        where
          cmd n  |  n <= 0     =  oneof [liftM (Avança . abs) arbitrary,
                                         liftM Gira arbitrary ]
                 |  otherwise  =  liftM2 (:#:) (cmd (n `div` 2)) (cmd (n `div`2))
