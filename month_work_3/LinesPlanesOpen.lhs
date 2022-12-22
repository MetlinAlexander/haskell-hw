Именно эти задания подлежат проверке и обязательному исполнению в 2022 году


\begin{code}

module LinesPlanesOpen where


import AnGeo
import Lines
import LinesPlanes
\end{code}

плоскости для тестов
\begin{code}
--одинаковые плоскости в двух разных видах
cp_1 = CPl 1.0 2.0 3.0 4.0
cp_2 = CPl 1 6 5 4
cp_3 = CPl 1 2 3 12


pl_1 = Pl  (Vc 5.0 (-6.0) 1) (normalForCPlane cp_1)
pl_2 = Pl (Vc (-4) (-4) (-4)) (normalForCPlane cp_1)
pl_3 = cplaneToPlane cp_2
pl_4 = cplaneToPlane cp_1
--

\end{code}

Преобразование типов плоскостей:
\begin{code}
--planeToCPlane pl_1
planeToCPlane :: Plane -> CPlane
planeToCPlane (Pl (Vc x y z) (Vc a b c)) = CPl (a) (b) (c) ( -(x*a+b*y+z*c) )

--cplaneToPlane cp_1
cplaneToPlane :: CPlane -> Plane
cplaneToPlane (CPl a b c d) = Pl (radius a b c d) (Vc a b c)
                      where radius aa bb cc dd  | aa/=0 = (Vc (-dd/aa) 0 0)
                                                | bb/=0 = (Vc (-dd/bb) 0 0)
                                                | cc/=0 = (Vc (-dd/cc) 0 0)

\end{code}

Красивое отображение канонической плоскости в виде уравнения (Ax + By + Cz + D = 0):
\begin{code}
instance Show CPlane where
  show (CPl a b c d) = ( if a /= 0 then ((show a) ++ "x") else ""  )
                        ++ ( if b /= 0 then ( (if b>0 then " + " else " - ") ++ (show (abs b)) ++ "y")  else "" )
                        ++ ( if c /= 0 then ( (if c>0 then " + " else " - ") ++ (show (abs c)) ++ "z")  else "" )
                        ++ ( if d /= 0 then ( (if d>0 then " + " else " - ") ++ (show (abs d)))  else "" )
                        ++ " = 0"

--tests
print_test_1 = CPl 0 2.0 3.0 (-1)
print_test_2 = CPl 0 0 3.0 (-1)
print_test_3 = CPl (-1) (-2) (-3) (1)
print_test_4 = CPl 1 2.0 3.0 0
\end{code}

отображение плоскости в каком-либо приемлемом виде :

\begin{code}
instance Show Plane where
  show (Pl (Vc x y z) (Vc a b c)) = ("radius vector: " ++ show(Vc x y z) ++ ":  vector normali: " ++ show(Vc a b c) ++ "  ~  ") 
                                  ++ ( show (planeToCPlane (Pl (Vc x y z) (Vc a b c))) )
--tests
--pl_1
--pl_2
\end{code}


Проверка принадлежности точки плоскости (в обеих формах)

\begin{code}
pointOnPlane :: Point -> Plane -> Bool
pointOnPlane point' plane'= pointOnCPlane point' (planeToCPlane plane')
--pointOnPlane test_point_1 pl_1   -> True
--pointOnPlane test_point_1 pl_2   -> False


pointOnCPlane :: Point -> CPlane -> Bool
pointOnCPlane (Pt xx yy zz) (CPl a b c d) = (  (xx*a + yy*b + zz*c +d) == 0  )
--tests
test_point_1 = (Pt (-4) 0 0)
test_point_2 = (Pt (-20) 30 1)
--pointOnCPlane test_point_1 cp_1   -> True
--pointOnCPlane test_point_2 cp_1   -> False

\end{code}

Проверка принадлежности прямой плоскости

\begin{code}
lineOnPlane  :: Line -> Plane -> Bool
lineOnPlane line' plane' = lineOnCPlane line' (planeToCPlane  plane')
--tests
--lineOnPlane test_line_1 pl_1 -> True
--lineOnPlane test_line_1 pl_2 -> False

lineOnCPlane :: Line -> CPlane -> Bool
lineOnCPlane (Ln (Vc x1 y1 z1) (Vc x2 y2 z2)) (CPl a b c d) = ( (x1*a + y1*b +z1*c +d) == 0 && (x2*a + y2*b +z2*c +d) == 0)
--tests
--lineOnCPlane test_line_1 cp_1 -> True
--lineOnCPlane test_line_2 cp_1 -> False
test_line_1 = Ln (Vc (-4) 0 0) (Vc 5.0 (-6.0) 1) 
test_line_2 = Ln (Vc 0 0 0) (Vc 5.0 (-6.0) 1) 

\end{code}

Проверка совпадения двух плоскостей

\begin{code}
instance Eq Plane where
            plane_1 == plane_2 = (  (normal plane_1) `coll` (normal plane_2)  ) && ( pointOnPlane (toPoint(mo plane_1)) plane_1 ) && ( pointOnPlane (toPoint(mo plane_2)) plane_2 )

instance Eq CPlane where
            plane_1 == plane_2 = compare_Cplane plane_1 plane_2
--tests
-- pl_1 == pl_3 -> False
-- pl_1 == pl_4 -> True
-- cp_1 == cp_3 -> False
-- cp_4 == cp_5 -> True
\end{code}

упрощает 2 уравнение плоскости до состояния, что один из A B C становиться равен 1
\begin{code}
cp_4 = CPl 0 4 6 8
cp_5 = CPl 0 8 12 16

simple_Cplane :: CPlane -> CPlane -> [Double]
simple_Cplane (CPl a1 b1 c1 d1) (CPl a2 b2 c2 d2) | ((a1/=0) && ( a2/=0)) = [(1),(b1/a1),(c1/a1) , (d1/a1)]
                                                  | ((b1/=0) && ( b2/=0)) = [(a1/b1), (1) ,(c1/b1) , (d1/b1)]
                                                  | ((c1/=0) && ( c2/=0)) = [(a1/c1),(b1/c1), (1) , (d1/c1)]
                                                  | otherwise = [a1, b1, c1 , d1]
--mult_Cplane cp_1 4

--compare two cplanes
compare_Cplane :: CPlane -> CPlane -> Bool
compare_Cplane plane_1 plane_2 = (simple_Cplane plane_1 plane_2) == (simple_Cplane plane_2 plane_1)

\end{code}


---------------------------------



Проверка параллельности двух плоскостей

\begin{code}
planeParall :: Plane -> Plane -> Bool
planeParall plane_1 plane_2 = (normal plane_1) `coll` (normal plane_2)
-- planeParall pl_1 pl_2 -> True
-- planeParall pl_1 pl_3 -> False

cplaneParall :: CPlane -> CPlane -> Bool
cplaneParall plane_1 plane_2 = planeParall (cplaneToPlane plane_1) (cplaneToPlane plane_2)
--cplaneParall cp_1 cp_3 -> True
--cplaneParall cp_1 cp_2 -> False

\end{code}

Проверка перпендикулярности двух плоскостей

\begin{code}
planePerp :: Plane -> Plane -> Bool
planePerp p1 p2 = (normal p1) `perp` (normal p2)
-- planePerp pl_1 pl_2 -> False

\end{code}

\begin{code}
cplanePerp :: CPlane -> CPlane -> Bool
cplanePerp p1 p2 = planePerp (cplaneToPlane p1) (cplaneToPlane p2)
--cplanePerp cp_1 cp_1 -> False
\end{code}

Проверка параллельности прямой и плоскости

\begin{code}
lineAndPlaneParall :: Line -> Plane -> Bool
lineAndPlaneParall line plane = (dir line) ┴ (normal plane)
--lineAndPlaneParall test_line_4 pl_3 -> True
--lineAndPlaneParall test_line_4 pl_1 -> False
test_line_4 = lineFrom2Points (Pt 1 (-2) 2) (Pt 3 1 (-2))
\end{code}

\begin{code}
lineAndCPlaneParall :: Line -> CPlane -> Bool
lineAndCPlaneParall line plane = lineAndPlaneParall (line) (cplaneToPlane plane)
--lineAndCPlaneParall test_line_4 cp_2 -> True
--lineAndCPlaneParall test_line_1 cp_2 -> False
\end{code}

Проверка перпендикулярности прямой и плоскости

\begin{code}
linePlanePerp  :: Line -> Plane -> Bool
linePlanePerp ln plane = (dir ln) `coll` (normal plane)

lineCPlanePerp :: Line -> CPlane -> Bool
lineCPlanePerp ln plane = linePlanePerp (ln) (cplaneToPlane plane)

\end{code}

Нахождение угла между плоскостями (в градусах бы)...

\begin{code}
planeAngle  :: Plane -> Plane  -> Double
planeAngle plane_1 plane_2 = cplaneAngle (planeToCPlane plane_1) (planeToCPlane plane_2)
-- planeAngle pl_1 pl_2 -> 0.0
--planeAngle pl_2 pl_3 -> 18.133853532378307

cplaneAngle :: CPlane -> CPlane  -> Double
cplaneAngle (CPl a1 b1 c1 d1) (CPl a2 b2 c2 d2) = ((acos (   ( abs( a1*a2 + b1*b2 + c1*c2 ) ) 
                                                  / (  ( ( a1**2 + b1**2 + c1**2 )**0.5 ) * ( (a2**2 + b2**2 + c2**2)**0.5 ) ) )) 
                                                  *180) / 3.14
--cplaneAngle cp_1 cp_2 -> 18.133853532378307
--cplaneAngle cp_1 cp_3 -> 0.0
\end{code}

Нахождение угла между прямой и плоскостью (в градусах бы)...

\begin{code}
lineAndPlaneAngle ::  Line -> Plane  -> Double
lineAndPlaneAngle ln plane' = lineAndCPlaneAngle (ln) (planeToCPlane plane')
--lineAndPlaneAngle test_line_4 pl_1 -> 11.456038860090878

lineAndCPlaneAngle :: Line -> CPlane  -> Double
lineAndCPlaneAngle (Ln (Vc _ _ _) (Vc a1 b1 c1) ) (CPl a b c d) = ((asin (   ( abs( a1*a + b1*b + c1*c ) ) 
                                                  / (  ( ( a1**2 + b1**2 + c1**2 )**0.5 ) * ( (a**2 + b**2 + c**2)**0.5 ) ) )) 
                                                  *180) / 3.14

--lineAndCPlaneAngle test_line_4 cp_1 -> 11.456038860090878
--lineAndCPlaneAngle test_line_4 cp_2 -> 0
\end{code}

Нахождение расстояния между точкой и плоскостью

\begin{code}
pointToPLaneDistance :: Point -> Plane -> Double
pointToPLaneDistance point' plane' = pointToCPLaneDistance point' (planeToCPlane plane')
--pointToPLaneDistance test_point_3 pl_1 -> 13.09580085370879

pointToCPLaneDistance :: Point -> CPlane -> Double
pointToCPLaneDistance (Pt x y z) (CPl a b c d) = (abs(a*x + b*y + c*z + d) )/ ( (a**2 +b**2 +c**2)**0.5 )
--pointToCPLaneDistance test_point_3 cp_1 -> 13.095800853708795
test_point_3 = (Pt 4 7 9)
\end{code}

Нахождение линии пересечения двух плоскостей


\begin{code}

lineIntersectionOf2Planes :: Plane -> Plane -> Line
lineIntersectionOf2Planes plane_1 plane_2 = lineIntersectionOf2CPlanes (planeToCPlane plane_1) (planeToCPlane plane_2)

--lineIntersectionOf2Planes pl_1 pl_3 -> (-2.0; -0.5; 1.0)(-8.0; -2.0; 4.0)

lineIntersectionOf2CPlanes :: CPlane -> CPlane -> Line
lineIntersectionOf2CPlanes plane_1@(CPl a1 b1 c1 d1) plane_2@(CPl a2 b2 c2 d2) = 
                                      Ln  ((normalForCPlane (mult_Cplane plane_1 0.5) ) `vprod` (normalForCPlane (mult_Cplane plane_2 0.5)))    ((normalForCPlane plane_1) `vprod` (normalForCPlane plane_2)) 
--lineIntersectionOf2CPlanes cp_1 cp_2 -> (-2.0; -0.5; 1.0)(-8.0; -2.0; 4.0)

\end{code}
--

my function to multiply a b c d in CPlane
\begin{code}
mult_Cplane :: CPlane -> Double -> CPlane
mult_Cplane (CPl a b c d) kf = CPl (a*kf) (b*kf) (c*kf) (d*kf)
--mult_Cplane cp_1 4
\end{code}

