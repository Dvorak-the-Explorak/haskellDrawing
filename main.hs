import Graphics.Gloss
import Debug.Trace

window :: Display
window = InWindow "Nice Window" (800,800) (100,100)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

differenceVector :: Point -> Point -> Point
differenceVector (x1,y1) (x2,y2) = (x2-x1, y2-y1)

interpolate :: Float -> Point -> Point -> Point
interpolate 0 p1 _ = p1
interpolate 1 _ p2 = p2
interpolate x (x1,y1) (x2,y2) = (x1 + x*(x2-x1), y1 + x*(y2-y1))


cartToPolar :: Vector -> (Float, Float)
cartToPolar v = (norm v, angle v)
  where 
    norm (x,y) = sqrt $ x*x + y*y
    angle (x,y) = 180.0 / pi * (atan2 y x)


-- #TODO this doesn't need to be split into 2 functions, i
--    and it might be easier without having to map all the time
kochLine :: Int -> Point -> Point -> Picture
kochLine n p1 p2 = translate (fst p1) (snd p1) $ rotate (-angle) $ scale size size $ Pictures $ kochLine' n
  where
    (size,angle) = cartToPolar $ differenceVector p1 p2

kochLine' :: Int -> [Picture]
kochLine' 0 = [Blank]
kochLine' 1 = [Line [(0,0), (1,0)]]
kochLine' n = part1 ++ part2 ++ part3 ++ part4
  where
    subLine = map (scale (1.0/3) (1.0/3)) $ kochLine' (n-1)
    xshift = translate (1.0/3) 0
    xshift2 = translate (2.0/3) 0
    part1 = subLine

    part2 = map xshift $ map (rotate (-60)) subLine
    part3 = map ( xshift . rotate (-60)  . xshift . rotate 120) $ subLine
    part4 = map xshift2 subLine



kochSnowflake :: Int -> Point -> Float -> Picture
kochSnowflake n (x,y) s = Pictures [base, left, right]
  where
    base = kochLine n (x+s/2.0,y) (x-s/2.0, y)
    left = kochLine n (x-s/2.0, y) (x, y + s*(sqrt 3)/2)
    right = kochLine n (x, y + s*(sqrt 3)/2) (x+s/2.0,y)



-- main :: IO ()
-- main = display window background drawing

main :: IO ()
-- main = display window background $ kochLine 7 (0,0) (200,200)
main = display window background $ kochSnowflake 7 (0,0) 200