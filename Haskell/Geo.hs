module Geometry
  ( sphereVolume
  , sphereArea
  , cubeVolume
  , cubeArea
  , cuboidVolume
  , cuboidArea
  ) where

sphereVolume :: Float -> Float
sphereVolume = (*(4.0/3.0)) . (*pi) . (^3)

sphereArea :: Float -> Float
sphereArea = (*(4 * pi)) . (^2)

cubeVolume :: Float -> Float
cubeVolume = (^3)

cubeArea :: Float -> Float
cubeArea = (*6) . (^2)

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c  b * 2

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
