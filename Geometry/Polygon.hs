module Geometry.Polygon
  ( Polygon(..)
  , polygon
  , hexagon
  , moveTo
  )
where


import Geometry.Point (Point)

import Data.List (intercalate)


newtype Polygon = Polygon { sides :: [Point] }


instance Show Polygon where
  show (Polygon v) = intercalate " " $ show . Point' <$> v


newtype Point' = Point' Point


instance Show Point' where
  show (Point' (x,y)) = intercalate "," $ show <$> [x,y]


polygon ::  Int -> Float -> Maybe Polygon
polygon 0 _ = Nothing
polygon _ 0 = Nothing
polygon n r = Just $ Polygon $ p <$> (α *) <$> [1.0..n']
  where
    p α' = (sin(α') * r, cos(α') * r)
    α    =  2 * pi / n'
    n'   =  fromIntegral n


hexagon :: Float -> Maybe Polygon
hexagon = polygon 6


moveTo :: Point -> Polygon -> Polygon
moveTo (x,y) (Polygon v) = Polygon $ (\(p,q) -> (x+p,y+q)) <$> v
