-- by helq

import Codec.Picture ( Pixel, Image, readImage, generateImage, pixelAt
                     , imageHeight , imageWidth, DynamicImage(ImageRGB8)
                     , generateImage , writePng)
import Codec.Picture.Types (PixelRGB8(..))
import Data.Fixed (mod')

planeProjection lat lon scale x y = cartesianTo_LatLon (vx4,vy4,vz4)
    -- from latitud-longitud system to spherical coordinate system
    -- r     = 1
    -- theta = pi/2 - lat
    -- phi   = lon
    where -- vector 1 (in lat-lon system)   v1 = [x1, y1, z1]
          vx1 = cos lat * cos lon
          vy1 = cos lat * sin lon
          vz1 = sin lat
          -- vector 2                       v2 = [x2, y2, z2]
          vx2 = cos (lon + pi/2)
          vy2 = sin (lon + pi/2)
          vz2 = 0
          -- vector 3                       v3 = [x3, y3, z3]
          vx3 = cos (lat + pi/2) * cos lon
          vy3 = cos (lat + pi/2) * sin lon
          vz3 = sin (lat + pi/2)
          -- vector 4                       v = v1 + (x*v2 + y*v3) * scale
          x' = fromIntegral x
          y' = fromIntegral y
          vx4 = vx1 + (x'*vx2 + y'*vx3) * scale
          vy4 = vy1 + (x'*vy2 + y'*vy3) * scale
          vz4 = vz1 + (x'*vz2 + y'*vz3) * scale

latLonToCartesian (lat,lon) = (x,y,z)
    where x = cos lat * cos lon
          y = cos lat * sin lon
          z = sin lat

equirectangularProjection :: (Pixel a, Floating b, Real b, Enum b) =>
                                Image a -> Int -> Int -> b -> b -> a
equirectangularProjection rawMap m n lat lon = pixelAt rawMap x y
    where (lat',lon') = normalize_LatLon (lat,lon)
          x' =     (lon'+pi)/pi     *(fromIntegral n/2)
          y' = (lat'+(pi/2))/(pi/2) *(fromIntegral m/2)
          x  = min (n-1) $ fromEnum x'
          y  = min (m-1) $ fromEnum y'

normalize_LatLon (lat,lon)
    | lat < -pi/2 || lat > pi/2 = newLatLon $ (lat + pi) `mod'` (2*pi) - pi
    | otherwise                 = (lat, normalizeLon lon)
    where newLatLon lat'
              | lat' < -pi/2 = (-pi-lat', normalizeLon $ lon+pi)
              | lat' >  pi/2 = ( pi-lat', normalizeLon $ lon+pi)
              | otherwise    = (    lat', normalizeLon $ lon)
          normalizeLon lon' = (lon' + pi) `mod'` (2*pi) - pi

cartesianTo_LatLon (x,y,z) = (lan, lon)
    where lan = asin $ z / sqrt (x^2 + y^2 + z^2)
          lon = if x /= 0
                then atan (y/x) + (if x>0 then 0 else pi)
                else pi/2

selectPixels pixelFun [] (lat,lon) = pixelFun lat lon
selectPixels pixelFun (f:fs) v
    | f v       = selectPixels pixelFun fs v
    | otherwise = PixelRGB8 0 0 0

planeInequality v1 v2 v3 = pointMul > 0
    where (x1,y1,z1) = latLonToCartesian v1
          (x2,y2,z2) = latLonToCartesian v2
          -- (xP,yP,zP) = v1 `crossProduct` v2 = vP (vector perpendicular Plane)
          (xP,yP,zP) = (y1*z2-z1*y2, x1*z2-z1*x2, x1*y2-y1*x2)

          (x3,y3,z3) = latLonToCartesian v3
          -- pointMul = vP . v3
          pointMul = xP*x3 + yP*y3 + zP*z3

main = do
    putStrLn "Loading image ..."
    Right (ImageRGB8 rawMap) <- readImage inputMap
    let latitude  = 0
        longitude = 20
        zoom      = 0.8
        θ = -latitude*pi/180
        φ = longitude*pi/180
        m         = imageHeight rawMap
        n         = imageWidth  rawMap
        pixelAt' = selectPixels (equirectangularProjection rawMap m n)
                                [planeInequality (θ,-φ) (θ+1,-φ)]

        basicProjection = \x y-> planeProjection θ φ
                                             (1/(zoom*(fromIntegral $ min m' n')))
                                             (x - m' `div` 2)
                                             (y - n' `div` 2)

        newMap = generateImage (\x y->pixelAt' $ basicProjection x y) m' n'
        --newMap = generateImage (pixelAt rawMap) m' n'
    putStrLn "Creating projection ..."
    writePng outputMap newMap

inputMap = "maps/world.topo.bathy.200412.3x5400x2700.png"
--inputMap = "maps/world.topo.bathy.200410.3x21600x10800.png"
--inputMap = "maps/Equirectangular_projection_SW.png"
outputMap = "newMap.png"
m' = 700
n' = 600
