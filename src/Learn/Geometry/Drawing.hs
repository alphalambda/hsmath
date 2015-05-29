module Drawing where

import Graphics.Gloss ( Picture(..) )
import qualified Graphics.Gloss as P
import qualified Graphics.Gloss.Data.Color as Color
import System.Random
import Global

--import qualified Geometry as Geo
import qualified GeoUtils as Geo
import GeoUtils ( (+|), Number, Point )

window = 600
size = 10.0
corner = (10,10)

dot_radius = 0.1
plot_resolution = 0.1

range = map (* 4) [left,next..right]
    where grain = 0.02
          left = -size
          next = left + grain
          right = -left

infixr 0 &
(&) :: Picture -> Picture -> Picture
a & Pictures bs = Pictures (a:bs)
a & b           = Pictures [a,b]

randomPoints gen =
    let (gen1,gen2) = split gen
        hsize = coverage * size
        xs = randomRs (-hsize,hsize) gen1
        ys = randomRs (-hsize,hsize) gen2
    in map Geo.coarsepoint $ zip xs ys

drawPicture generate =
        do gen <- getStdGen
           P.display (P.InWindow windowtitle (window,window) corner)
                     P.white
                     (P.scale s s $ generate (randomPoints gen))
    where s = fromIntegral window * 0.5 / size


drawGraph f = drawPath $ map f' xs
    where xs = [-margin,-next..margin]
          margin = 2.0 * size
          next = margin - plot_resolution
          f' x = (x,f x)
          
shownum :: Number -> String
shownum x = show . Geo.coarsenum $ x

showpoint :: Point -> String
showpoint (x,y) = "(" ++ shownum x ++ "," ++ shownum y ++ ")"

translate_ (x,y) p = P.translate x y p
rotate a = P.rotate (-a)

plus_pt = P.pictures [ hline, vline ]
    where hline = P.line [(-dot_radius,0),(dot_radius,0)]
          vline = P.line [(0,-dot_radius),(0,dot_radius)]

eks_pt = P.pictures [ lline, rline ]
    where lline = P.line [(-dot_radius,-dot_radius),(dot_radius,dot_radius)]
          rline = P.line [(dot_radius,-dot_radius),(-dot_radius,dot_radius)]

star_pt = plus_pt & eks_pt

dot_pt =  P.circleSolid dot_radius

circle_pt = P.circle dot_radius

drawPointAs fig p = translate_ p fig

drawPointsAs fig [] = P.blank
drawPointsAs fig [p] = drawPointAs fig p
drawPointsAs fig (p:ps) = drawPointAs fig p & drawPointsAs fig ps

drawStar = drawPointAs star_pt
drawStars = drawPointsAs star_pt

drawEks = drawPointAs eks_pt
drawEkses = drawPointsAs eks_pt

drawPlus = drawPointAs plus_pt
drawPluses = drawPointsAs plus_pt

drawPoint = drawPointAs dot_pt
drawPoints = drawPointsAs dot_pt

autolabels = [[x] | x <- ['A'..'Z'] ]

draw = P.pictures

drawAutoLabels ps = drawLabels ps autolabels

drawPointLabel p text = drawPoint p & drawLabel p text
drawPointsLabels ps ls = drawPoints ps & drawLabels ps ls

drawLabel p text = translate_ (p +| (0.25,0)) $ P.scale textscale textscale $ P.text text
drawLabels ps ls = P.pictures $ zipWith drawLabel ps ls

drawSegment (a,b) = P.line [a,b]

drawSegmentLabel (pa,pb) (la,lb) =
    drawPointLabel pa la
    & drawPointLabel pb lb
    & P.line [pa,pb]

drawLine (p,q) | Geo.apart p q = P.line [(xlow,ylow),(xhigh,yhigh)]
               | otherwise = P.blank
    where xlow = -(4*size)
          xhigh = 4*size
          ylow = Geo.line_gety l' xlow
          yhigh = Geo.line_gety l' xhigh
          l' = Geo.line (p,q)

-- draw an arc passing through A
drawArc (a,o,b) = translate_ o $ P.arc pstart pstop r
    where a_ = Geo.translate_axes o a
          b_ = Geo.translate_axes o b
          (pstart,pstop) = Geo.dphase a_ b_
          r = Geo.dist a o

drawCircle (o,p) = translate_ o $ P.circle r
    where r = Geo.dist o p

drawCircle' (o,p) = drawPoint o & drawCircle (o,p)

drawCircle'' (o,p) = drawPoint o & drawPoint p & drawCircle (o,p)

drawTriangle (a,b,c) = drawPolygon abc & drawPoints abc
    where abc = [a,b,c]

drawPolygon = P.lineLoop
drawPath = P.line

message text = translate_ (0.5-size,0.5-size) $ P.scale textscale textscale $ P.text text

coordinates = axes & guidelines -- & marks
    where xline x y = iline [(-x,0), (x,0)]
          xaxis = P.color P.black (xline hbound 0)
          axes = xaxis & rotate 90 xaxis
          guidelines = faint (vguidelines & hguidelines)
          vguidelines = P.pictures [ iline [(x,-vbound),(x,vbound)] | x <- [-hbound, 1-hbound .. hbound] ]
          hguidelines = P.pictures [ iline [(-hbound,y),(hbound,y)] | y <- [-vbound, 1-vbound .. vbound] ]
          iline ps = P.line $ map (\(x,y) -> (fromIntegral x,fromIntegral y)) ps
          hbound = round (4 * size)
          vbound = round (4 * size)

red = P.color P.red
blue = P.color P.blue
green = P.color P.green
faint = P.color (Color.makeColor 0.5 0.5 0.5 0.5)
