
import Drawing
import Exercises
import Geometry (find,beyond,line_circle,circle_circle)
import GeoUtils (coarse_ne)

main = drawPicture myPicture

myPicture = version1

grain = 0.02

left = -10
next = left + grain
right = -left
range = [left,next..right]

version1 points =
    drawPoints circle &
    drawPoints parabola &
    message "Fat Circle and Parabola"
    where circle = [(x,y) | x<-range,y<-range,(let r2=x*x+y*y in eq r2 1 || eq r2 4)]
          parabola = [(x,x*x) | x<-range]
          eq x y = not (coarse_ne x y)
