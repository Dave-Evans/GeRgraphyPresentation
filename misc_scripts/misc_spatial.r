par(mfrow=c(2,3))
## Spatial Points
sptl = data.frame(
    id=c(1,2,3,4),
    data=rnorm(4),
    x=runif(4,0,2),
    y=runif(4,0,2)
)
coordinates(sptl) <- ~ x+y
plot(sptl, axes=T, pch=19, main="Points")
mtext("bird sightings or fire hydrants")

###Spatial Lines
l1 = cbind(c(1,2,3),c(3,2,2))
l1a = cbind(l1[,1]+1,l1[,2]+1)
l2 = cbind(c(1,2,3),c(1,1.5,1))
Sl1 = Line(l1)
Sl1a = Line(l1a)
Sl2 = Line(l2)
S1 = Lines(list(Sl1, Sl1a), ID="a")
S2 = Lines(list(Sl2), ID="b")
sl = SpatialLines(list(S1))#,S2
plot(sl, axes=T)
mtext("rivers or roads")

## Spatial Polygons
x = readWKT("POLYGON((1 0,0 1,1 2,2 1,1 0))")
plot(x, axes=T, main="Lines")
mtext("building footprint or county")

## fourth plot
plot(1, type="n", axes=F, xlab="", ylab="")

## Raster
data("meuse.grid")
coordinates(meuse.grid) <- ~ x+y
gridded(meuse.grid) = TRUE
image(meuse.grid["dist"], main="Raster")
mtext("distance to river or lead concentration")

## sixth plot
plot(1, type="n", axes=F, xlab="", ylab="")

sl2 = SpatialLines(list(S2))
crdTxt = apply(l2, 1, FUN=function(x) paste(x[1], x[2], sep=", "))
plot(sl2)
points(x=l2[1:3], y=l2[4:6],pch=19)
text(x=l2[1:3], y=l2[4:6], labels=crdTxt, pos=3)
## Difference
library(rgeos)
library(scales)
x = readWKT("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
y = readWKT("POLYGON ((3 3, 7 3, 7 7, 3 7, 3 3))")

d = gDifference(x,y)
plot(d,col='red',pbg='white')

# Empty geometry since y is completely contained with x
d2 = gDifference(y,x)

x = readWKT("POLYGON((1 0,0 1,1 2,2 1,1 0))")
x.inter = x
x.bound = gBoundary(x)

y = readWKT("POLYGON((2 0,1 1,2 2,3 1,2 0))")
y.inter = y
y.bound = gBoundary(y)

xy.inter = gIntersection(x,y)
xy.inter.bound = gBoundary(xy.inter)

xy.union = gUnion(x,y)
bbox = gBuffer(gEnvelope(xy.union),width=0.5,joinStyle='mitre',mitreLimit=3)

x.exter = gDifference(bbox,x)
y.exter = gDifference(bbox,y)

plot(bbox, border='white')
plot(x, add=T, col=alpha('red', 0.5))
plot(y, add=T, col=alpha('blue', 0.5))

# geometry decomposition
pdf("Relates.pdf")
par(mfrow=c(2,3))
plot(bbox,border='grey');plot(x,col="black",add=TRUE);title("Interior",ylab = "Polygon X")
plot(bbox,border='grey');plot(x.bound,col="black",add=TRUE);title("Boundary")
plot(bbox,border='grey');plot(x.exter,col="black",pbg='white',add=TRUE);title("Exterior")
plot(bbox,border='grey');plot(y,col="black",add=TRUE);title(ylab = "Polygon Y")
plot(bbox,border='grey');plot(y.bound,col="black",add=TRUE)
plot(bbox,border='grey');plot(y.exter,col="black",pbg='white',add=TRUE)
dev.off()


defaultplot = function() {
    plot(bbox,border='grey')
    plot(x,add=TRUE,col='red1',border="red3")
    plot(y,add=TRUE,col='blue1',border="blue3")        
    plot(xy.inter,add=TRUE,col='orange1',border="orange3")
}

# Dimensionally Extended 9-Intersection Matrix
pat = gRelate(x,y)
patchars = strsplit(pat,"")[[1]]

par(mfrow=c(3,3))
defaultplot(); plot(gIntersection(x.inter,y.inter),add=TRUE,col='black')
title(paste("dim:",patchars[1]))
defaultplot(); plot(gIntersection(x.bound,y.inter),add=TRUE,col='black',lwd=2)
title(paste("dim:",patchars[2]))
defaultplot(); plot(gIntersection(x.exter,y.inter),add=TRUE,col='black')
title(paste("dim:",patchars[3]))

defaultplot(); plot(gIntersection(x.inter,y.bound),add=TRUE,col='black',lwd=2)
title(paste("dim:",patchars[4]))
defaultplot(); plot(gIntersection(x.bound,y.bound),add=TRUE,col='black',pch=16)
title(paste("dim:",patchars[5]))
defaultplot(); plot(gIntersection(x.exter,y.bound),add=TRUE,col='black',lwd=2)
title(paste("dim:",patchars[6]))

defaultplot(); plot(gIntersection(x.inter,y.exter),add=TRUE,col='black')
title(paste("dim:",patchars[7]))
defaultplot(); plot(gIntersection(x.bound,y.exter),add=TRUE,col='black',lwd=2)
title(paste("dim:",patchars[8]))
defaultplot(); plot(gIntersection(x.exter,y.exter),add=TRUE,col='black')
title(paste("dim:",patchars[9]))

####
p1 = readWKT("POLYGON((0 0,1 0,1 1,0 1,0 0))")
p2 = readWKT("POLYGON((0.5 1,0 2,1 2,0.5 1))")
p3 = readWKT("POLYGON((0.5 0.5,0 1.5,1 1.5,0.5 0.5))")

l1 = readWKT("LINESTRING(0 3,1 1,2 2,3 0)")
l2 = readWKT("LINESTRING(1 3.5,3 3,2 1)")
l3 = readWKT("LINESTRING(-0.1 0,-0.1 1.1,1 1.1)")

pt1 = readWKT("MULTIPOINT(1 1,3 0,2 1)")
pt2 = readWKT("MULTIPOINT(0 3,3 0,2 1)")
pt3 = readWKT("MULTIPOINT(-0.2 0,1 -0.2,1.2 1,0 1.2)")

pdf("Intersect.pdf")
par(mfrow=c(3,2))
plot(p1,col='blue',border='blue',ylim=c(0,2.5));plot(p2,col='black',add=TRUE,pch=16)
title(paste("Intersects:",gIntersects(p1,p2),
            "\nDisjoint:",gDisjoint(p1,p2)))

plot(p1,col='blue',border='blue',ylim=c(0,2.5));plot(p3,col='black',add=TRUE,pch=16)
title(paste("Intersects:",gIntersects(p1,p3),
            "\nDisjoint:",gDisjoint(p1,p3)))

plot(l1,col='blue');plot(pt1,add=TRUE,pch=16)
title(paste("Intersects:",gIntersects(l1,pt1),
            "\nDisjoint:",gDisjoint(l1,pt1)))

plot(l1,col='blue');plot(pt2,add=TRUE,pch=16)
title(paste("Intersects:",gIntersects(l1,pt2),
            "\nDisjoint:",gDisjoint(l1,pt2)))

plot(p1,col='blue',border='blue',xlim=c(-0.5,2),ylim=c(0,2.5))
plot(l3,lwd=2,col='black',add=TRUE)
title(paste("Intersects:",gIntersects(p1,l3),
            "\nDisjoint:",gDisjoint(p1,l3)))

plot(p1,col='blue',border='blue',xlim=c(-0.5,2),ylim=c(-0.5,2))
plot(pt3,pch=16,col='black',add=TRUE)
title(paste("Intersects:",gIntersects(p1,pt3),
            "\nDisjoint:",gDisjoint(p1,pt3)))
dev.off()



