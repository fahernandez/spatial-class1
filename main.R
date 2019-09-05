library(dplyr)
library(sf)
library(spData)
library(here)
library(sp)
library(raster)
library(rgdal)
library(rgeos)

name <- LETTERS[1:10]
longitude <- c(-111.6, -120.4, -116.7, -113.5, -115.5, 
               -120.8, -119.5, -113.7, -113.7, -110.7)
latitude  <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9,
               36.2, 39, 41.6, 39.9)

#ejem1
stations<-cbind(longitude, latitude)
set.seed(0)
precip <- (runif(length(latitude))*10)^2
psize <- 1 + precip/500
plot(stations, cex=psize, pch=20, col="red", main = "Precipitación (mm/año)")
text(stations, name, pos = 4)

breaks <- c(100,500,1000,2000)
legend("topright", legend = breaks,pch = 20, 
       pt.cex = psize, col="red", bg= "gray")

#ejemp2
lon <- c(-116.8, -114.2, -112.9, -111.9, 
         -114.2, -115.4, -117.7)
lat  <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6)
x <- cbind(lon, lat)


plot(stations, cex=psize, pch=20, col="red", main = "Precipitación (mm/año)")
polygon(x, col = "blue", border = "light blue")
lines(stations, lwd=3, col="red")
points(stations, cex=psize, pch=20)


wst<-data.frame(longitude, latitude, name, precip)
wst


#ejemp3
longitude <- c(-116.7, -120.4, -116.7, -113.5, -115.5, -120.8, -119.5, -113.7, -113.7, -110.7)
latitude <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9, 36.2, 39, 41.6, 36.9)
lonlat <- cbind(longitude, latitude)
pts<-SpatialPoints(lonlat)
# bbots es la caja que enmarca los puntos
showDefault(pts)

crdref<-CRS('+proj=longlat +datum=WGS84')
ptsdf <- SpatialPoints(lonlat, proj4string = crdref)

plot(pts)
points(ptsdf, col="red")


#ejm4
lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6) 
lonlat <- cbind(lon, lat)
df<-data.frame(ID=1:nrow(lonlat), precip=(latitude-30)^2)
# You can assign data attributes to the point
ptsdf<-SpatialPointsDataFrame(pts, data=df)
# summary
ptsdf
str(ptsdf)

lns<-spLines(lonlat, crs=crdref)
lns

pols <- spPolygons(lonlat, crs=crdref)
pols 

plot(pols, border="blue", col="yellow", lwd=3, axes=TRUE, las=1)
points(pts, col="red", pch=20, cex=3)

# Spatial data can be plugin into the linear model taken into account different structure in whithin the data

#ejmp4
# A raster is like a grid
r<-raster(ncol=10, nrow=10, xmx=-80, xmn=-150, ymn=20, ymx=60)
r
values(r)<-runif(ncell(r))
plot(r)
plot(pols, border="blue", lwd=2, add=TRUE)
points(pts, col="red", pch=20, cex=3)

r2 <- r*r
r3 <- sqrt(r)
# Group raster leyers
s <- stack(r, r2, r3)
class(s)

plot(s)


#ejmpl5
# archivo shp
filename<-system.file("external/lux.shp", package = "raster")
s<-shapefile(filename)
plot(s)

# Create the shp file
shapefile(s, "test:shp", overwrite=TRUE)


f<-system.file("external/rlogo.grd", package = "raster")
r1<-raster(f)
r1
plot(r1)

r2<-raster(f, band=2)
b<-brick(f) # similiar to stack, but brick same the objets at different dimensions so you cannot operate over it
plot(b)

#ejmp6
f <- system.file("external/lux.shp", package="raster")
p <- shapefile(f)
p
par(mai=c(0,0,0,0))
plot(p)

g<-geom(p)
head(g)

# Respet the column of the stracted column
p[,"NAME_2"]

# remover poligonos
p$new<-sample(letters, length(p))
i<-which(p$NAME_1=="Grevenmacher")
g<-p[i,]
plot(g)

# Agregar
z<-raster(p, nrow=2, ncol=2, vals=1:4)
names(z)
plot(z)
plot(z, add=TRUE, border="blue", lwd=5)


z2<-z[3,]
plot(p)


# Agregar por canton
pa<-aggregate(p, by="NAME_1")
za<-aggregate(z)

plot(p)
plot(pa, col=rainbow(3), add=TRUE, )

# boorrar
e<-erase(p,z2)
plot(e)

# sacar el pedazo
plot(intersect(p,z2))
plot(p*z2)

# subrayar
e<-extent(6,6.4,49.7,50)
pe<-crop(p,e)
plot(p)
plot(pe, col="light blue", add=TRUE)
plot(e, add=TRUE, lwd=3, col="red")


# Uniones
u<-union(p,z)
u<-p+z

set.seed(5)
plot(u, col=sample(rainbow(length(u))))


# 
cov<-cover(p,z)
vov
plot(cov)

# 
dif<-symdif()



# Ejenpl8
filename <- system.file("external/test.grd", package="raster")
filename

## ---- raster-2a2---------------------------------------------------------
r <- raster(filename)
filename(r)
hasValues(r)
# Poner el objeto en memoria
inMemory(r)
plot(r, main='RasterLayer from file')

r1 <- r2 <- r3 <- raster(nrow=10, ncol=10)
# Assign random cell values 
values(r1) <- runif(ncell(r1))
values(r2) <- runif(ncell(r2))
values(r3) <- runif(ncell(r3))

s <- stack(r1, r2, r3)
s
nlayers(s)
b1<-brick(r1,r1,r3)
b2<-brick(s)

# Todos los errores en espacial tienen que ver con la clase del objeto y si es compatible con la funcion que se esta usando.
