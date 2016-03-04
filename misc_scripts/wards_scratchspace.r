options(stringsAsFactors=F)

library(rgdal)
library(rgeos)
library(foreign)
library(classInt)
library(RColorBrewer)
### Putting together the ward data
dir_media = "/media/devans/KINGSTON/misc/election_dot_map"

### Ward data
wrds = readOGR(
    dsn=dir_media,
    layer="wards_2011"
)
### Election Results from 2012
elecdata = read.dbf(paste(dir_media, "ElectionData_by_Ward.dbf", sep="/"))

wrds = merge(wrds, elecdata, by="GEOID10")
### Join them on ward id

kp = c("NAME.x", 'ASM', "SEN", "CON", "MCD_NAME.x", 
    "PERSONS.x", "WHITE", "BLACK", "HISPANIC", "ASIAN",
    "AMINDIAN", "PISLAND", "OTHER", "OTHERMLT", "PERSONS18.x", 
    "WHITE18", "BLACK18", "HISPANIC18", "ASIAN18",
    "AMINDIAN18", "PISLAND18", "OTHER18", "OTHERMLT18", "CNTY_NAME.x",
    'PRES_TOT12', 'PRES_REP12', 'PRES_DEM12', 'PRES_CON12', 'PRESIND112', 
    'PRESIND212', 'PRESIND312', 'PRESIND412', 'PRESIND512', 'PRESIND612', 
    'PRESSCAT12', 'SEN_TOT12', 'SEN_REP12', 'SEN_DEM12', 'SEN_IND112', 
    'SEN_IND212', 'SEN_IND312', 'SEN_CON_12', 'SEN_SCAT12', 'CON_TOT_12', 
    'CON_REP_12', 'CON_DEM_12', 'CON_IND_12', 'CON_SCAT12', 'SS_TOT_12', 
    'SS_REP_12', 'SS_DEM_12', 'SS_IND_12', 'SS_SCAT_12', 'ASM_TOT_12', 
    'ASM_REP_12', 'ASM_DEM_12', 'ASM_IND112', 'ASM_SCAT12', 'ASM_DEM212',
    'ASM_IND212', 'ASM_REP212', 'DA_TOT_12', 'DA_REP_12', 'DA_DEM_12',
    'DA_IND_12', 'DA_SCAT_12', 'DA_DEM2_12')

### Select just the wards that comprise sen district 24
sen26 = subset(
    wrds,
    SEN == "26",
    select=kp)

### Export
writeOGR(
    obj=sen26,
    dsn="./Documents/GeRgraphyPresentation/data",
    layer="WardData",
    driver="ESRI Shapefile",
    overwrite_layer=T
)

### Scratch
library(RColorBrewer)
library(classInt)
wards = readOGR(
    dsn="./data",
    layer="WardData"
)

wards@data$SEN_PERC_DEM = with(wards@data, SEN_DEM/SEN_TOT)
wards@data$SEN_PERC_TURN = with(wards@data, SEN_TOT/PERSONS1)

wards_centroids = gCentroid(wards, byid=T)
wards_centroids = SpatialPointsDataFrame(
    gCentroid(wards, byid=T), 
    over(wards_centroids, wards)
)


## defining number of classes
num_classes = 6
## the color palette
pal = brewer.pal(num_classes, "RdBu")
## the class intervals to use for the colors
class_ints = classIntervals(wards@data$SEN_PERC_DEM, num_classes, style='equal')
plot(class_ints, pal=pal)
## grab the colors for plotting
colrs = findColours(class_ints, pal)

legtxt = properLegend(colrs, 2)

pdf("PercentDem.pdf")
plot(wards,
     col=colrs,
     main="Senate 24",
     border=NA)
plot(wards_centroids,
     pch=20,
     cex=(wards_centroids@data$PERC_TURN),
     col=alpha('black', 0.5),
     add=T)
plot(wards_centroids,
     pch=1,
     cex=1,
     col=1,
     add=T)

legend("topleft",
    legtxt,
    title="Proportion Democrat",
    fill=pal,
    bty='n'
)
turnPos = legend("topright",
       c("  20%", '  50%', '  80%'),
       bty='n',
       title="Percent Turnout"
)
points(x=turnPos$text$x-50, y=turnPos$text$y, pch=1, cex=1)
points(x=turnPos$text$x-50, y=turnPos$text$y, pch=20, cex=c(0.2, 0.5, 0.8), col=alpha('black', 0.5))

dev.off()


library(spdep)
wards = readOGR(
    dsn="./data",
    layer="WardData"
)

wards@data$SEN_PERC_DEM = with(wards@data, SEN_DEM/SEN_TOT)
wards@data$SEN_PERC_TURN = with(wards@data, SEN_TOT/PERSONS1)
wards@data$CON_PERC_DEM = with(wards@data, CON_DEM/CON_TOT)
wards@data$PRES_PERC_DEM = with(wards@data, PRES_DE/PRES_TO)

wards = subset(wards, !is.nan(wards@data$SEN_PERC_DEM))
neighborhood_binary = poly2nb(wards)
list_of_weights = nb2listw(neighborhood_binary, zero.policy=T)
moran.test(wards@data$SEN_PERC_DEM, list_of_weights, alternative="two.sided")
spat_lin_reg = spautolm(
    SEN_PERC_DEM ~ WHITE + BLACK + PRES_PERC_DEM + CON_PERC_DEM,
    data=wards,
    family="SAR",
    listw=list_of_weights)

# moran.test(
#     resid(spat_lin_reg),
#     list_of_weights,
#     alternative="two.sided")

