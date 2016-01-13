options(stringsAsFactors=F)

library(rgdal)
library(rgeos)
library(foreign)
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
sen24 = subset(
    wrds,
    SEN == "24",
    select=kp)

### Export
writeOGR(
    obj=sen24,
    dsn="./Documents/GeRgraphyPresentation/data",
    layer="WardData",
    driver="ESRI Shapefile"
)
