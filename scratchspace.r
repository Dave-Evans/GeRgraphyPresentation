## Scratch Space
options(stringsAsFactors=F)
if (!require(soilDB)) { install.packages('soilDB', dep=TRUE) }
if (!require(SSOAP)) { install.packages("SSOAP", repos = "http://www.omegahat.org/R", type="source") }

library(soilDB)
library(SSOAP)
library(rgdal)
library(rgeos)
library(reshape2)

vrbls = c(
    "Sand",
    "Silt",
    "Clay",
    "OM"
)
datCols = c(
    "sandtotal_r",
    "silttotal_r",
    "claytotal_r",
    "om_r"
)
# soilCols = c(
#     # "Bot1","Bot2","Bot3","Bot4","Bot5","Bot6",
#     "Sand1","Sand2","Sand3","Sand4","Sand5","Sand6",
#     "Clay1","Clay2","Clay3","Clay4","Clay5","Clay6",
#     "Silt1","Silt2","Silt3","Silt4","Silt5","Silt6",
#     "OM1","OM2","OM3","OM4","OM5","OM6"
# )

### Helper functions
scale_to_1 = function(pcts) {
    scaled = pcts / (sum(pcts, na.rm=T))
    return(scaled)
}
sum.that.works = function(values, hrz.height, comppct) {
    d = matrix(values, nrow=hrz.height)
    pct_bool = matrix(!is.na(d), nrow=hrz.height, ncol=ncol(d))
    pct_bool[!pct_bool] = NA
    pcts = t(apply(pct_bool, 1, function(X) {X * comppct}))
    scaled_pcts = t(apply(pcts, 1, scale_to_1))
    ss = rowSums(d * scaled_pcts, na.rm=T)
    s = mean(ss, na.rm=T)
    return(c(s=s))
}
####
sda_crs = CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
# c(e@xmin, e@ymin, e@xmax, e@ymax)
m = mapunit_geom_by_ll_bbox(c(-90.074433, 43.084657, -90.04693, 43.105714))
m = subset(m, select=c(mukey, muareaacres))

proj4string(m) = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

inState = format_SQL_in_statement(m@data$mukey)

q <- paste(
    "SELECT 
        component.mukey,
        component.cokey, 
        compname, 
        hzname, 
        hzdept_r,
        hzdepb_r, 
        comppct_r, 
        sandtotal_r, 
        claytotal_r, 
        silttotal_r, 
        om_r
    FROM component JOIN chorizon ON component.cokey = chorizon.cokey AND mukey in ", 
        inState, 
    "ORDER BY mukey, comppct_r DESC, hzdept_r ASC", sep="")
# albedodry_r, 
# hydgrp, 
# awc_r, 
# dbovendry_r, 
# ksat_r, 
# caco3_r, 
# kwfact, 
# ec_r, 
# ph1to1h2o_r,

	# now get component and horizon-level data for these map unit keys
comp_data <- SDA_query(q)

for (mky in unique(comp_data$mukey)) {
    
    mc = subset(comp_data, mukey == mky)
    SNAM = mc$compname[which.max(mc$comppct_r)][1]
    if (length(unique(mc$cokey))==1 | all(is.na(mc$hzdept_r))) {
        mc = mc[,c('mukey',
           "comppct_r",
           "hzdept_r", 
           "hzdepb_r",
           datCols)]
        if (all(is.na(mc$hzdepb_r))) {
            max_depth = NA
        } else {
            max_depth = max(mc$hzdepb_r, na.rm=T)
        }
        w.aves = colSums(mc$comppct_r * mc[datCols], na.rm=T)
        mky_data = melt(mc, id=c("hzdept_r", "hzdepb_r"))
        names(mky_data)[1:2] = c("top", "bottom")
        mky_data = subset(mky_data, !(variable %in% c("mukey", "chkey")))
        mky_data$value = as.numeric(mky_data$value)
    } else {
        print("Slabber...")
        max_depths = aggregate(cbind(hzdepb_r, comppct_r) ~ cokey, mc, max, na.rm=T)
        max_depth = with(max_depths, weighted.mean(hzdepb_r, w = comppct_r))
        depths(mc) = cokey ~ hzdept_r + hzdepb_r
        # slab to the MU level           
        slab.structure = seq(0,round(max_depth),length.out=6)
        hrz.height = floor(slab.structure[2])
        comppct = with(mc@horizons, unique(cbind(cokey, comppct_r)))[,2]
        mky_data = slab(mc, fm = 
                            ~ sandtotal_r +
                            silttotal_r + 
                            claytotal_r +
                            om_r,
        #                     hydgrp +
        #                     dbovendry_r + 
        #                     kwfact + 
        #                     awc_r + 
        #                     ksat_r + 
        #                     ph1to1h2o_r + 
        #                     fragvol_r +
        #                     albedodry_r + 
        #                     ec_r +
        #                     caco3_r,
                        slab.structure=slab.structure,
                        slab.fun=sum.that.works,
                        hrz.height=hrz.height,
                        comppct=comppct
        )
    }
    nlyrs = length(unique(mky_data$top))
    mky_data = mky_data[order(mky_data$variable, mky_data$top),]
    
    dpths = unique(mky_data$bottom)
    dpths = dpths - c(0, dpths[-length(dpths)])
    
    polys = which(m@data$mukey == mky) 
    
    for (vrbl in 1:length(vrbls)){
        ssurgoCol = datCols[vrbl]
        varData = subset(mky_data, variable == ssurgoCol)
        varData = varData[order(varData$top),]
        for (hz in 1:nrow(varData)){
            hrzCol = paste0(vrbls[vrbl], hz)
            prpVal = varData[hz, "value"]
            m@data[polys,hrzCol] = prpVal
        }
    }
    for (dpth in 1:length(dpths)){
        m@data[polys, paste0("Depth", dpth)] = dpths[dpth]
    }
    
}
writeOGR(
    m,
    dsn=paste0(getwd(),"/data"),
    layer="soilsData",
    driver="ESRI Shapefile",
    overwrite_layer=T
)


    