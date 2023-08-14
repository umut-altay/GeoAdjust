# Plots of prediction and uncertainty for analysis of NDHS2018 (Without aggregation)

# Constructing a prediction raster:

# national borders map 
NGA_0 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                layer = "gadm40_NGA_0")  

# reproject the shape file into following coordinate system:
proj = "+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"
NGA_0_trnsfrmd = spTransform(NGA_0,proj)
# > NGA_0@bbox
# min       max
# x -3805.7869 -2222.120
# y   562.5405  1828.165
  
# create a raster within the bbox of the reprojected map :
predRaster <- raster(ncol=400, nrow=400, xmn=-3805.7869, xmx=-2222.120, ymn=562.5405, ymx=1828.165)
#assign resolution :
res(predRaster) = 5   
#assign projection :
projection(predRaster) = "+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"
  
# > print(predRaster)
# class      : RasterLayer 
# dimensions : 253, 317, 80201  (nrow, ncol, ncell)
# resolution : 5, 5  (x, y)
# extent     : -3805.787, -2220.787, 563.165, 1828.165  (xmin, xmax, ymin, ymax)
# crs        : +proj=utm +zone=37 +ellps=clrk80 +units=km +no_defs 
  

# Geography
proj = "+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"

NGA_1 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                layer = "gadm40_NGA_1")

NGA_1_trnsfrmd = spTransform(NGA_1,proj)

NGA_1compressed = gSimplify(NGA_1_trnsfrmd, tol=.01, topologyPreserve = TRUE)
NGA_1compressed = SpatialPolygonsDataFrame(NGA_1compressed, NGA_1_trnsfrmd@data, match.ID=FALSE)

NGA_2 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                layer = "gadm40_NGA_2")

NGA_2_trnsfrmd = spTransform(NGA_2,proj)


idx = 1:80201

predCoords = xyFromCell(predRaster, idx)
predCoords = SpatialPoints(predCoords, proj4string=CRS("+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"))
polyg = NGA_2_trnsfrmd[160,] # the lake

# find which points are inside the lake
pointInPolygon = rgeos::gWithin(predCoords, polyg, byid=TRUE)

inLake = which(pointInPolygon == TRUE)


# predictions and the uncertainty from full adjusted model
load("realData_Results_CR.RData")

predCR = setValues(predRaster, values = Results_CR[["PredictedResponses"]][,2], index=idx)
predCR[inLake] = NA
predCR = raster::mask(crop(predCR, extent(NGA_0_trnsfrmd)), NGA_0_trnsfrmd, snap = 'out')

uncertaintyCR = (Results_CR[["PredictedResponses"]][,3]/Results_CR[["PredictedResponses"]][,1])*100
uncertaintyCR = setValues(predRaster, values = uncertaintyCR, index=idx)
uncertaintyCR[inLake] = NA
uncertaintyCR = raster::mask(crop(uncertaintyCR, extent(NGA_0_trnsfrmd)), NGA_0_trnsfrmd, snap = 'out')


# predictions and the uncertainty from unadjusted model
load("realData_Results_NN.RData")

predNN = setValues(predRaster, values = Results_NN[["PredictedResponses"]][,2], index=idx)
predNN[inLake] = NA
predNN = raster::mask(crop(predNN, extent(NGA_0_trnsfrmd)), NGA_0_trnsfrmd, snap = 'out')

uncertaintyNN = (Results_NN[["PredictedResponses"]][,3]/Results_NN[["PredictedResponses"]][,1])*100

uncertaintyNN = setValues(predRaster, values = uncertaintyNN, index=idx)
uncertaintyNN[inLake] = NA
uncertaintyNN = raster::mask(crop(uncertaintyNN, extent(NGA_0_trnsfrmd)), NGA_0_trnsfrmd, snap = 'out')

rm(Results_NN)

# predictions and the uncertainty from smoothed model
load("realData_Results_NNsmoothed.RData")

predNNsmoothed = setValues(predRaster, values = Results_NNsmoothed[["PredictedResponses"]][,2], index=idx)
predNNsmoothed[inLake] = NA
predNNsmoothed = raster::mask(crop(predNNsmoothed, extent(NGA_0_trnsfrmd)), NGA_0_trnsfrmd, snap = 'out')

uncertaintyNNsmoothed = (Results_NNsmoothed[["PredictedResponses"]][,3]/Results_NNsmoothed[["PredictedResponses"]][,1])*100

uncertaintyNNsmoothed = setValues(predRaster, values = uncertaintyNNsmoothed, index=idx)
uncertaintyNNsmoothed[inLake] = NA
uncertaintyNNsmoothed = raster::mask(crop(uncertaintyNNsmoothed, extent(NGA_0_trnsfrmd)), NGA_0_trnsfrmd, snap = 'out')

rm(Results_NNsmoothed)


#set common color limits
predNNsmoothedval = getValues(predNNsmoothed)
predNNval = getValues(predNN)
predCRval = getValues(predCR)
predVal = c(predNNval, predCRval,predNNsmoothedval)
# > summary(predVal)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.04    0.12    0.24    0.39    0.98   93849 

# PLOTS : 

dfNigeria <- fortify(NGA_1_trnsfrmd, region = "NAME_1")

NGA_1@data[["OBJECTID"]] =1:37
dfNigeria <- fortify(NGA_1_trnsfrmd, region = "NAME_1")

# Prediction Map (plot NN, CR and smoothed by altering the variable names on the same code below)

locsPred = xyFromCell(predRaster, idx)
val = getValues(predNNsmoothed)
d=data.frame(East = locsPred[,1],
             North = locsPred[,2],val = val)

ggplot(d, aes(East,North)) + 
  geom_raster(aes(fill=val)) + theme_bw() +
  geom_path(data = dfNigeria, aes(long,lat, group = group),colour = "black", inherit.aes = FALSE)+
  theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3)))+
  theme(legend.title = element_text(size = rel(3))) + coord_fixed() +
  xlab("Easting (km)") + 
  ylab("Northing (km)")  + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(legend.text=element_text(size=35))+
  scale_fill_viridis_c(option = "viridis", begin = 0.2, end = 1, limits = c(0, 0.98), na.value="white") + 
  guides(fill = guide_colourbar(barwidth = 2.5, barheight = 25, title = labs("pred."), title.vjust=3) ) + 
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))


ggsave("predNNsmoothed.pdf", path = "", width = 30.24 , height = 22.34, units = "cm")


# Uncertainty Map  (plot NN, CR and smoothed by altering the variable names on the same code below)
uncertaintyNNsmoothedval = getValues(uncertaintyNNsmoothed)
uncertaintyNNval = getValues(uncertaintyNN)
uncertaintyCRval = getValues(uncertaintyCR)
uncertaintyVal = c(uncertaintyNNval, uncertaintyCRval,uncertaintyNNsmoothedval)  

# > summary(uncertaintyVal)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    1.02   41.63   76.42   80.46  118.67  249.44   93849 


val = getValues(uncertaintyNNsmoothed)
d=data.frame(East = locsPred[,1],
             North = locsPred[,2],val = val)

ggplot(d, aes(East,North)) + 
  geom_raster(aes(fill=val)) + theme_bw() +
  geom_path(data = dfNigeria, aes(long,lat, group = group),colour = "black", inherit.aes = FALSE)+
  theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3)))+
  theme(legend.title = element_text(size = rel(3))) + coord_fixed() +
  xlab("Easting (km)") + 
  ylab("")  + # if not used, this should stay as "" . Otherwise the plot uses the area of the title to get bigger
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(legend.text=element_text(size=35))+
  scale_fill_viridis_c(option = "viridis", begin = 0.2, end = 1, limits = c(1.02, 249.44), na.value="white") + #geom_point(data = locObs, color = "red", size=0.001, shape="plus")+
  guides(fill = guide_colourbar(barwidth = 2.5, barheight = 25, title = labs("cv (%)"), title.vjust=3) ) + 
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) # these remove the gap between the graph and the axes

ggsave("uncertaintyNNsmoothed.pdf", path = "", width = 30.24 , height = 22.34, units = "cm")


# Ratio maps  (since predNN, predCR, uncertaintyNN and uncertaintyCR are processed above, you need to re-extract those)

# predictions and the uncertainty from full adjusted model
predCR = Results_CR[["PredictedResponses"]][,2]
uncertaintyCR = (Results_CR[["PredictedResponses"]][,3]/Results_CR[["PredictedResponses"]][,1])*100
rm(Results_CR)

# predictions and the uncertainty from unadjusted model
predNN = Results_NN[["PredictedResponses"]][,2]
uncertaintyNN = (Results_NN[["PredictedResponses"]][,3]/Results_NN[["PredictedResponses"]][,1])*100

# ratios
ratioPred = predNN/predCR
ratioUncertainty =uncertaintyNN/uncertaintyCR


ratioPred = setValues(predRaster, values = ratioPred, index=idx)
ratioPred[inLake] = NA

ratioUncertainty = setValues(predRaster, values = ratioUncertainty, index=idx)
ratioUncertainty[inLake] = NA

ratioPred = raster::mask(crop(ratioPred, extent(NGA_0_trnsfrmd)), NGA_0_trnsfrmd, snap = 'out')
ratioUncertainty = raster::mask(crop(ratioUncertainty, extent(NGA_0_trnsfrmd)), NGA_0_trnsfrmd, snap = 'out')

# Ratio of Predictions:

locsPred = xyFromCell(predRaster, idx)
val = getValues(ratioPred)
d=data.frame(East = locsPred[,1],
             North = locsPred[,2],val = val)



ggplot(d, aes(East,North)) + 
  geom_raster(aes(fill=val)) + colorspace::scale_fill_continuous_divergingx(
    trans = "log10",
    mid = log10(1),
    palette = "BrBg" , na.value="white"
  ) + theme_bw() +
  geom_path(data = dfNigeria, aes(long,lat, group = group),colour = "black", inherit.aes = FALSE)+
  theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3)))+
  theme(legend.title = element_text(size = rel(2))) + coord_fixed() + 
  xlab("Easting (km)") + ylab("Northing (km)")  + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) + theme(legend.text=element_text(size=35))+
  guides(fill = guide_colourbar(barwidth = 2.5, barheight = 25, title = labs("Ratio"), title.vjust=3)) + 
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) # these remove the gap between the graph and the axes

ggsave("ratioPred.pdf", path = "", width = 30.24 , height = 22.34, units = "cm")



# Ratio of Uncertainty

val = getValues(ratioUncertainty)
d=data.frame(East = locsPred[,1],
             North = locsPred[,2],val = val)

ggplot(d, aes(East,North)) + 
  geom_raster(aes(fill=val)) + colorspace::scale_fill_continuous_divergingx(
    trans = "log10",
    mid = log10(1),
    palette = "BrBg" , na.value="white"
  ) + theme_bw() +
  geom_path(data = dfNigeria, aes(long,lat, group = group),colour = "black", inherit.aes = FALSE)+
  theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3)))+
  theme(legend.title = element_text(size = rel(2))) + coord_fixed() + 
  xlab("Easting (km)") + ylab("")  + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) + theme(legend.text=element_text(size=35))+
  guides(fill = guide_colourbar(barwidth = 2.5, barheight = 25, title = labs("Ratio"), title.vjust=3)) + 
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) # these remove the gap between the graph and the axes


ggsave("ratioUncertainty.pdf", path = "", width = 30.24 , height = 22.34, units = "cm")












