# Aggregated (areal) prediction and uncertainty plots for NDHS2018
library(rgdal)
library(sp)
library(rgeos)
library(raster)


# Geography
proj = "+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"

NGA_0 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                layer = "gadm40_NGA_0")

NGA_0_trnsfrmd = spTransform(NGA_0,proj)

NGA_1 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                layer = "gadm40_NGA_1")
NGA_1_trnsfrmd = spTransform(NGA_1,proj)

#dfNigeria <- fortify(adm1Nigeria_trnsfrmd, region = "NAME_1")

NGA_1compressed = gSimplify(NGA_1_trnsfrmd, tol=.01, topologyPreserve = TRUE)
NGA_1compressed = SpatialPolygonsDataFrame(NGA_1compressed, NGA_1_trnsfrmd@data, match.ID=FALSE)


# Constructing a prediction raster :

# > NGA_0_trnsfrmd@bbox
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

idx = 1:80201
cellcenters = xyFromCell(predRaster, cell=idx)
cellcenters = SpatialPoints(cellcenters, proj4string=CRS("+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"))

areas = over(cellcenters, NGA_1_trnsfrmd, returnList = FALSE)

nameVec = areas[,"NAME_1"]

# population raster
populationRaster <- raster::raster(x = "NGA_ppp_v2c_2015.tif")
pop <- raster::extract(populationRaster, cellcenters, ncol=2)

# Prepare data from the real data results of adusted model :

load("realData_Results_CR.RData")


finalPred_CR = list()
for (i in 1:10000){
  plotData =data.frame(areas = areas[,"NAME_1"],
                       East = cellcenters@coords[,1],
                       North = cellcenters@coords[,2],
                       pred = Results_CR[["eta.samples"]][,i],
                       population = pop,
                       predxPop = Results_CR[["eta.samples"]][,i]*pop)
  plotTable = aggregate(.~areas, data=plotData, FUN=sum)
  finalPred_CR[[i]] = plotTable[,"predxPop"]/plotTable[,"population"]
}


matCR = do.call(rbind, finalPred_CR)
sdCR = apply(matCR, 2, sd)
meanCR = apply(matCR, 2, mean)
medianCR = apply(matCR, 2, median)
cvCR = (sdCR/meanCR)*100

save(matCR, sdCR, meanCR, medianCR, cvCR, file= "")
rm(finalPred_CR)
rm(Results_CR)

# Prepare data from the real data results of smoothed model :
load("realData_Results_NNsmoothed.RData")

finalPred_NNsmoothed = list()
for (i in 1:10000){
  plotData =data.frame(areas = areas[,"NAME_1"],
                       East = cellcenters@coords[,1],
                       North = cellcenters@coords[,2],
                       pred = Results_NNsmoothed[["eta.samples"]][,i],
                       population = pop,
                       predxPop = Results_NNsmoothed[["eta.samples"]][,i]*pop)
  plotTable = aggregate(.~areas, data=plotData, FUN=sum)
  finalPred_NNsmoothed[[i]] = plotTable[,"predxPop"]/plotTable[,"population"]
}

matNNsmoothed = do.call(rbind, finalPred_NNsmoothed)
sdNNsmoothed = apply(matNNsmoothed, 2, sd)
meanNNsmoothed = apply(matNNsmoothed, 2, mean)
medianNNsmoothed = apply(matNNsmoothed, 2, median)
cvNNsmoothed = (sdNNsmoothed/meanNNsmoothed)*100

save(matNNsmoothed, sdNNsmoothed, meanNNsmoothed, medianNNsmoothed, cvNNsmoothed, file= "~/Desktop/Paper2RevisionMaterials/areal/arealNNsmoothed.RData")
rm(finalPred_NNsmoothed)
rm(Results_NNsmoothed)


# Prepare data from the real data results of unadjusted model :
load("realData_Results_NN.RData")

finalPred_NN = list()
for (i in 1:10000){
  plotData =data.frame(areas = areas[,"NAME_1"],
                       East = cellcenters@coords[,1],
                       North = cellcenters@coords[,2],
                       pred = Results_NN[["eta.samples"]][,i],
                       population = pop,
                       predxPop = Results_NN[["eta.samples"]][,i]*pop)
  plotTable = aggregate(.~areas, data=plotData, FUN=sum)
  finalPred_NN[[i]] = plotTable[,"predxPop"]/plotTable[,"population"]
}


matNN = do.call(rbind, finalPred_NN)

sdNN = apply(matNN, 2, sd)
meanNN = apply(matNN, 2, mean)
medianNN = apply(matNN, 2, median)
cvNN = (sdNN/meanNN)*100
save(matNN, sdNN, meanNN, medianNN, cvNN, file= "")


rm(finalPred_NN)
rm(Results_NN)

ratioAggregatedUncertainty = cvNN/cvCR
ratioAggregatedPred = meanNN/meanCR


# Prepare for plotting
NAME_1 = NGA_1_trnsfrmd@data[["NAME_1"]]
valuesCR_df<-data.frame(NAME_1, cvCR, meanCR)
valuesNN_df<-data.frame(NAME_1, cvNN, meanNN)
valuesNNsmoothed_df<-data.frame(NAME_1, cvNNsmoothed, meanNNsmoothed)
valuesDirect_df<-data.frame(NAME_1, cvDirect, meanDirect)

library(plyr)
NGA_1_trnsfrmd@data$id <- rownames(NGA_1_trnsfrmd@data)
NGA_1_trnsfrmd@data <- join(NGA_1_trnsfrmd@data, valuesNN_df, by="NAME_1")
NGA_1_trnsfrmd@data <- join(NGA_1_trnsfrmd@data, valuesCR_df, by="NAME_1")
NGA_1_trnsfrmd@data <- join(NGA_1_trnsfrmd@data, valuesNNsmoothed_df, by="NAME_1")
NGA_1_trnsfrmd@data <- join(NGA_1_trnsfrmd@data, valuesDirect_df, by="NAME_1")



dfNigeria <- fortify(NGA_1_trnsfrmd)
dfNigeria <- join(dfNigeria,NGA_1_trnsfrmd@data, by="id")

# admin 2 borders
NGA_2 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                layer = "gadm40_NGA_2")

NGA_2_trnsfrmd = spTransform(NGA_2,proj)
polyg = NGA_2_trnsfrmd[160,] # the lake
val = c(NA)

dfpolyg <- fortify(polyg)

# CV
cvValues = c(cvNN, cvNNsmoothed, cvCR, cvDirect)
summary(cvValues)
# > summary(cvValues)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.194   3.472   5.086   7.178  10.522  29.669


ggplot() +
  geom_polygon(data = dfNigeria, aes(x = long, y = lat, group = group, fill =
                                       cvCR), color = "black", size = 0.25) +
  geom_polygon(data = dfpolyg, aes(x = long, y = lat), colour="black",  linewidth = 0.25, fill="white") + theme_bw() + # fill the lake with white color
  coord_fixed() +
  scale_fill_viridis_c(limits = c(1.193, 29.7)) +
  theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3)))+
  xlab("Easting (km)") +
  ylab("Northing (km)")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(legend.text=element_text(size=35), legend.title=element_text(size=25)) +
  guides(fill = guide_colourbar(barwidth = 2.5, barheight = 25, title = labs("cv (%)"), title.vjust=1) ) +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))

ggsave("cvAggregatedCR.pdf", path = "", width = 30.24 , height = 22.34, units = "cm")


# PRED

meanValues = c(meanNN, meanNNsmoothed, meanCR, meanDirect)
# summary(meanValues)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.05359 0.27408 0.52475 0.49583 0.69896 0.89386

ggplot() +
  geom_polygon(data = dfNigeria, aes(x = long, y = lat, group = group, fill =
                                       meanDirect), color = "black", size = 0.25) +
  geom_polygon(data = dfpolyg, aes(x = long, y = lat), colour="black",  linewidth = 0.25, fill="white") + theme_bw() + # fill the lake with white color
  coord_fixed() +
  scale_fill_viridis_c(limits = c(0.053, 0.895), option = "viridis") +  #limit from 0 to 0.99 is the limit of unaggregated ones. It is used here as well, to make the color scale the same.
  theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3)))+
  xlab("Easting (km)") +
  ylab("Northing (km)")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(legend.text=element_text(size=35), legend.title=element_text(size=25)) +
  guides(fill = guide_colourbar(barwidth = 2.5, barheight = 25, title = labs("pred."), title.vjust=1) ) +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))

ggsave("predDirect.pdf", path = "", width = 30.24 , height = 22.34, units = "cm")


# RATIO CV

ggplot() +
  geom_polygon(data = dfNigeria, aes(x = long, y = lat, group = group, fill =
                                       ratioAggregatedUncertainty), color = "black", size = 0.25) +
  geom_polygon(data = dfpolyg, aes(x = long, y = lat), colour="black",  linewidth = 0.25, fill="white") + # fill the lake with white color
  colorspace::scale_fill_continuous_divergingx(
    trans = "log10",
    mid = log10(1),
    palette = "BrBg" , na.value="white"
  ) + theme_bw() +
  theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3)))+
  xlab("Easting (km)") +
  ylab("Northing (km)")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) + theme(legend.text=element_text(size=35),legend.title=element_text(size=25))+
  guides(fill = guide_colourbar(barwidth = 2.5, barheight = 25, title = labs("Ratio"), title.vjust=3)) +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))

ggsave("ratioAggregatedCV.pdf", path = "", width = 30.24 , height = 22.34, units = "cm")


# RATIO CV


ggplot() +
  geom_polygon(data = dfNigeria, aes(x = long, y = lat, group = group, fill =
                                       ratioAggregatedPred), color = "black", size = 0.25) +
  geom_polygon(data = dfpolyg, aes(x = long, y = lat), colour="black",  linewidth = 0.25, fill="white") + # fill the lake with white color
  colorspace::scale_fill_continuous_divergingx(
    trans = "log10",
    mid = log10(1),
    palette = "BrBg" , na.value="white"
  ) + theme_bw() +
  theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3)))+
  xlab("Easting (km)") +
  ylab("Northing (km)")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) + theme(legend.text=element_text(size=35),legend.title=element_text(size=25))+
  guides(fill = guide_colourbar(barwidth = 2.5, barheight = 25, title = labs("Ratio"), title.vjust=3)) +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))

ggsave("ratioAggregatedPred.pdf", path = "", width = 30.24 , height = 22.34, units = "cm")


# Scatter plots

load the file containing the direct estimates created by using the script called "getDirectEsts.R" .
#####################
meanDirect = directEst$est
dfCRmean = data.frame(direct = meanDirect, s = meanCR)

ggplot() +
  geom_point(data=dfCRmean, aes(x=s, y=direct, color='FullAdj'),size=5) +
  theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3)))+
  xlab("Areal Estimates  (mean)") +
  ylab("Direct Estimates (mean)") +
  xlim(0, 0.9) + ylim(0,0.9)+
  geom_abline(intercept = 0, slope = 1, size = 0.5)+
  theme(legend.position="none")
  
ggsave("meanVSmean.pdf", path = "", width = 30.24 , height = 22.34, units = "cm")


######################
sdDirect = sqrt(directEst$var)
cvDirect = (sdDirect/directEst$est)*100


dfNNcv = data.frame(direct = cvDirect, s = cvNN)
dfNNsmoothedcv = data.frame(direct = cvDirect, s = cvNNsmoothed)
dfCRcv = data.frame(direct = cvDirect, s = cvCR)

ggplot() +
  geom_point(data=dfCRcv, aes(x=s, y=direct, color='FullAdj'),size=5) +
  theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3)))+
  xlab("Areal Estimates  cv (%)") +
  ylab("Direct Estimates  cv (%)") +
  xlim(0, 30) + ylim(0,30)+
  geom_abline(intercept = 0, slope = 1, size = 0.5)+
  theme(legend.position="none")

ggsave("CVvsCV.pdf", path = "", width = 30.24 , height = 22.34, units = "cm")












