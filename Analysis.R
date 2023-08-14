#                This script is used for 
# Section 5 - Analysis of completion of secondary education 
#                    of our paper

## Initialization
# Load libraries
library(SUMMER)
library(rgdal)
library(INLA)
library(haven)
library(sp)
library(rgeos)
library(foreign)
library(TMB)
library(scoringRules)
library(foreach)
library(geosphere)
library(raster)
library(spatialEco)
library(mvtnorm)

# load functions
source("modSPDEJitter.R")
source("makeIntegrationPoints.R")
source('functions.R')

# set the seeds
set.seed(2345)
# Setup

# spatial range (in kilometers)
range = 160

# number of unique sub-integration point radii and angles per integration point
# (i.e. the number of sub-integration points per integration point is
# nSubRPerPoint * nSubAPerPoint)
nSubRPerPoint = 10
nSubAPerPoint = 10

# likelihood :
# 1st row : 0/1 for Gaussian/Binomial
likelihoodSc = as.matrix(rbind(likelihood = c(1), nuggetVar = c(0), p = c(0.5)))

# geography and demography data

## road and prepare geography

# read admin0 map (national borders)
nigeriaMap_admin0 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                            layer = "gadm40_NGA_0")

# read admin1 map (subnational)
nigeriaMap_admin1 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                            layer = "gadm40_NGA_1")

# read admin2 map (subnational)
nigeriaMap_admin2 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                     layer = "gadm40_NGA_2")

# remove Lake Chad (Water body)
nigeriaMap_admin2 = nigeriaMap_admin2[-160,]

# read DHS survey
educationData = read_dta("NGIR7BDT/NGIR7BFL.DTA")

# read cluster coordinates 

corList = readOGR(dsn = "dataFiles/DHS/NG_2018_DHS_02242022_98_147470/NGGE7BFL",
                             layer = "NGGE7BFL")


# processing data
smallGeo = data.frame(clusterIdx = corList$DHSCLUST, urban = corList$URBAN_RURA,
                      long = corList$LONGNUM, lat = corList$LATNUM,
                      admin1 = corList$ADM1NAME)

myData = data.frame(clusterIdx = educationData$v001, householdIdx = educationData$v002,
                    stratum = educationData$v023,
                    age = educationData$v012,
                    secondaryEducation = educationData$v106)

myData = subset(myData, age <= 49 & age >=20)
myData$ys = (myData$secondaryEducation>=2)+0

myData = merge(myData, smallGeo, by = "clusterIdx")

# add number of trials
myData$Ntrials = 1


answers_x = aggregate(myData$ys,
                      by = list(clusterID = myData[, 1]),
                      FUN = sum)
answers_n= aggregate(myData$ys,
                     by = list(clusterID = myData[, 1]),
                     FUN = length)

answers_joint = merge(answers_x, answers_n, by="clusterID")
colnames(answers_joint) = c("clusterID", "ys", "ns")

nigeria.data = data.frame(clusterID = corList@data[["DHSCLUST"]], long = corList@coords[,1], lat = corList@coords[,2])
nigeria.data = merge(nigeria.data, answers_joint, by="clusterID", all=T)

# urbanRural from DHS data set
nigeria.data$urbanRuralDHS = corList@data[["URBAN_RURA"]]

# remove NA rows
nigeria.data = nigeria.data[complete.cases(nigeria.data$ys), ]
nigeria.data = nigeria.data[complete.cases(nigeria.data$long), ]
nigeria.data = nigeria.data[complete.cases(nigeria.data$lat), ]

# create admin2 point in polygon table (object "check1") to check against new locations
true_latLon = cbind(nigeria.data[,"long"], nigeria.data[,"lat"])
colnames(true_latLon) = c("long", "lat")
true_latLon = SpatialPoints(true_latLon, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"), bbox = NULL)

nigeriaMap_admin2@data[["OBJECTID"]] =1:774 #(originally 775, but we removed the 160th polygon, which was a lake)
check1 <- over(true_latLon, nigeriaMap_admin2, returnList = FALSE)

# Drop rows which doesn't match with admin1 and 2 areas 
# Jittering algorithm will need them to match.

# > which(is.na(check1$NAME_2))
# [1]   48  122  205  848  857 1116 1122 1287 1328

nigeria.data = nigeria.data[-c(48, 122,  205,  848,  857, 1116, 1122, 1287, 1328),]

check1 = check1[-c(48, 122,  205,  848,  857, 1116, 1122, 1287, 1328),]

nigeria.data$east = rep(NA, length(nigeria.data$long))
nigeria.data$north = rep(NA, length(nigeria.data$long))

# Assign UTM37 coordinates to clusters
nigeria.data[,c("east", "north")] = convertDegToKM(nigeria.data[,c("long", "lat")])
locKM = nigeria.data[,c("east", "north")]

# constructing a prediction raster and predicting at cell centers :

# reproject the shape file into following coordinate system:
proj = "+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"
NGA_0 = spTransform(NGA_0,proj)
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

# make an index for the number of cells :
idx = 1:80201
# extract central coordinates of each cell with respect to the index (idx)
loc.pred = xyFromCell(predRaster, idx)
predCoords = cbind(loc.pred[,1], loc.pred[,2])

# Load the data rasters :
load("processedCovariateData.RData")
load("croppedNotScaledUrbanization.RData")
load("smoothedRasters.RData")
load("populationSmoothRaster.RData")

ys=nigeria.data$ys
ns=nigeria.data$ns

mesh.s <- inla.mesh.2d(loc.domain = cbind(nigeria.data$east, nigeria.data$north),
                       n=5000,
                       max.n=10000,
                       offset=-.08,
                       cutoff=4,
                       max.edge=c(25, 50))


# Compile .cpp files

# standard and jittering accounted models in one file
compile( "simulations.cpp")
dyn.load( dynlib("simulations") )

flag2 = likelihoodSc[[1, 1]]
jScale=1
USpatial = 1
alphaSpatial = 0.05
locObs =cbind(nigeria.data[["east"]], nigeria.data[["north"]])

adminMap = nigeriaMap_admin2
urban = nigeria.data$urbanRuralDHS
testMode=FALSE
nLoc = length(locObs[,1])

# spde components
spde = getSPDEPrior(mesh.s, U=USpatial, alpha=alphaSpatial)
A.proj = inla.spde.make.A(mesh = mesh.s, loc = cbind(locObs[,1], locObs[,2]))

# preparing TMB input

# convert urban U/R into TRUE/FALSE
for (i in 1:length(urban)){
  if (urban[i]=='U'){
    urban[i]='TRUE'
  }else{
    urban[i]='FALSE'
  }
}
urbanVals=as.logical(urban)


intPointInfo = makeAllIntegrationPoints(coords = cbind(locObs[,1], locObs[,2]), urbanVals,
                                        numPointsUrban=1+15*4, numPointsRural=1+15*9,
                                        scalingFactor = jScale,
                                        JInnerUrban=5, JOuterUrban=0,
                                        JInnerRural=5, JOuterRural=5,
                                        adminMap=adminMap,
                                        nSubAPerPoint=nSubAPerPoint,
                                        nSubRPerPoint=nSubRPerPoint,
                                        testMode=testMode)


xUrban = intPointInfo$xUrban
yUrban = intPointInfo$yUrban
wUrban = intPointInfo$wUrban
xRural = intPointInfo$xRural
yRural = intPointInfo$yRural
wRural = intPointInfo$wRural

# get the long set of coordinates
coordsUrban = cbind(c(xUrban), c(yUrban))
coordsRural = cbind(c(xRural), c(yRural))

# separate observations by urbanicity
ysUrban = ys[urbanVals]
ysRural = ys[!urbanVals]
nsUrban = ns[urbanVals]
nsRural = ns[!urbanVals]


# Covariate values at DHS locations and integration points:

coordsUrbanKM = SpatialPoints(coordsUrban, proj4string = CRS("+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"), bbox = NULL)
coordsRuralKM = SpatialPoints(coordsRural, proj4string = CRS("+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"), bbox = NULL)

# Extract the corresponding covariate values seperately for urban/rural

populationRaster = processedCovariateData[["population"]]
urbanizationRaster = croppedNotScaledUrbanization
elevationRaster = processedCovariateData[["elevation"]]
travelTimeRaster = processedCovariateData[["travelTime"]]
distRiverLakeRaster = processedCovariateData[["minDistRiverlakes"]]

# Minimum distance to rivers & lakes

minDistRiverLakeUrban <- raster::extract(distRiverLakeRaster, coordsUrbanKM, ncol=2)
minDistRiverLakeUrban[is.nan(minDistRiverLakeUrban)] = 0
minDistRiverLakeUrban[is.na(minDistRiverLakeUrban)] <- 0

minDistRiverLakeRural <- raster::extract(distRiverLakeRaster, coordsRuralKM, ncol=2)
minDistRiverLakeRural[is.nan(minDistRiverLakeRural)] = 0
minDistRiverLakeRural[is.na(minDistRiverLakeRural)] <- 0

###
# Population

populationUrban <- raster::extract(populationRaster, coordsUrbanKM, ncol=2)
populationUrban[is.nan(populationUrban)] = 0
populationUrban[is.na(populationUrban)] <- 0

populationRural <- raster::extract(populationRaster, coordsRuralKM, ncol=2)
populationRural[is.nan(populationRural)] = 0
populationRural[is.na(populationRural)] <- 0
# Urbanicity

urbanicityRural <- raster::extract(urbanizationRaster, coordsRuralKM, ncol=2)
urbanicityRural[is.nan(urbanicityRural)] = 0
urbanicityRural[is.na(urbanicityRural)] <- 0

urbanicityUrban <- raster::extract(urbanizationRaster, coordsUrbanKM, ncol=2)
urbanicityUrban[is.nan(urbanicityUrban)] = 0
urbanicityUrban[is.na(urbanicityUrban)] <- 0

# Elevation

elevationUrban <- raster::extract(elevationRaster, coordsUrbanKM, ncol=2)
elevationUrban[is.nan(elevationUrban)] = 0
elevationUrban[is.na(elevationUrban)] <- 0


elevationRural <- raster::extract(elevationRaster, coordsRuralKM, ncol=2)
elevationRural[is.nan(elevationRural)] = 0
elevationRural[is.na(elevationRural)] <- 0

# travel time 

accessCitiesUrban <- data.frame(raster::extract(travelTimeRaster, coordsUrbanKM, ncol=2))
accessCitiesUrban = accessCitiesUrban[,1]
accessCitiesUrban[is.nan(accessCitiesUrban)] = 0
accessCitiesUrban[is.na(accessCitiesUrban)] <- 0

accessCitiesRural <- data.frame(raster::extract(travelTimeRaster, coordsRuralKM, ncol=2))
accessCitiesRural = accessCitiesRural[,1]
accessCitiesRural[is.nan(accessCitiesRural)] = 0
accessCitiesRural[is.na(accessCitiesRural)] <- 0


nLoc_urban = length(coordsUrbanKM@coords[,1])
nLoc_rural = length(coordsRuralKM@coords[,1])

desMatrixJittUrban = cbind(rep(1, nLoc_urban), minDistRiverLakeUrban, accessCitiesUrban, elevationUrban, populationUrban, urbanicityUrban)
desMatrixJittRural = cbind(rep(1, nLoc_rural), minDistRiverLakeRural, accessCitiesRural, elevationRural, populationRural, urbanicityRural)

n_integrationPointsUrban = ncol(wUrban)
n_integrationPointsRural = ncol(wRural)


# # Construct projection matrices, and get other relevant info for TMB
out = makeJitterDataForTMB(intPointInfo, ys , urbanVals, ns, spdeMesh = mesh.s)
ysUrban = out$ysUrban
ysRural = out$ysRural
nsUrban = out$nsUrban
nsRural = out$nsRural
AUrban = out$AUrban
ARural = out$ARural


# TMB input for jittering accounted model
data <- list(num_iUrban = length(ysUrban),  # Total number of urban observations
             num_iRural = length(ysRural),  # Total number of rural observations
             num_s = mesh.s[['n']], # num. of vertices in SPDE mesh
             y_iUrban   = ysUrban, # num. of pos. urban obs in the cluster
             y_iRural   = ysRural, # num. of pos. rural obs in the cluster
             n_iUrban   = nsUrban,  # num. of urban exposures in the cluster
             n_iRural   = nsRural,  # num. of rural exposures in the cluster
             n_integrationPointsUrban = n_integrationPointsUrban,
             n_integrationPointsRural = n_integrationPointsRural,
             wUrban = wUrban,
             wRural = wRural,
             X_betaUrban = desMatrixJittUrban,
             X_betaRural = desMatrixJittRural,
             M0    = spde[['param.inla']][['M0']], # SPDE sparse matrix
             M1    = spde[['param.inla']][['M1']], # SPDE sparse matrix
             M2    = spde[['param.inla']][['M2']], # SPDE sparse matrix
             AprojUrban = AUrban,             # Projection matrix (urban)
             AprojRural = ARural,             # Projection matrix (rural)
             options = c(1, ## if 1, use normalization trick
                         1), ## if 1, run adreport
             # normalization flag.
             flag1 = 1,
             flag2 = flag2, #(0/1 for Gaussian/Binomial)
             beta_pri = c(0, 5), ## normal
             matern_pri = c(range, 0.5, USpatial = USpatial , alphaSpatial = alphaSpatial)
)


# prepare input for Smoothed Model

# # Extract the corresponding covariate values seperately for urban/rural
#
populationSmoothRaster = populationSmoothRaster
urbanizationSmoothRaster = smoothedRasters[["urbanizationSmoothRaster"]]
elevationSmoothRaster = smoothedRasters[["elevationSmoothRaster"]]
travelTimeSmoothRaster = smoothedRasters[["travelTimeSmoothRaster"]]
distRiverLakeSmoothRaster = smoothedRasters[["distRiverLakeSmoothRaster"]]

#
# # Minimum distance to rivers & lakes
#
minDistRiverLakeUrban <- raster::extract(distRiverLakeSmoothRaster, coordsUrbanKM, ncol=2)
minDistRiverLakeUrban[is.nan(minDistRiverLakeUrban)] = 0
minDistRiverLakeUrban[is.na(minDistRiverLakeUrban)] <- 0
# #
minDistRiverLakeRural <- raster::extract(distRiverLakeSmoothRaster, coordsRuralKM, ncol=2)
minDistRiverLakeRural[is.nan(minDistRiverLakeRural)] = 0
minDistRiverLakeRural[is.na(minDistRiverLakeRural)] <- 0
#
# # Population
#
populationUrban <- raster::extract(populationSmoothRaster, coordsUrbanKM, ncol=2)
populationUrban[is.nan(populationUrban)] = 0
populationUrban[is.na(populationUrban)] <- 0
# #
populationRural <- raster::extract(populationSmoothRaster, coordsRuralKM, ncol=2)
populationRural[is.nan(populationRural)] = 0
populationRural[is.na(populationRural)] <- 0
#
# # Urbanicity
#
urbanicityRural <- raster::extract(urbanizationSmoothRaster, coordsRuralKM, ncol=2)
urbanicityRural[is.nan(urbanicityRural)] = 0
urbanicityRural[is.na(urbanicityRural)] <- 0
# #
urbanicityUrban <- raster::extract(urbanizationSmoothRaster, coordsUrbanKM, ncol=2)
urbanicityUrban[is.nan(urbanicityUrban)] = 0
urbanicityUrban[is.na(urbanicityUrban)] <- 0
#
# # Elevation
#
elevationUrban <- raster::extract(elevationSmoothRaster, coordsUrbanKM, ncol=2)
elevationUrban[is.nan(elevationUrban)] = 0
elevationUrban[is.na(elevationUrban)] <- 0
# #
elevationRural <- raster::extract(elevationSmoothRaster, coordsRuralKM, ncol=2)
elevationRural[is.nan(elevationRural)] = 0
elevationRural[is.na(elevationRural)] <- 0
#
# # Access to the cities (predicted travel time in minutes to the nearest city)
#
accessCitiesUrban <- data.frame(raster::extract(travelTimeSmoothRaster, coordsUrbanKM, ncol=2))
accessCitiesUrban = accessCitiesUrban[,1]
accessCitiesUrban[is.nan(accessCitiesUrban)] = 0
accessCitiesUrban[is.na(accessCitiesUrban)] <- 0
# #
accessCitiesRural <- data.frame(raster::extract(travelTimeSmoothRaster, coordsRuralKM, ncol=2))
accessCitiesRural = accessCitiesRural[,1]
accessCitiesRural[is.nan(accessCitiesRural)] = 0
accessCitiesRural[is.na(accessCitiesRural)] <- 0
# #
# #
nLoc_urban = length(coordsUrbanKM@coords[,1])
nLoc_rural = length(coordsRuralKM@coords[,1])
# #
desMatrixJittUrban = cbind(rep(1, nLoc_urban), minDistRiverLakeUrban, accessCitiesUrban, elevationUrban, populationUrban, urbanicityUrban)
desMatrixJittRural = cbind(rep(1, nLoc_rural), minDistRiverLakeRural, accessCitiesRural, elevationRural, populationRural, urbanicityRural)
# #

# # Compile inputs for TMB
data_smoothed <- list(num_iUrban = length(ysUrban),  # Total number of urban observations
             num_iRural = length(ysRural),  # Total number of rural observations
             num_s = mesh.s[['n']], # num. of vertices in SPDE mesh
             y_iUrban   = ysUrban, # num. of pos. urban obs in the cluster
             y_iRural   = ysRural, # num. of pos. rural obs in the cluster
             n_iUrban   = nsUrban,  # num. of urban exposures in the cluster
             n_iRural   = nsRural,  # num. of rural exposures in the cluster
             n_integrationPointsUrban = n_integrationPointsUrban,
             n_integrationPointsRural = n_integrationPointsRural,
             wUrban = wUrban,
             wRural = wRural,
             X_betaUrban = desMatrixJittUrban,
             X_betaRural = desMatrixJittRural,
             M0    = spde[['param.inla']][['M0']], # SPDE sparse matrix
             M1    = spde[['param.inla']][['M1']], # SPDE sparse matrix
             M2    = spde[['param.inla']][['M2']], # SPDE sparse matrix
             AprojUrban = AUrban,             # Projection matrix (urban)
             AprojRural = ARural,             # Projection matrix (rural)
             options = c(1, ## if 1, use normalization trick
                         1), ## if 1, run adreport
             # normalization flag.
             flag1 = 1,
             flag2 = flag2, #(0/1 for Gaussian/Binomial)
             beta_pri = c(0, 5), ## normal
             matern_pri = c(rangeMaternPri, 0.5, USpatial = USpatial , alphaSpatial = alphaSpatial)
)

# ########################################


tmb_params <- list(beta=rep(0, 6),
                   #log_tau = 5, # Log tau (i.e. log spatial precision, Epsilon)
                   log_tau = 2.74, # Log tau (i.e. log spatial precision, Epsilon)
                   log_kappa = -4, # SPDE parameter related to the range
                   Epsilon_s = rep(0, mesh.s[['n']])#,  RE on mesh vertices
                   #log_nug_std = log(sqrt(0.1))
)



# random effects
rand_effs <- c("Epsilon_s", "beta")


#Extract covariate values for the prediction locations

predCoordsKM = SpatialPoints(predCoords, proj4string = CRS("+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"), bbox = NULL)

#1.) Elevation

elevationAtPred <- raster::extract(elevationRaster, predCoordsKM, ncol=2)
elevationAtPred[is.nan(elevationAtPred)] = 0
elevationAtPred[is.na(elevationAtPred)] <- 0
covariatesAtPred = data.frame(elevationAtPred = elevationAtPred)


#2.) Distance from rivers and lakes

minDistRiverLakeAtPred <- raster::extract(distRiverLakeRaster, predCoordsKM, ncol=2)
minDistRiverLakeAtPred[is.nan(minDistRiverLakeAtPred)] = 0
minDistRiverLakeAtPred[is.na(minDistRiverLakeAtPred)] <- 0
covariatesAtPred$minDistRiverLakeAtPred = minDistRiverLakeAtPred

#3.) Population

popAtPred <- raster::extract(populationRaster, predCoordsKM, ncol=2)
popAtPred[is.nan(popAtPred)] = 0
popAtPred[is.na(popAtPred)] <- 0
covariatesAtPred$popAtPred <- popAtPred

# #4.) Urbanicity
#
urbanicityAtPred <- raster::extract(urbanizationRaster, predCoordsKM, ncol=2)
urbanicityAtPred[is.na(urbanicityAtPred)] <- 0
urbanicityAtPred[is.nan(urbanicityAtPred)] = 0
covariatesAtPred$urbanicityAtPred <- urbanicityAtPred


#5.) TRAVEL TIME DATA

accessCitiesAtpred <- raster::extract(travelTimeRaster, predCoordsKM, ncol=2)
accessCitiesAtpred[is.nan(accessCitiesAtpred)] = 0
accessCitiesAtpred[is.na(accessCitiesAtpred)] <- 0
covariatesAtPred$accessCitiesAtpred = accessCitiesAtpred

covariatesAtPred = cbind(rep(1, length(minDistRiverLakeAtPred)),
                         covariatesAtPred$minDistRiverLakeAtPred,
                         covariatesAtPred$accessCitiesAtpred,
                         covariatesAtPred$elevationAtPred,
                         covariatesAtPred$popAtPred,
                         covariatesAtPred$urbanicityAtPred)


# Extract smoothed covariate values at the prediction locations


# Elevation

elevationAtPredSmoothed <- raster::extract(elevationSmoothRaster, predCoordsKM, ncol=2)
elevationAtPredSmoothed[is.nan(elevationAtPredSmoothed)] = 0
elevationAtPredSmoothed[is.na(elevationAtPredSmoothed)] <- 0
covariatesAtPredSmoothed = data.frame(elevationAtPredSmoothed = elevationAtPredSmoothed)

# Distance from rivers and lakes

minDistRiverLakeAtPredSmoothed <- raster::extract(distRiverLakeSmoothRaster, predCoordsKM, ncol=2)
minDistRiverLakeAtPredSmoothed[is.nan(minDistRiverLakeAtPredSmoothed)] = 0
minDistRiverLakeAtPredSmoothed[is.na(minDistRiverLakeAtPredSmoothed)] <- 0
covariatesAtPredSmoothed$minDistRiverLakeAtPredSmoothed = minDistRiverLakeAtPredSmoothed

# Population

popAtPredSmoothed <- raster::extract(populationSmoothRaster, predCoordsKM, ncol=2)
popAtPredSmoothed[is.nan(popAtPredSmoothed)] = 0
popAtPredSmoothed[is.na(popAtPredSmoothed)] <- 0
covariatesAtPredSmoothed$popAtPredSmoothed <- popAtPredSmoothed

# Urbanicity

urbanicityAtPredSmoothed <- raster::extract(urbanizationSmoothRaster, predCoordsKM, ncol=2)
urbanicityAtPredSmoothed[is.na(urbanicityAtPredSmoothed)] <- 0
urbanicityAtPredSmoothed[is.nan(urbanicityAtPredSmoothed)] = 0
covariatesAtPredSmoothed$urbanicityAtPredSmoothed <- urbanicityAtPredSmoothed

# Travel time

accessCitiesAtpredSmoothed <- raster::extract(travelTimeSmoothRaster, predCoordsKM, ncol=2)
accessCitiesAtpredSmoothed[is.nan(accessCitiesAtpredSmoothed)] = 0
accessCitiesAtpredSmoothed[is.na(accessCitiesAtpredSmoothed)] <- 0
covariatesAtPredSmoothed$accessCitiesAtpredSmoothed = accessCitiesAtpredSmoothed

covariatesAtPredSmoothed = cbind(rep(1, length(popAtPredSmoothed)),
                                 covariatesAtPredSmoothed$minDistRiverLakeAtPredSmoothed,
                                 covariatesAtPredSmoothed$accessCitiesAtpredSmoothed,
                                 covariatesAtPredSmoothed$elevationAtPredSmoothed,
                                 covariatesAtPredSmoothed$popAtPredSmoothed,
                                 covariatesAtPredSmoothed$urbanicityAtPredSmoothed)


#Run TMB

dataList = data

##############################################################
# jittering is not accounted for
dataList[["flagRandomField"]] = 0
dataList[["flagCovariates"]] = 0


obj <- MakeADFun(data=dataList,
                 parameters=tmb_params,
                 random = rand_effs,
                 hessian=TRUE,
                 DLL='simulations')

obj <- normalize(obj, flag="flag1", value = 0)

opt0 = optim(par=obj$par, fn = obj$fn, gr = obj$gr,
             method = c("BFGS"), hessian = FALSE, control=list(parscale=c(.1, .1)))

par <- obj$env$last.par
Qtest = obj$env$spHess(par, random = TRUE)

#Sampling
#mu <- c(SD0$par.fixed,SD0$par.random)
mu <- c(par[-c(7,8)])  # 7 and 8 are log_kappa and log_tau


# Simulate draws
rmvnorm_prec <- function(mu, chol_prec, n.sims) {
  z <- matrix(rnorm(length(mu) * n.sims), ncol=n.sims)
  L <- chol_prec #Cholesky(prec, super=TRUE)
  z <- Matrix::solve(L, z, system = "Lt") ## z = Lt^-1 %*% z
  z <- Matrix::solve(L, z, system = "Pt") ## z = Pt    %*% z
  z <- as.matrix(z)
  mu + z
}
#

prec = Qtest
L = Cholesky(prec, super = T)
#
A.pred = inla.spde.make.A(mesh = mesh.s, loc = predCoords)
#
t.draws <- rmvnorm_prec(mu = mu , chol_prec = L, n.sims = 1000)

parnames <- c(names(mu))

epsilon_draws  <- t.draws[parnames == 'Epsilon_s',]
beta_draws<- t.draws[parnames == 'beta',]

eta.samples = covariatesAtPred%*%beta_draws + as.matrix(A.pred%*%epsilon_draws)
#
# convert to probability scale
eta.samples = expit(eta.samples)

PredictedResponses <- cbind(mean = (apply(eta.samples, 1, mean)),
                            median = (apply(eta.samples, 1, median)),
                            sd     = (apply(eta.samples, 1, sd)),
                            lower = (apply(eta.samples, 1, quantile, .025)),
                            upper = (apply(eta.samples, 1, quantile, .975)))

#
intercept = beta_draws[1,]
beta_distRiversLakes = beta_draws[2,]
beta_accessCities = beta_draws[3,]
beta_elevation = beta_draws[4,]
beta_population = beta_draws[5,]
beta_urbanicity = beta_draws[6,]
# #
# #
SampledParameters = cbind(median = c(intercept = median(intercept), beta_distRiversLakes = median(beta_distRiversLakes), beta_accessCities = median(beta_accessCities),
                                     beta_elevation = median(beta_elevation), beta_population = median(beta_population), beta_urbanicity = median(beta_urbanicity)),
                          lower = c(quantile(intercept, probs = 0.025), quantile(beta_distRiversLakes, probs = 0.025), quantile(beta_accessCities, probs = 0.025),
                                    quantile(beta_elevation, probs = 0.025), quantile(beta_population, probs = 0.025), quantile(beta_urbanicity, probs = 0.025)),
                          upper = c(quantile(intercept, probs = 0.975), quantile(beta_distRiversLakes, probs = 0.975), quantile(beta_accessCities, probs = 0.975),
                                    quantile(beta_elevation, probs = 0.975), quantile(beta_population, probs = 0.975), quantile(beta_urbanicity, probs = 0.975)),
                          length = c((quantile(intercept, probs = 0.975)- c(quantile(intercept, probs = 0.025))), (quantile(beta_distRiversLakes, probs = 0.975)- quantile(beta_distRiversLakes, probs = 0.025)),
                                     (quantile(beta_accessCities, probs = 0.975) - quantile(beta_accessCities, probs = 0.025)), (quantile(beta_elevation, probs = 0.975) - quantile(beta_elevation, probs = 0.025)),
                                     (quantile(beta_population, probs = 0.975) - quantile(beta_population, probs = 0.025)),(quantile(beta_urbanicity, probs = 0.975) - quantile(beta_urbanicity, probs = 0.025))))


Results_NN = list(SampledParameters = SampledParameters,
                  PredictedResponses = PredictedResponses,
                  prec = prec,
                  mu = mu,
                  fixed.par = par[c(7,8)],
                  eta.samples = eta.samples)

save(Results_NN, file = "realData_Results_NN.RData")

##############################################################

# smoothed covariates

dataList = data_smoothed

dataList[["flagRandomField"]] = 0
dataList[["flagCovariates"]] = 0


obj <- MakeADFun(data=dataList,
                 parameters=tmb_params,
                 random = rand_effs,
                 hessian=TRUE,
                 DLL='simulations')

obj <- normalize(obj, flag="flag1", value = 0)

opt0 = optim(par=obj$par, fn = obj$fn, gr = obj$gr,
             method = c("BFGS"), hessian = FALSE, control=list(parscale=c(.1, .1)))

par <- obj$env$last.par
Qtest = obj$env$spHess(par, random = TRUE)

#Sampling
#mu <- c(SD0$par.fixed,SD0$par.random)
mu <- c(par[-c(7,8)])  # 7 and 8 are log_kappa and log_tau


# Simulate draws
rmvnorm_prec <- function(mu, chol_prec, n.sims) {
  z <- matrix(rnorm(length(mu) * n.sims), ncol=n.sims)
  L <- chol_prec #Cholesky(prec, super=TRUE)
  z <- Matrix::solve(L, z, system = "Lt") ## z = Lt^-1 %*% z
  z <- Matrix::solve(L, z, system = "Pt") ## z = Pt    %*% z
  z <- as.matrix(z)
  mu + z
}
#

prec = Qtest
L = Cholesky(prec, super = T)
#
A.pred = inla.spde.make.A(mesh = mesh.s, loc = predCoords)
#
t.draws <- rmvnorm_prec(mu = mu , chol_prec = L, n.sims = 1000)

parnames <- c(names(mu))

epsilon_draws  <- t.draws[parnames == 'Epsilon_s',]
beta_draws<- t.draws[parnames == 'beta',]

eta.samples = covariatesAtPred%*%beta_draws + as.matrix(A.pred%*%epsilon_draws)
#
# convert to probability scale
eta.samples = expit(eta.samples)

PredictedResponses <- cbind(mean = (apply(eta.samples, 1, mean)),
                            median = (apply(eta.samples, 1, median)),
                            sd     = (apply(eta.samples, 1, sd)),
                            lower = (apply(eta.samples, 1, quantile, .025)),
                            upper = (apply(eta.samples, 1, quantile, .975)))

#
intercept = beta_draws[1,]
beta_distRiversLakes = beta_draws[2,]
beta_accessCities = beta_draws[3,]
beta_elevation = beta_draws[4,]
beta_population = beta_draws[5,]
beta_urbanicity = beta_draws[6,]
# #
# #
SampledParameters = cbind(median = c(intercept = median(intercept), beta_distRiversLakes = median(beta_distRiversLakes), beta_accessCities = median(beta_accessCities),
                                     beta_elevation = median(beta_elevation), beta_population = median(beta_population), beta_urbanicity = median(beta_urbanicity)),
                          lower = c(quantile(intercept, probs = 0.025), quantile(beta_distRiversLakes, probs = 0.025), quantile(beta_accessCities, probs = 0.025),
                                    quantile(beta_elevation, probs = 0.025), quantile(beta_population, probs = 0.025), quantile(beta_urbanicity, probs = 0.025)),
                          upper = c(quantile(intercept, probs = 0.975), quantile(beta_distRiversLakes, probs = 0.975), quantile(beta_accessCities, probs = 0.975),
                                    quantile(beta_elevation, probs = 0.975), quantile(beta_population, probs = 0.975), quantile(beta_urbanicity, probs = 0.975)),
                          length = c((quantile(intercept, probs = 0.975)- c(quantile(intercept, probs = 0.025))), (quantile(beta_distRiversLakes, probs = 0.975)- quantile(beta_distRiversLakes, probs = 0.025)),
                                     (quantile(beta_accessCities, probs = 0.975) - quantile(beta_accessCities, probs = 0.025)), (quantile(beta_elevation, probs = 0.975) - quantile(beta_elevation, probs = 0.025)),
                                     (quantile(beta_population, probs = 0.975) - quantile(beta_population, probs = 0.025)),(quantile(beta_urbanicity, probs = 0.975) - quantile(beta_urbanicity, probs = 0.025))))


Results_NN = list(SampledParameters = SampledParameters,
                  PredictedResponses = PredictedResponses,
                  prec = prec,
                  mu = mu,
                  fixed.par = par[c(7,8)],
                  eta.samples = eta.samples)

save(Results_NN, file = "realData_Results_NNsmoothed.RData")


##############################################################

# accounting for jittering in covariates and random effect
dataList[["flagRandomField"]] = 1
dataList[["flagCovariates"]] = 1

start_time_optimization <- Sys.time()

obj <- MakeADFun(data=dataList,
                 parameters=tmb_params,
                 random = rand_effs,
                 hessian=TRUE,
                 DLL='simulations')

obj <- normalize(obj, flag="flag1", value = 0)

opt0 = optim(par=obj$par, fn = obj$fn, gr = obj$gr,
             method = c("BFGS"), hessian = FALSE, control=list(parscale=c(.1, .1)))

par <- obj$env$last.par
Qtest = obj$env$spHess(par, random = TRUE)

end_time_optimization <- Sys.time()

runTime_optimization = end_time_optimization - start_time_optimization


#Sampling
start_time_sampling <- Sys.time()

mu <- c(par[-c(7,8)])  # 7 and 8 are log_kappa and log_tau

# Simulate draws
rmvnorm_prec <- function(mu, chol_prec, n.sims) {
  z <- matrix(rnorm(length(mu) * n.sims), ncol=n.sims)
  L <- chol_prec #Cholesky(prec, super=TRUE)
  z <- Matrix::solve(L, z, system = "Lt") ## z = Lt^-1 %*% z
  z <- Matrix::solve(L, z, system = "Pt") ## z = Pt    %*% z
  z <- as.matrix(z)
  mu + z
}


prec = Qtest
L = Cholesky(prec, super = T)
#
A.pred = inla.spde.make.A(mesh = mesh.s, loc = predCoords)
#
t.draws <- rmvnorm_prec(mu = mu , chol_prec = L, n.sims = 1000)

parnames <- c(names(mu))

epsilon_draws  <- t.draws[parnames == 'Epsilon_s',]
beta_draws<- t.draws[parnames == 'beta',]

eta.samples = covariatesAtPred%*%beta_draws + as.matrix(A.pred%*%epsilon_draws)
#
# convert to probability scale
eta.samples = expit(eta.samples)

PredictedResponses <- cbind(mean = (apply(eta.samples, 1, mean)),
                            median = (apply(eta.samples, 1, median)),
                            sd     = (apply(eta.samples, 1, sd)),
                            lower = (apply(eta.samples, 1, quantile, .025)),
                            upper = (apply(eta.samples, 1, quantile, .975)))

#
intercept = beta_draws[1,]
beta_distRiversLakes = beta_draws[2,]
beta_accessCities = beta_draws[3,]
beta_elevation = beta_draws[4,]
beta_population = beta_draws[5,]
beta_urbanicity = beta_draws[6,]
# #

SampledParameters = cbind(median = c(intercept = median(intercept), beta_distRiversLakes = median(beta_distRiversLakes), beta_accessCities = median(beta_accessCities),
                                     beta_elevation = median(beta_elevation), beta_population = median(beta_population), beta_urbanicity = median(beta_urbanicity)),
                          lower = c(quantile(intercept, probs = 0.025), quantile(beta_distRiversLakes, probs = 0.025), quantile(beta_accessCities, probs = 0.025),
                                    quantile(beta_elevation, probs = 0.025), quantile(beta_population, probs = 0.025), quantile(beta_urbanicity, probs = 0.025)),
                          upper = c(quantile(intercept, probs = 0.975), quantile(beta_distRiversLakes, probs = 0.975), quantile(beta_accessCities, probs = 0.975),
                                    quantile(beta_elevation, probs = 0.975), quantile(beta_population, probs = 0.975), quantile(beta_urbanicity, probs = 0.975)),
                          length = c((quantile(intercept, probs = 0.975)- c(quantile(intercept, probs = 0.025))), (quantile(beta_distRiversLakes, probs = 0.975)- quantile(beta_distRiversLakes, probs = 0.025)),
                                     (quantile(beta_accessCities, probs = 0.975) - quantile(beta_accessCities, probs = 0.025)), (quantile(beta_elevation, probs = 0.975) - quantile(beta_elevation, probs = 0.025)),
                                     (quantile(beta_population, probs = 0.975) - quantile(beta_population, probs = 0.025)),(quantile(beta_urbanicity, probs = 0.975) - quantile(beta_urbanicity, probs = 0.025))))


Results_CR = list(SampledParameters = SampledParameters,
                  PredictedResponses = PredictedResponses,
                  prec = prec,
                  mu = mu,
                  fixed.par = par[c(7,8)],
                  eta.samples = eta.samples)

end_time_sampling <- Sys.time()

runTime_sampling  = end_time_sampling  - start_time_sampling


save(Results_CR, file = "realData_Results_CR.RData")

# Results_RealData = list(Results_NN = Results_NN,
#                         Results_NNsmoothed = Results_NNsmoothed,
#                         Results_CR = Results_CR)
# 
# save(Results_RealData, file = "")

quit()
