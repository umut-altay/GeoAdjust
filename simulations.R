
## 1.) Initialization ----
#  * Load libraries ----
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

#  * Load functions ----
source("modSPDEJitter.R")
source("makeIntegrationPoints.R")
source('functions.R')
load("") # load the parameter estimates from your analysis on real data (with
         # the adjusted model). Estimated log_tau and log_kappa will be used to 
         # calculate rangeSc and sigma.sim

#set the seeds
set.seed(2345)

# * Setup ----

# Spatial Range (in kilometers)
rangeSc = sqrt(8.0)/exp(log_kappa)

# Marginal variance
sigma.sim = 1.0 / sqrt(4.0 * 3.14159265359 *
                         exp(2.0 * log_tau) * exp(2.0 * log_kappa))


rangeMaternPri =160 # the range value that will be first passed into tmbInput, and then into 
                    # the data object of TMB, and will be used as input of C++ for constructing 
                    # the PC-priors

# number of unique sub-integration point radii and angles per integration point 
# (i.e. the number of sub-integration points per integration point is 
# nSubRPerPoint * nSubAPerPoint)
nSubRPerPoint = 10
nSubAPerPoint = 10

# Jittering Factor
scaleSc = c(1)  # the default max. DHS jittering distances can be scaled and tested using this
                # currently it is scaled by 1 (non-scaled)

#Likelihood :
likelihoodSc = as.matrix(rbind(likelihood = c(1), nuggetVar = c(0), p = c(0.5)))

# Provincial Boundaries : # respecting admin1 area boundaries while jittering (or not)
boundarySc = TRUE         

# number of simulations per scenario 
# (each combination of boundarySc, likelihoodSc, scaleSc and rangeSc 
#  corresponds to one scenario)
nSim = 50

## 2.) Geography and demography data----

# Load and prepare the geography
# Read the maps
NGA_1 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                            layer = "gadm40_NGA_1")

NGA_0 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                            layer = "gadm40_NGA_0")

educationData = read_dta("NGIR7BDT/NGIR7BFL.DTA")

# Read the coordinates
corList = readOGR(dsn = "dataFiles/DHS/NG_2018_DHS_02242022_98_147470/NGGE7BFL",
                             layer = "NGGE7BFL")

smallGeo = data.frame(clusterIdx = corList$DHSCLUST, urban = corList$URBAN_RURA,
                      long = corList$LONGNUM, lat = corList$LATNUM,
                      admin1 = corList$ADM1NAME)

myData = data.frame(clusterIdx = educationData$v001, householdIdx = educationData$v002,
                    stratum = educationData$v023, 
                    age = educationData$v012,
                    secondaryEducation = educationData$v106)

# Subset the data for age and secondary education completion :
myData = subset(myData, age <= 39 & age >=20)
myData$ys = (myData$secondaryEducation>=2)+0

myData = merge(myData, smallGeo, by = "clusterIdx")

# Add number of trials
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

#urbanRural from DHS data set
nigeria.data$urbanRuralDHS = corList@data[["URBAN_RURA"]]

# Remove NA rows
nigeria.data = nigeria.data[complete.cases(nigeria.data$ys), ]
nigeria.data = nigeria.data[complete.cases(nigeria.data$long), ]
nigeria.data = nigeria.data[complete.cases(nigeria.data$lat), ]

# Create point in polygon table (object "check1") to check against new locations
true_latLon = cbind(nigeria.data[,"long"], nigeria.data[,"lat"])
colnames(true_latLon) = c("long", "lat")
true_latLon = SpatialPoints(true_latLon, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"), bbox = NULL)

NGA_1@data[["OBJECTID"]] =1:37   # add an index for number of admin areas in it
check1 <- over(true_latLon, NGA_1, returnList = FALSE)

#Drop rows which doesn't match with admin areas. Jittering algorithm will need them to match.
which(is.na(check1$NAME_1))

# > which(is.na(check1$NAME_1))
# [1]   48  122  205  848  857 1116 1122 1287 1328

nigeria.data = nigeria.data[-c(48, 122,  205,  848,  857, 1116, 1122, 1287, 1328),]

check1 = check1[-c(48, 122,  205,  848,  857, 1116, 1122, 1287, 1328),]

nigeria.data$east = rep(NA, length(nigeria.data$long))
nigeria.data$north = rep(NA, length(nigeria.data$long)) 

# Assign coordinates in kilometers to the clusters
nigeria.data[,c("east", "north")] = convertDegToKM(nigeria.data[,c("long", "lat")])
locKM = nigeria.data[,c("east", "north")]

# Prediction locations (follow either the first or the second approach)

# 1.) Choosing 1000 locations randomly from a grid :
xx = seq(min(nigeria.data$long), max(nigeria.data$long), length.out = 50)
yy = seq(min(nigeria.data$lat), max(nigeria.data$lat), length.out = 50)
loc.pred = cbind(rep(xx, each = length(yy)), rep(yy, length(xx)))

#Convert prediction grid into a SpatialPointsDataFrame object
grid=data.frame(xCoor = loc.pred[,1], yCoor = loc.pred[, 2])

grid <- SpatialPointsDataFrame(grid, data.frame(id=1:2500)) #we have 2500 points in the initial grid
grid@proj4string@projargs = NGA_0@proj4string@projargs


#Remove the locations that are outside Kenya
grid=erase.point(grid, NGA_0, inside = FALSE)

#Remove the points that are inside Lake Chad
grid=erase.point(grid, NGA_2[160,], inside = TRUE)

# #Make a new loc.pred matrix from the remaining points
loc.pred = cbind(grid@coords[ ,1], grid@coords[ ,2])

# Select certain number (1000 here) of prediction locations
nPred = 1000
idx = sample.int(dim(loc.pred)[1], size = nPred)
loc.pred = loc.pred[idx,]

loc.pred = data.frame(long = loc.pred[,1], lat = loc.pred[,2], east = rep(NA, nPred), north = rep(NA, nPred))

loc.pred[,c("east", "north")] = convertDegToKM(loc.pred[,c("long", "lat")])

predCoords = cbind(loc.pred$east, loc.pred$north) #prediction locations

#2.) Constructing a prediction raster and predicting at cell centers :

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
predCoords = xyFromCell(predRaster, cell = idx) # prediction locations

## 3.) Simulations ----

#betas = estimated intercept and the covariate coefficients from Adjusted model on real data

# > betas
# beta        beta        beta        beta        beta        beta 
# -2.04264031  0.71385960 -0.39465673  0.02686258  0.42973156 -1.59457093 

betas = rbind(Results_RealData[["Results_CR"]][["SD0"]][["par.random"]][1:6]/2,  #scaled by 0.5
               Results_RealData[["Results_CR"]][["SD0"]][["par.random"]][1:6],   #scaled by 1 (non-scaled)
               Results_RealData[["Results_CR"]][["SD0"]][["par.random"]][1:6]*2) #scaled by 2
               
# settings
intercept = logit(0.5)
sigma.sim =        #marginal variance obtained from the adjusted model on real data
#>sigma.sim
# [1] 1.734917


simLoc = rbind(as.matrix(nigeria.data[, c("east", "north")]),
               predCoords)

ns = nigeria.data$ns

# Load the covariate rasters and smoothed rasters:
load("")

#simulate the responses and spatial field

simulatedData = list()
for(k in 1:nrow(betas)){
  admin1Boundaries = list()
  for (g in 1:length(boundarySc)){
    likelihoodType = list()
    for (i in 1:length(likelihoodSc[1,])){
      spatialRange =list()
      for (j in 1:1){
        jitteringFactor = list()
        for (h in 1:length(scaleSc)){
          simulationNumber = list()
          for(l in 1:nSim){
            
            sigma.cluster.sim = sqrt(likelihoodSc[[2, i]])
            range.sim = rangeSc[[j]]
            flag2 = likelihoodSc[[1, i]]
            betaSim = betas[k,]
            
            sim.data = simulateResponses(nObs = length(nigeria.data[,1]),
                                         loc = simLoc,
                                         ns = ns,
                                         betaSim = betaSim,
                                         space.range = range.sim,
                                         space.sigma = sigma.sim,
                                         gauss.sim = sigma.cluster.sim,
                                         covariateData = processedCovariateData,
                                         mainData = nigeria.data,
                                         flag2 = flag2)
            
            simulationNumber[[paste("Simulation", l)]] = sim.data
          }
          #
          if (h==1){
            jitteringFactor[["jitteringFactor = 1"]] = simulationNumber
          }else {
            jitteringFactor[["jitteringFactor = 4"]] = simulationNumber
          }
          #
          
        }
        #
        if (j==1){
          spatialRange[["range from RealData"]] = jitteringFactor
        }else {
          spatialRange[["range = 340 km"]]  = jitteringFactor
        }
        #
      }
      #
      if (i==1){
        likelihoodType[["binomial likelihood"]] = spatialRange
      }else {
        likelihoodType[["Gaussian likelihood"]] = spatialRange
      }
      #
    }
    #
    if (boundarySc==TRUE){
      admin1Boundaries[["Borders respected"]] = likelihoodType
    }else {
      admin1Boundaries[["Borders not respected"]] = likelihoodType
    }
    #
  }
  #
  if (k==1){
    simulatedData[["scaledBetasby0.5"]] = admin1Boundaries
  }else if (k==2){
    simulatedData[["notScaled"]]  = admin1Boundaries
  }else{
    simulatedData[["scaledBetasby2"]]  = admin1Boundaries
  }
  #
}

# create the mesh
mesh.s <- inla.mesh.2d(loc.domain = cbind(nigeria.data$east, nigeria.data$north),
                       n=5000, 
                       max.n=10000,
                       offset=-.08,
                       cutoff=4, 
                       max.edge=c(25, 50))


# 4.) Compile .cpp files----

# standard and jittering accounted models in one file
compile( "simulations.cpp")
dyn.load( dynlib("simulations") )

# 5.) Prepare the inputs ----
# for model fitting with TMB

inputs = list()
for(k in 1:nrow(betas)){
  admin1Boundaries = list()
  for (g in 1:length(boundarySc)){
    likelihoodType = list()
    for (i in 1:length(likelihoodSc[1,])){
      spatialRange =list()
      for (j in 1:length(rangeSc)){
        jitteringFactor = list()
        for (h in 1:length(scaleSc)){
          simulationNumber = list()
          for(l in 1:nSim){
            
            boundary = boundarySc[[g]]
            jScale = scaleSc[[h]]
            flag2 = likelihoodSc[[1, i]]
            sigma.cluster.sim = sqrt(likelihoodSc[[2, i]])
            range.sim = rangeSc[[j]]
            
            
            modelParams = list(intercept = intercept,
                               range.sim = range.sim,  #in kilometers
                               sigma.sim = sigma.sim,
                               sigma.cluster.sim = sigma.cluster.sim)
            
            otherValues = list(USpatial = 1,
                               alphaSpatial = 0.05,
                               flag2 = flag2,
                               jScale = jScale)
            
            # TMB Input
            locObs = Displace(scale = jScale,        # randomly displace (jitter)
                              locKM = locKM,         # the coordinates
                              urbanRural = nigeria.data$urbanRuralDHS,
                              Admin1ShapeFile = NGA_1,
                              check1 = check1,
                              boundary = boundary)
            
            
            locObs =cbind(locObs[[1]][["east"]], locObs[[1]][["north"]])
            
            if(boundarySc) {
              thisMap = NGA_1
            } else {
              thisMap = NULL
            }
            
            # data
            tmbInput = prepare_input(sim.data = simulatedData[[k]][[g]][[i]][[j]][[h]][[l]], 
                                     locObs = locObs, 
                                     modelParams = modelParams, 
                                     otherValues = otherValues, 
                                     jScale = jScale, 
                                     urban = nigeria.data$urbanRuralDHS,
                                     mesh.s = mesh.s, 
                                     adminMap = thisMap, 
                                     nSubRPerPoint=nSubRPerPoint, 
                                     nSubAPerPoint=nSubAPerPoint,
                                     covariateData = processedCovariateData,
                                     ns=ns,
                                     rangeMaternPri = rangeMaternPri
            )
            
            
            tmbInputSmoothed = prepare_inputSmoothed(sim.data = simulatedData[[k]][[g]][[i]][[j]][[h]][[l]], 
                                     locObs = locObs, 
                                     modelParams = modelParams, 
                                     otherValues = otherValues, 
                                     jScale = jScale, 
                                     urban = nigeria.data$urbanRuralDHS,
                                     mesh.s = mesh.s, 
                                     adminMap = thisMap, 
                                     nSubRPerPoint=nSubRPerPoint, 
                                     nSubAPerPoint=nSubAPerPoint,
                                     covariateData = smoothedRasters,
                                     populationSmoothRaster = populationSmoothRaster,
                                     ns=ns,
                                     rangeMaternPri = rangeMaternPri
            )
            
            tmbInputSmoothed$locObs = locObs
            tmbInput$locObs = locObs
            simulationNumber[[paste("Simulation", l, "inputs")]] = list(tmbInput = tmbInput,
                                                                        tmbInputSmoothed = tmbInputSmoothed)
            #simulationNumber[[paste("Simulation", l, "locations")]] = locObs
          }
          #
          if (h==1){
            jitteringFactor[["jitteringFactor = 1"]] = simulationNumber
          }else {
            jitteringFactor[["jitteringFactor = 4"]] = simulationNumber
          }
          #
          
        }
        #
        if (j==1){
          spatialRange[["range from RealData"]] = jitteringFactor
        }else {
          spatialRange[["range = 340 km"]]  = jitteringFactor
        }
        #
      }
      #
      if (i==1){
        likelihoodType[["binomial likelihood"]] = spatialRange
      }else {
        likelihoodType[["Gaussian likelihood"]] = spatialRange
      }
      #
    }
    #
    if (boundarySc==TRUE){
      admin1Boundaries[["Borders respected"]] = likelihoodType
    }else {
      admin1Boundaries[["Borders not Respected"]] = likelihoodType
    }
    #
  }
  #
  if (k==1){
    inputs[["scaledBetasby0.5"]] = admin1Boundaries
  }else if (k==2){
    inputs[["notScaled"]]  = admin1Boundaries
  }else{
    inputs[["scaledBetasby2"]]  = admin1Boundaries
  }
  #
}        


# parameters
tmb_params <- list(beta=rep(0, 6),
                   log_tau =   , # log_tau value estimated from the real data using adjusted model
                                 # Log tau (i.e. log spatial precision, Epsilon)
                   log_kappa = , # log_kappa value estimated from the real data using adjusted model
                                 # log_kappa (SPDE parameter related to the range)
                   Epsilon_s = rep(0, mesh.s[['n']]) #,  Random effect on mesh vertices
                   #log_nug_std = log(sqrt(0.1)) # can be used when nugget is included
)


# random effects
rand_effs <- c("Epsilon_s", "beta")


nLoc = length(nigeria.data$east)

# Extract covariate values at the prediction locations

locPredDegree = cbind(loc.pred$long, loc.pred$lat)
predCoordsDegrees = SpatialPoints(locPredDegree, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"), bbox = NULL)

populationRaster = processedCovariateData[["population"]]
urbanizationRaster = croppedNotScaledUrbanization
elevationRaster = processedCovariateData[["elevation"]]
travelTimeRaster = processedCovariateData[["travelTime"]]
distRiverLakeRaster = processedCovariateData[["minDistRiverlakes"]]

# Elevation

elevationAtPred <- raster::extract(elevationRaster, predCoordsDegrees, ncol=2)
elevationAtPred[is.nan(elevationAtPred)] = 0
elevationAtPred[is.na(elevationAtPred)] <- 0
covariatesAtPred = data.frame(elevationAtPred = elevationAtPred)

# Distance from rivers and lakes

minDistRiverLakeAtPred <- raster::extract(distRiverLakeRaster, predCoordsDegrees, ncol=2)
minDistRiverLakeAtPred[is.nan(minDistRiverLakeAtPred)] = 0
minDistRiverLakeAtPred[is.na(minDistRiverLakeAtPred)] <- 0
covariatesAtPred$minDistRiverLakeAtPred = minDistRiverLakeAtPred

# Population

popAtPred <- raster::extract(populationRaster, predCoordsDegrees, ncol=2)
popAtPred[is.nan(popAtPred)] = 0
popAtPred[is.na(popAtPred)] <- 0
covariatesAtPred$popAtPred <- popAtPred

# Urbanicity

urbanicityAtPred <- raster::extract(urbanizationRaster, predCoordsDegrees, ncol=2)
urbanicityAtPred[is.na(urbanicityAtPred)] <- 0
urbanicityAtPred[is.nan(urbanicityAtPred)] = 0
covariatesAtPred$urbanicityAtPred <- urbanicityAtPred

# Travel time

accessCitiesAtpred <- raster::extract(travelTimeRaster, predCoordsDegrees, ncol=2)
accessCitiesAtpred[is.nan(accessCitiesAtpred)] = 0
accessCitiesAtpred[is.na(accessCitiesAtpred)] <- 0
covariatesAtPred$accessCitiesAtpred = accessCitiesAtpred

covariatesAtPred = cbind(rep(1, length(popAtPred)),
                         covariatesAtPred$minDistRiverLakeAtPred,
                         covariatesAtPred$accessCitiesAtpred,
                         covariatesAtPred$elevationAtPred,
                         covariatesAtPred$popAtPred,
                         covariatesAtPred$urbanicityAtPred)


# Extract smoothed covariate values at the prediction locations

locPredDegree = cbind(loc.pred$long, loc.pred$lat)
predCoordsDegrees = SpatialPoints(locPredDegree, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"), bbox = NULL)

populationSmoothRaster = populationSmoothRaster
urbanizationSmoothRaster = smoothedRasters[["urbanizationSmoothRaster"]]
elevationSmoothRaster = smoothedRasters[["elevationSmoothRaster"]]
travelTimeSmoothRaster = smoothedRasters[["travelTimeSmoothRaster"]]
distRiverLakeSmoothRaster = smoothedRasters[["distRiverLakeSmoothRaster"]]

# Elevation

elevationAtPredSmoothed <- raster::extract(elevationSmoothRaster, predCoordsDegrees, ncol=2)
elevationAtPredSmoothed[is.nan(elevationAtPredSmoothed)] = 0
elevationAtPredSmoothed[is.na(elevationAtPredSmoothed)] <- 0
covariatesAtPredSmoothed = data.frame(elevationAtPredSmoothed = elevationAtPredSmoothed)

# Distance from rivers and lakes

minDistRiverLakeAtPredSmoothed <- raster::extract(distRiverLakeSmoothRaster, predCoordsDegrees, ncol=2)
minDistRiverLakeAtPredSmoothed[is.nan(minDistRiverLakeAtPredSmoothed)] = 0
minDistRiverLakeAtPredSmoothed[is.na(minDistRiverLakeAtPredSmoothed)] <- 0
covariatesAtPredSmoothed$minDistRiverLakeAtPredSmoothed = minDistRiverLakeAtPredSmoothed

# Population

popAtPredSmoothed <- raster::extract(populationSmoothRaster, predCoordsDegrees, ncol=2)
popAtPredSmoothed[is.nan(popAtPredSmoothed)] = 0
popAtPredSmoothed[is.na(popAtPredSmoothed)] <- 0
covariatesAtPredSmoothed$popAtPredSmoothed <- popAtPredSmoothed

# Urbanicity

urbanicityAtPredSmoothed <- raster::extract(urbanizationSmoothRaster, predCoordsDegrees, ncol=2)
urbanicityAtPredSmoothed[is.na(urbanicityAtPredSmoothed)] <- 0
urbanicityAtPredSmoothed[is.nan(urbanicityAtPredSmoothed)] = 0
covariatesAtPredSmoothed$urbanicityAtPredSmoothed <- urbanicityAtPredSmoothed

# Travel time

accessCitiesAtpredSmoothed <- raster::extract(travelTimeSmoothRaster, predCoordsDegrees, ncol=2)
accessCitiesAtpredSmoothed[is.nan(accessCitiesAtpredSmoothed)] = 0
accessCitiesAtpredSmoothed[is.na(accessCitiesAtpredSmoothed)] <- 0
covariatesAtPredSmoothed$accessCitiesAtpredSmoothed = accessCitiesAtpredSmoothed

covariatesAtPredSmoothed = cbind(rep(1, length(popAtPredSmoothed)),
                                 covariatesAtPredSmoothed$minDistRiverLakeAtPredSmoothed,
                                 covariatesAtPredSmoothed$accessCitiesAtpredSmoothed,
                                 covariatesAtPredSmoothed$elevationAtPredSmoothed,
                                 covariatesAtPredSmoothed$popAtPredSmoothed,
                         covariatesAtPredSmoothed$urbanicityAtPredSmoothed)


# 6.) Run TMB----
outputTMB = list()
for(k in 1:nrow(betas)){
  admin1Boundaries = list()
  for (g in 1:length(boundarySc)){
    likelihoodType = list()
    for (i in 1:length(likelihoodSc[1,])){
      spatialRange =list()
      for (j in 1:length(rangeSc)){
        jitteringFactor = list()
        for (h in 1:length(scaleSc)){
          
          dataList = list()
          dataListSmoothed = list()
          u.sim = list()
          flag2 = list()
          
          betaSim = betas[k,]
          
          
          for (l in 1:nSim){
            dataList[[l]] = inputs[[k]][[g]][[i]][[j]][[h]][[l]][["tmbInput"]][["data"]] # data input
            dataListSmoothed[[l]] = inputs[[k]][[g]][[i]][[j]][[h]][[l]][["tmbInputSmoothed"]][["data"]] # data input
            u.sim[[l]] = simulatedData[[k]][[g]][[i]][[j]][[h]][[l]][["u.sim"]][-(1:nLoc)]  # the random field
            flag2[[l]] = likelihoodSc[[1, i]]
          }
          
          # Results = list()
          simulationNumber = list()
          for(l in 1:nSim){
            print("Simulation #:")
            print(l)
            
            
            # fitting with the smoothed covariates (doesn't account for jittering)
            
            dataListSmoothed[[l]][["flagRandomField"]] = 0
            dataListSmoothed[[l]][["flagCovariates"]] = 0

            print("Smoothed")
            Results_Smoothed = try(FitSamplePredict(nLoc =nLoc,
                                              theIntercept = intercept,
                                              dataList = dataListSmoothed[[l]],
                                              parameters = tmb_params,
                                              random = rand_effs,
                                              flag2 = flag2[[l]],
                                              predCoords = predCoords,
                                              mesh.s = mesh.s,
                                              u.sim = u.sim[[l]],
                                              covariatesAtPred = covariatesAtPredSmoothed,
                                              betaSim = betaSim), TRUE)

            # Accounting for jittering or not (on/off => 1/0)
            
            #Unadjusted model (doesn't account for jittering)
            dataList[[l]][["flagRandomField"]] = 0
            dataList[[l]][["flagCovariates"]] = 0
            print("Unadjusted")
            Results_Unadjusted = try(FitSamplePredict(nLoc =nLoc,
                                              theIntercept = intercept,
                                              dataList = dataList[[l]],
                                              parameters = tmb_params,
                                              random = rand_effs,
                                              flag2 = flag2[[l]],
                                              predCoords = predCoords,
                                              mesh.s = mesh.s,
                                              u.sim = u.sim[[l]],
                                              covariatesAtPred = covariatesAtPred,
                                              betaSim = betaSim), TRUE)

            # # # Accounting for jittering in covariates
            dataList[[l]][["flagRandomField"]] = 0
            dataList[[l]][["flagCovariates"]] = 1

            print("C")
            Results_C = try(FitSamplePredict(nLoc =nLoc,
                                                  theIntercept = intercept,
                                                  dataList = dataList[[l]],
                                                  parameters = tmb_params,
                                                  random = rand_effs,
                                                  flag2 = flag2[[l]],
                                                  predCoords = predCoords,
                                                  mesh.s = mesh.s,
                                                  u.sim = u.sim[[l]],
                                                  covariatesAtPred = covariatesAtPred,
                                                  betaSim = betaSim), TRUE)
            #
            # # Accounting for jittering in random field
            dataList[[l]][["flagRandomField"]] = 1
            dataList[[l]][["flagCovariates"]] = 0
            print("R")
            Results_R = try(FitSamplePredict(nLoc =nLoc,
                                                  theIntercept = intercept,
                                                  dataList = dataList[[l]],
                                                  parameters = tmb_params,
                                                  random = rand_effs,
                                                  flag2 = flag2[[l]],
                                                  predCoords = predCoords,
                                                  mesh.s = mesh.s,
                                                  u.sim = u.sim[[l]],
                                                  covariatesAtPred = covariatesAtPred,
                                                  betaSim = betaSim), TRUE)


            # Accounting for jittering in both
            dataList[[l]][["flagRandomField"]] = 1
            dataList[[l]][["flagCovariates"]] = 1
            print("Adjusted")
            Results_Adusted = try(FitSamplePredict(nLoc =nLoc,
                                              theIntercept = intercept,
                                              dataList = dataList[[l]],
                                              parameters = tmb_params,
                                              random = rand_effs,
                                              flag2 = flag2[[l]],
                                              predCoords = predCoords,
                                              mesh.s = mesh.s,
                                              u.sim = u.sim[[l]],
                                              covariatesAtPred = covariatesAtPred,
                                              betaSim = betaSim), TRUE)
            
            Results = list(Results_Unadjusted = Results_Unadjusted,
                           Results_Smoothed = Results_Smoothed,
                           Results_C = Results_C,
                           Results_R = Results_R,
                           Results_Adusted = Results_Adusted
                           )
            
            save(Results, g, i, h, file = "temp.RData")
            
            
            simulationNumber[[paste("Simulation", l)]] = Results
            
          }
          #
          if (h==1){
            jitteringFactor[["jitteringFactor = 1"]] = simulationNumber
          }else {
            jitteringFactor[["jitteringFactor = 4"]] = simulationNumber
          }
          #
          
        }
        #
        if (j==1){
          spatialRange[["range from RealData"]] = jitteringFactor
        }else {
          spatialRange[["range = 340 km"]]  = jitteringFactor
        }
        #
      }
      #
      if (i==1){
        likelihoodType[["binomial likelihood"]] = spatialRange
      }else {
        likelihoodType[["Gaussian likelihood"]] = spatialRange
      }
      #
    }
    #
    if (boundarySc==TRUE){
      admin1Boundaries[["Borders respected"]] = likelihoodType
    }else {
      admin1Boundaries[["Borders not Respected"]] = likelihoodType
    }
    #
  }
  #
  if (k==1){
    outputTMB[["scaledBetasby0.5"]] = admin1Boundaries
  }else if (k==2){
    outputTMB[["notScaled"]]  = admin1Boundaries
  }else{
    outputTMB[["scaledBetasby2"]]  = admin1Boundaries
  }
  #
}        

save(outputTMB, file = "output.RData")  

quit()


