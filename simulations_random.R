# this script is used for "Section 4- Simulation Study"
# of our paper. The script runs the simulation study 
# where the observation locations are randomly generated 
# with respect to the population density for each simulation


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
library(parallel)

# Load functions
source("modSPDEJitter.R")
source("makeIntegrationPoints.R")
source('functions.R')

load("") # load the parameter estimates from your analysis on real data (with
# the adjusted model). Estimated log_tau and log_kappa will be used to 
# calculate rangeSc and sigma.sim

# 
# #set the seeds
set.seed(2345)

# Setup

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
#nSubRPerPoint * nSubAPerPoint)
nSubRPerPoint = 10
nSubAPerPoint = 10

# Jittering Factor
scaleSc = c(1) # the default max. DHS jittering distances can be scaled and tested using this
               # currently it is scaled by 1 (non-scaled)

# #Likelihood :
# #1st row : 0/1 for Gaussian/Binomial
likelihoodSc = as.matrix(rbind(likelihood = c(1), nuggetVar = c(0), p = c(0.5)))
# 
# # Provincial Boundaries :
boundarySc = TRUE


# number of simulations per scenario 
# (each combination of boundarySc, likelihoodSc, scaleSc and rangeSc 
#  corresponds to one scenario)
nSim = 50

## 2.) Geography and demography data----

# Load and prepare the geography
# Read the maps
nigeriaMap_admin0 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                            layer = "gadm40_NGA_0")

nigeriaMap_admin1 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                            layer = "gadm40_NGA_1")

nigeriaMap_admin2 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                      layer = "gadm40_NGA_2")

# # Remove Lake Chad (Water body)
 nigeriaMap_admin2 = nigeriaMap_admin2[-160,]

# read the DHS data
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

# 
myData = subset(myData, age <= 49 & age >=20)
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
# 
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

# Create admin2 point in polygon table (object "check1") to check against new locations
true_latLon = cbind(nigeria.data[,"long"], nigeria.data[,"lat"])
colnames(true_latLon) = c("long", "lat")
true_latLon = SpatialPoints(true_latLon, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"), bbox = NULL)


nigeriaMap_admin2@data[["OBJECTID"]] =1:774 #normally 775, but we removed one of them (160th polygon), which was the lake
check1 <- over(true_latLon, nigeriaMap_admin2, returnList = FALSE)

# Drop rows which doesn't match with admin areas. 
# Jittering algorithm will need them to match.

which(is.na(check1$NAME_2))

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


#Remove the locations that are outside Nigeria
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
# -2.215155, 0.6231852, -0.4352419, -0.02390748, 0.3300220, -1.3566005

betas = rbind(Results_RealData[["Results_CR"]][["SD0"]][["par.random"]][1:6]/2,  #scaled by 0.5
              Results_RealData[["Results_CR"]][["SD0"]][["par.random"]][1:6],   #scaled by 1 (non-scaled)
              Results_RealData[["Results_CR"]][["SD0"]][["par.random"]][1:6]*1.5) #scaled by 1.5


# settings
intercept = logit(0.5)
#

ns = nigeria.data$ns
#
# # Load the data rasters :
load("processedCovariateData.RData")
load("croppedNotScaledUrbanization.RData")
load("smoothedRasters.RData")
load("populationSmoothRaster.RData")
#

# simulate new sets of DHS locations (for each simulation),
# responses and spatial field
#

# read the population raster
r<-'NGA_ppp_v2c_2015.tif' 
r=raster(r)
r = raster::mask(crop(r,extent(nigeriaMap_admin0)),nigeriaMap_admin0, snap = 'out')
# > print(r)
# class      : RasterLayer 
# dimensions : 11547, 14414, 166438458  (nrow, ncol, ncell)
# resolution : 0.0008333, 0.0008333  (x, y)
# extent     : 2.668418, 14.6796, 4.272374, 13.89449  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# source     : NGA_ppp_v2c_2015.tif 
# names      : NGA_ppp_v2c_2015 

idx = 1:ncell(r)
r[is.nan(r)] = 0
r[is.na(r)] <- 0

val = getValues(r)
r_total=sum(val, is.na=TRUE)
probs = val/r_total
cellCenters = xyFromCell(r, idx)

nLoc = length(nigeria.data$ns)

simulatedData = list()
simulatedDataLocs = list()
for(k in 1:nrow(betas)){
  admin1Boundaries = list()
  admin1BoundariesLocs = list()
  for (g in 1:length(boundarySc)){
    likelihoodType = list()
    likelihoodTypeLocs = list()
    for (i in 1:length(likelihoodSc[1,])){
      spatialRange =list()
      spatialRangeLocs =list()
      for (j in 1:1){
        jitteringFactor = list()
        jitteringFactorLocs = list()
        for (h in 1:length(scaleSc)){
          simulationNumber = list()
          simulationNumberLocs = list()
          for(l in 1:nSim){

            # simulate new set of DHS locations for each simulation
            repeat{
              
              nlocsInPixel = rmultinom(n=1, size=nLoc, prob=probs)
              idxNonZeros = which(!nlocsInPixel == 0)
              coordsNonZeros = data.frame(lon = cellCenters[,1][idxNonZeros], lat = cellCenters[,2][idxNonZeros])
              NonZeros = nlocsInPixel[which(!nlocsInPixel == 0)]
              nDHS = sum(NonZeros)
              
              locDegrees = cbind(rep(coordsNonZeros[1,1], NonZeros[1]), rep(coordsNonZeros[1,2], NonZeros[1]))
              for(t in 2:length(NonZeros)){
                locDegrees = rbind(locDegrees, cbind(rep(coordsNonZeros[t,1], NonZeros[t]), rep(coordsNonZeros[t,2], NonZeros[t])))
              }
              
              # check if all sampled locations match with admin2 areas
              sampledLocs=data.frame(xCoor = locDegrees[,1], yCoor = locDegrees[, 2])
              sampledLocs <- SpatialPointsDataFrame(sampledLocs, data.frame(id=1:nLoc)) 
              sampledLocs@proj4string@projargs = nigeriaMap_admin0@proj4string@projargs
              locCheck=over(sampledLocs,nigeriaMap_admin2,returnList = FALSE)
              selectSampLoc = which(!is.na(locCheck[,1]))
              
              locObsKM = convertDegToKM(locDegrees)
              
              locDegrees = data.frame(long = locDegrees[,1], lat = locDegrees[,2])
              
              if(length(selectSampLoc)==nLoc){
                break
              }
              
            }
            
          
            simLoc = rbind(as.matrix(locObsKM),
                           predCoords)
              
            sigma.cluster.sim = sqrt(likelihoodSc[[2, i]])
            range.sim = rangeSc[[j]]
            flag2 = likelihoodSc[[1, i]]
            betaSim = betas[k,]

            sim.data = simulateResponses(nObs = length(nigeria.data[,1]),
                                         locMatrix = simLoc,
                                         ns = ns,
                                         betaSim = betaSim,
                                         space.range = range.sim,
                                         space.sigma = sigma.sim,
                                         gauss.sim = sigma.cluster.sim,
                                         covariateData = processedCovariateData,
                                         locDegrees = locDegrees,
                                         flag2 = flag2)

            simulationNumber[[paste("Simulation", l)]] = sim.data
            simulationNumberLocs[[paste("Simulation", l)]] = locObsKM
          }
          ######
          if (h==1){
            jitteringFactor[["jitteringFactor = 1"]] = simulationNumber
            jitteringFactorLocs[["jitteringFactor = 1"]] = simulationNumberLocs
            
            
          }else {
            jitteringFactor[["jitteringFactor = 4"]] = simulationNumber
            jitteringFactorLocs[["jitteringFactor = 4"]] = simulationNumberLocs
          }
          ######

        }
        ######
        if (j==1){
          spatialRange[["range from RealData"]] = jitteringFactor
          spatialRangeLocs[["range from RealData"]] = jitteringFactorLocs
        }else {
          spatialRange[["range = 340 km"]]  = jitteringFactor
          spatialRangeLocs[["range = 340 km"]] = jitteringFactorLocs
        }
        ######
      }
      ######
      if (i==1){
        likelihoodType[["binomial likelihood"]] = spatialRange
        likelihoodTypeLocs[["binomial likelihood"]] = spatialRangeLocs
      }else {
        likelihoodType[["Gaussian likelihood"]] = spatialRange
        likelihoodTypeLocs[["Gaussian likelihood"]] = spatialRangeLocs
      }
      ######
    }
    ######
    if (boundarySc==TRUE){
      admin1Boundaries[["Borders respected"]] = likelihoodType
      admin1BoundariesLocs[["Borders respected"]] = likelihoodTypeLocs
    }else {
      admin1Boundaries[["Borders not respected"]] = likelihoodType
      admin1BoundariesLocs[["Borders not respected"]] = likelihoodTypeLocs
    }
    ######
  }
  ######
  if (k==1){
    simulatedData[["scaledBetasby0.5"]] = admin1Boundaries
    simulatedDataLocs[["scaledBetasby0.5"]] = admin1BoundariesLocs
  }else if (k==2){
    simulatedData[["notScaled"]]  = admin1Boundaries
    simulatedDataLocs[["notScaled"]] = admin1BoundariesLocs
  }else{
    simulatedData[["scaledBetasby1.5"]]  = admin1Boundaries
    simulatedDataLocs[["scaledBetasby1.5"]] = admin1BoundariesLocs
  }
  ######
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

save.image("image.RData")


# 5.) Prepare the inputs ----
# for model fitting with TMB

# set seeds for main and parallel processes
set.seed(123)
totalIter = length(boundarySc)*length(rangeSc)*length(likelihoodSc[1,])*length(rangeSc)*length(scaleSc)*nSim
# set seeds
allSeeds = sample(1:1000000, totalIter, replace=FALSE)

# set up parallel processes
library(parallel)
cl <- makeCluster(16)
clusterEvalQ(cl, {
  setwd("")
  library(SUMMER)
  library(rgdal)
  library(foreign)
  library(INLA)
  library(TMB)
  library(scoringRules)
  
  # Load functions
  source("modSPDEJitter.R")
  source("makeIntegrationPoints.R")
  source('functions_5covariateSmoothed.R')
  
  load("image.RData")
})
clusterExport(cl, c("allSeeds", "totalIter"))

# Prepare inputs for model fitting with TMB

inputs = list()
for(k in 1:nrow(betas)){
  tempInput1 =list()
  for (g in 1:length(boundarySc)){
    tempInput2 =list()
    for (i in 1:length(likelihoodSc[1,])){
      tempInput3 =list()
      for (j in 1:length(rangeSc)){
        tempInput4 = list()
        for (h in 1:length(scaleSc)){
          tempInput5 = list()

          theseIters = (g-1)*length(likelihoodSc[1,])*length(rangeSc)*length(scaleSc)*nSim + 
            (i-1) * length(rangeSc)*length(scaleSc)*nSim + 
            (j-1) * length(scaleSc) + 
            (h-1) * nSim + 1:nSim
          clusterExport(cl, "theseIters")
          # set seeds
          
          # for(l in 1:nSim){
          innerForFun = function(l) {
            # print(paste0("g: ", g, "/", length(boundarySc), 
            #              ", i: ", i, "/", length(likelihoodSc[1,]), ", ", 
            #              ", j: ", j, "/", length(rangeSc), 
            #              ", h: ", h, "/", length(scaleSc), 
            #              ", l: ", l, "/", nSim))
            
            # set seed
            thisIter = theseIters[l]
            set.seed(allSeeds[thisIter])
          
          
            boundary = boundarySc[[g]]
            jScale = scaleSc[[h]]
            flag2 = likelihoodSc[[1, i]]
            sigma.cluster.sim = sqrt(likelihoodSc[[2, i]])
            range.sim = rangeSc[[j]]


            modelParams = list(intercept = intercept,
                               range.sim = range.sim,  #kilometers
                               sigma.sim = sigma.sim,
                               sigma.cluster.sim = sigma.cluster.sim)

            otherValues = list(USpatial = 1,
                               alphaSpatial = 0.05,
                               flag2 = flag2,
                               jScale = jScale)

            locKM = data.frame(east = simulatedDataLocs[[k]][[g]][[i]][[j]][[h]][[l]][,1],
                               north = simulatedDataLocs[[k]][[g]][[i]][[j]][[h]][[l]][,2])
            
            
            # check what Admin2 areas the simulated locations are in
            origPoint_spatialPointsObject = SpatialPoints(locKM, 
                                                          proj4string=CRS("+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"), 
                                                          bbox = NULL)
            # Transform it also into longitude/latitude format
            origPoint_longLat <- sp::spTransform(origPoint_spatialPointsObject, nigeriaMap_admin2@proj4string)
            #see which admin1 area it landed in:
            thisCheck <- over(origPoint_longLat, nigeriaMap_admin2, returnList = FALSE)
            
            # TMB Input
            locObs = Displace(scale = jScale,
                              locKM = locKM,
                              urbanRural = nigeria.data$urbanRuralDHS,
                              Admin2ShapeFile = nigeriaMap_admin2,
                              check1 = thisCheck,
                              boundary = boundary)


            locObs =cbind(locObs[[1]][["east"]], locObs[[1]][["north"]])

            if(boundarySc) {
              thisMap = nigeriaMap_admin2
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
            
            list(tmbInput = tmbInput, tmbInputSmoothed = tmbInputSmoothed)
            
          }
          
          clusterExport(cl, c("k","g", "i", "j", "h"))
          
          res = parLapply(cl, 1:nSim, innerForFun)
          
          tempInput5[["tmbInput"]] = lapply(res, function(x) {x$tmbInput})
          tempInput5[["tmbInputSmoothed"]] = lapply(res, function(x) {x$tmbInputSmoothed})
          
          
          tempInput4[[h]] = tempInput5
          
        }
        tempInput3[[j]] = tempInput4
      }
      
      tempInput2[[i]] = tempInput3
    }
    tempInput1[[g]] = tempInput2
  }
  inputs[[k]] = tempInput1
}

stopCluster(cl)


# parameters
tmb_params <- list(beta=rep(0, 6),
                   log_tau = log_tau, # log_tau from real data fitting (adjusted model)
                   log_kappa = log_kappa, # log_kappa from real data fitting (adjusted model)
                   Epsilon_s = rep(0, mesh.s[['n']])#,  RE on mesh vertices
                   )
)

# 
# # random effects
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
 
# TRAVEL TIME DATA

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
            dataList[[l]] = inputs[[k]][[g]][[i]][[j]][[h]][["tmbInput"]][[l]][["data"]] # data input
            dataListSmoothed[[l]] = inputs[[k]][[g]][[i]][[j]][[h]][["tmbInputSmoothed"]][[l]][["data"]] # data input
            u.sim[[l]] = simulatedData[[k]][[g]][[i]][[j]][[h]][[l]][["u.sim"]][-(1:nLoc)]
            flag2[[l]] = likelihoodSc[[1, i]]
          }
          
          # Results = list()
          simulationNumber = list()
          for(l in 1:nSim){
            print("Simulation #:")
            print(l)
            
            
            # fitting with smoothed covariates 
            
            dataListSmoothed[[l]][["flagRandomField"]] = 0
            dataListSmoothed[[l]][["flagCovariates"]] = 0

            print("NNsmoothed")
            Results_NNsmoothed = try(FitSamplePredict(nLoc =nLoc,
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

            
           
            # Accounting for jittering in covariates and random field (on/off => 1/0)
            
            # Jittering is not accounted for
            dataList[[l]][["flagRandomField"]] = 0
            dataList[[l]][["flagCovariates"]] = 0
            print("NN")
            Results_NN = try(FitSamplePredict(nLoc =nLoc,
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
            print("CR")
            Results_CR = try(FitSamplePredict(nLoc =nLoc,
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
            
            Results = list(Results_NN = Results_NN,
                           Results_NNsmoothed = Results_NNsmoothed,
                           Results_CR = Results_CR
                           )
            
            save(Results, g, i, h, j, k, l, file = "temp.RData")
            
            
            simulationNumber[[paste("Simulation", l)]] = Results
            
          }
          ######
          if (h==1){
            jitteringFactor[["jitteringFactor = 1"]] = simulationNumber
          }else {
            jitteringFactor[["jitteringFactor = 4"]] = simulationNumber
          }
          ######
          
        }
        ######
        if (j==1){
          spatialRange[["range = 160 km"]] = jitteringFactor
        }else {
          spatialRange[["range = 340 km"]]  = jitteringFactor
        }
        ######
      }
      ######
      if (i==1){
        likelihoodType[["binomial likelihood"]] = spatialRange
      }else {
        likelihoodType[["Gaussian likelihood"]] = spatialRange
      }
      ######
    }
    ######
    if (boundarySc==TRUE){
      admin1Boundaries[["Borders respected"]] = likelihoodType
    }else {
      admin1Boundaries[["Borders not Respected"]] = likelihoodType
    }
    ######
  }
  ######
  if (k==1){
    outputTMB[["SignalLow"]] = admin1Boundaries
  }else if (k==2){
    outputTMB[["SignalMed"]]  = admin1Boundaries
  }else{
    outputTMB[["SignalHigh"]]  = admin1Boundaries
  }
  ######
}        

save(outputTMB, file = "output.RData")  

quit()


