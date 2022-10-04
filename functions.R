#...............................................................................
#        These are the helper functions for our main script       
#    titled "The Impact of Jittering on Raster- and Distance-based 
#               Geostatistical Analyses of DHS Data"
#...............................................................................

# Convert from lon/lat to easting/northing
convertDegToKM = function(loc){   # loc = cbind(longitude, latitude)
  locLatLon = SpatialPoints(loc, 
                            proj4string = CRS("+proj=longlat +datum=WGS84"))
  locKM = spTransform(locLatLon,
                      CRS("+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"))
  return(locKM@coords[,c(1,2)])
}

#Convert from easting/northing to lon/lat 
convertKMToDeg = function(loc) {   # loc = cbind(easting, northing)
  locSP = SpatialPoints(loc, proj4string=CRS("+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"))
  lonLatCoords = spTransform(locSP, CRS("+proj=longlat +datum=WGS84"))
  attr(lonLatCoords, "coords")
}

# Convert degrees (lon/lat) to Mollweide
convertDegToMollweide = function(loc){  # loc = cbind(longitude, latitude)
  locLatLon = SpatialPoints(loc, 
                            proj4string = CRS("+proj=longlat +datum=WGS84"))
  locMollweide = spTransform(locLatLon,
                             CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
  return(locMollweide@coords[,c(1,2)])
}

#...............................................................................

# Matérn covariance function
# dMat = distance matrix
covMatern = function(dMat, range, stdDev){
  Sig = inla.matern.cov(nu = 1,
                        kappa = sqrt(8*1)/range,
                        x = dMat,
                        corr = TRUE)
  Sig = stdDev^2*Sig
}

#...............................................................................

# Simulate from model
# nObs = number of observations 
# loc = simulation locations  
# ns = number of trials at each cluster
# betaSim = vector of the intercept and the coefficients of covariates
# space.range = calculated by the estimated log_kappa on real data with adjusted model
# space.sigma = calculated by the estimated log_kappa and log_tau on real data with adjusted model
# gauss.sim = nugget variance (if you have a nugget in the model)  (passes via likelihoodSc)
# covariateData = design matrix
# mainData = the main data set
# flag2 = flag that indicates binomial or Gaussian likelihood  (passes via likelihoodSc[[1, i]])

simulateResponses = function(nObs, loc, ns,
                             betaSim, space.range, 
                             space.sigma, gauss.sim, covariateData, mainData, flag2){
  
  # Spatial covariance matrix
  covMat = covMatern(dMat = as.matrix(dist(loc)),
                     range = space.range,
                     stdDev = space.sigma)
  
  # Add small noise
  covMat = covMat + space.sigma^2*diag(x = 1e-6^2, nrow = dim(covMat)[1], ncol = dim(covMat)[2])
  
  # Simulate spatial effect
  L = t(chol(covMat))
  u.sim = L%*%rnorm(dim(L)[1])
  
  longLat = cbind(mainData$long, mainData$lat)
  dhsLocsDegree = SpatialPoints(longLat, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"), bbox = NULL)
  
  #Extract covariate values from data layers at dhsLocs
  populationRaster = processedCovariateData[["population"]]
  urbanizationRaster = croppedNotScaledUrbanization
  elevationRaster = processedCovariateData[["elevation"]]
  travelTimeRaster = processedCovariateData[["travelTime"]]
  distRiverLakeRaster = processedCovariateData[["minDistRiverlakes"]]
  
  # minimum distance to rivers & lakes
 
  minDistRiverLake <- raster::extract(distRiverLakeRaster, dhsLocsDegree)
  minDistRiverLake[is.nan(minDistRiverLake)] = 0
  minDistRiverLake[is.na(minDistRiverLake)] <- 0
  
  # population
  
  population <- raster::extract(populationRaster, dhsLocsDegree, ncol=2)
  population[is.nan(population)] = 0
  population[is.na(population)] <- 0
  
  # urbanicity
  
  urbanicity <- raster::extract(urbanizationRaster, dhsLocsDegree, ncol=2)
  urbanicity[is.nan(urbanicity)] = 0
  urbanicity[is.na(urbanicity)] <- 0
   
  # elevation
  
  Elev <- raster::extract(elevationRaster, dhsLocsDegree, ncol=2)
  Elev[is.nan(Elev)] = 0
  Elev[is.na(Elev)] <- 0
   
  # travel time to the cities
  accessCities <- data.frame(raster::extract(travelTimeRaster, dhsLocsDegree, ncol=2)) 
  accessCities = accessCities[,1]
  accessCities[is.nan(accessCities)] = 0
  accessCities[is.na(accessCities)] <- 0
  
  desMat = cbind(rep(1, nObs), minDistRiverLake, accessCities, Elev, population, urbanicity)
  xBeta = desMat%*%betaSim
  
  if (flag2 == 0){
    lin.pred = xBeta + u.sim[1:nObs] + rnorm(nObs, mean = 0, sd = gauss.sim)
    # Simulate Gaussian responses
    ys = rnorm(n = nObs,
               mean = lin.pred,
               sd = gauss.sim)
    
    
  } else {
    # Simulate Binomial responses
    lin.pred = xBeta + u.sim[1:nObs]
    ps = expit(lin.pred)
    ys = rbinom(n = nObs,
                size = ns,
                prob = ps)
    
  }
  return(list(lin.pred = lin.pred,
              ys = ys,
              X = desMat,
              u.sim = u.sim))
  
}

#...............................................................................

# Generate random distances with respect to the selected jittering scale
# type : a vector of location types : U for urban, R for rural
# s = scaling factor (1, 3 ,5, 10)
random.distance<- function(type, s){      
  distance<- rep(0, length(type))
  for (i in 1:length(type)){
    if (type[[i]]=="U"){
      distance[[i]]=runif(1, min = 0, max = 2*s)}
    else {
      if (runif(1) < 0.01){
        distance[[i]]=runif(1, min = 0, max = 10*s)
      } else{
        distance[[i]]=runif(1, min = 0, max = 5*s)
      }
      list(rand.dist=distance)
    }}
  return(distance)
}

#...............................................................................
#Generate random displacement angles (between 0 - 2pi) 

# length: Number of observations.
random.angle<- function(length){
  angle<- (runif(length, min = 0, max = 2*pi))
  return(angle)
}

#...............................................................................
# Displace locations w.r.t distance and angle generated by two functions above
displace <- function(east, north, angle, distance){
  locx=rep(0, length(north))
  locy=rep(0, length(north))	
  for (i in 1:length(north)){
    (locy[i]=north[i]+((distance[i])*sin(angle[i])))&
      (locx[i]=east[i]+((distance[i])*cos(angle[i])))}
  results=data.frame(locx=locx, locy=locy)
  return(results)
}

#...............................................................................

# Bring 3 functions above together. Jitter coordinate sets by respecting/not respecting admin1 areas.
# scale-->jittering scale,  locKM--> true locations as east/north (km)
# urbanRural--> should be a vector of U/R, boundary--> TRUE if we respect admin1 boundaries

Displace = function(scale, locKM, urbanRural, Admin1ShapeFile, check1, boundary){
  # Jitter each true coordinate one by one and then check the administrative areas they are in now
  #If they landed into a different area then their previous one, jitter that location again
  #Stop when it is jittered and stayed in the same area as befor. Continue jittering with the next location.
  eastOriginal = locKM[, "east"]
  northOriginal = locKM[,"north"]
  nLoc = length(eastOriginal)
  jitteredCoords = list()
  for (j in 1:length(scale)){
    newLocationSet=data.frame(east = rep(NA, nLoc), north = rep(NA, nLoc))
    for (i in 1:nLoc){
      repeat{
        #arguments to be used in jittering:
        east = eastOriginal[i]; north = northOriginal[i]; angle = random.angle(1)
        distance = random.distance(type = urbanRural[i], s = scale[j])
        #jitter location i with respect to scale j  (in east/north format)
        newPoint_eastNorth = displace(east = east, north = north, angle = angle, distance = distance)
        # If we respect admin1 boundaries, check the new point against initial point in polygon table (check1)
        if (boundary == "TRUE"){
          #convert jittered location i to a spatialPoints object
          newPoint_spatialPointsObject = SpatialPoints(cbind(newPoint_eastNorth[,1], newPoint_eastNorth[,2]), proj4string = CRS("+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"), bbox = NULL)
          # Transform it also into longitude/latitude format
          newPoint_longLat <- sp::spTransform(newPoint_spatialPointsObject, Admin1ShapeFile@proj4string)
          #see which admin1 area it landed in:
          check2 <- over(newPoint_longLat, Admin1ShapeFile, returnList = FALSE)
          #compare it with its previous admin1 area, keep jittering the same point until two areas match
          if ((is.na(check2[,"NAME_1"][[1]]) == FALSE) & (check2[,"NAME_1"][[1]] == check1[,"NAME_1"][[i]])){
            break
          }else{next}
        }else{break}
      } #fill in the jittered location i
      newLocationSet[[i,1]] = newPoint_eastNorth[[1,1]]
      newLocationSet[[i,2]] = newPoint_eastNorth[[1,2]]
    }
    jitteredCoords[[j]] = newLocationSet
  }
  return(jitteredCoords)
}

#...............................................................................


# prepare TMB input w.r.t Gaussian/Binomial response
# flag2-->0! for Gaussian/Binomial response   
# sim.data--> contains spatial field, simulated Gaussian and Binomial responses

prepare_input = function(sim.data, locObs, modelParams, otherValues, jScale, urban, mesh.s, 
                         adminMap=NULL, nSubAPerPoint=10, nSubRPerPoint=10, testMode=FALSE,
                         covariateData, ns, rangeMaternPri){
  
  # extract arguments
  USpatial = otherValues[["USpatial"]]
  alphaSpatial = otherValues[["alphaSpatial"]]
  flag2 = otherValues[["flag2"]]
  flagRandomField = otherValues[["flagRandomField"]]
  flagCovariates = otherValues[["flagCovariates"]]
  #flag1 = otherValues[["flag1"]]
  
  range.sim = modelParams[["range.sim"]]
  
  # number of observed locations
  nLoc = length(locObs[,1])
  
  #response variable Gaussian/Binomial
  if (flag2 == 0){
    ys = sim.data[["ys"]]
    ns = rep(1, nLoc)
  } else {
    ys = sim.data[["ys"]]
    ns = ns
  }

  # spde components
  #
  #jittering the points a bit just to make a mesh
  spde = getSPDEPrior(mesh.s, U=USpatial, alpha=alphaSpatial)
  A.proj = inla.spde.make.A(mesh = mesh.s, loc = cbind(locObs[,1], locObs[,2]))
  #
  # TMB input for the model that accounts for jittering
  
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
  
  
  if(testMode) {
    return(intPointInfo)
  }
  
  
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
  
  # Covariate values at jittered locations and integration points:
  
  # Convert them into degrees and Mollweide
  coordsUrbanDegree = convertKMToDeg(coordsUrban)
  coordsRuralDegree = convertKMToDeg(coordsRural)
  
  #coordsUrbanMollweide = convertDegToMollweide(coordsUrbanDegree)
  #coordsRuralMollweide = convertDegToMollweide(coordsRuralDegree)
  
  # Convert them into SpatialPoints 
  coordsUrbanDegree = SpatialPoints(cbind(coordsUrbanDegree[,1], coordsUrbanDegree[,2]), proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"), bbox = NULL)
  coordsRuralDegree = SpatialPoints(cbind(coordsRuralDegree[,1], coordsRuralDegree[,2]), proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"), bbox = NULL)
  
  #coordsUrbanMollweide = SpatialPoints(cbind(coordsUrbanMollweide[,1], coordsUrbanMollweide[,2]), proj4string = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"), bbox = NULL)
  #coordsRuralMollweide = SpatialPoints(cbind(coordsRuralMollweide[,1], coordsRuralMollweide[,2]), proj4string = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"), bbox = NULL)
  
  # Extract the corresponding covariate values seperately for urban/rural
  
  populationRaster = processedCovariateData[["population"]]
  urbanizationRaster = croppedNotScaledUrbanization
  elevationRaster = processedCovariateData[["elevation"]]
  travelTimeRaster = processedCovariateData[["travelTime"]]
  distRiverLakeRaster = processedCovariateData[["minDistRiverlakes"]]
  
  
  # Minimum distance to rivers & lakes
  
  minDistRiverLakeUrban <- raster::extract(distRiverLakeRaster, coordsUrbanDegree, ncol=2)
  minDistRiverLakeUrban[is.nan(minDistRiverLakeUrban)] = 0
  minDistRiverLakeUrban[is.na(minDistRiverLakeUrban)] <- 0
  # 
  minDistRiverLakeRural <- raster::extract(distRiverLakeRaster, coordsRuralDegree, ncol=2)
  minDistRiverLakeRural[is.nan(minDistRiverLakeRural)] = 0
  minDistRiverLakeRural[is.na(minDistRiverLakeRural)] <- 0
  
  # Population
  
  populationUrban <- raster::extract(populationRaster, coordsUrbanDegree, ncol=2)
  populationUrban[is.nan(populationUrban)] = 0
  populationUrban[is.na(populationUrban)] <- 0
  #
  populationRural <- raster::extract(populationRaster, coordsRuralDegree, ncol=2)
  populationRural[is.nan(populationRural)] = 0
  populationRural[is.na(populationRural)] <- 0
  
  # Urbanicity
  
  urbanicityRural <- raster::extract(urbanizationRaster, coordsRuralDegree, ncol=2)
  urbanicityRural[is.nan(urbanicityRural)] = 0
  urbanicityRural[is.na(urbanicityRural)] <- 0
  # 
  urbanicityUrban <- raster::extract(urbanizationRaster, coordsUrbanDegree, ncol=2)
  urbanicityUrban[is.nan(urbanicityUrban)] = 0
  urbanicityUrban[is.na(urbanicityUrban)] <- 0
  
  # Elevation
  
  elevationUrban <- raster::extract(elevationRaster, coordsUrbanDegree, ncol=2)
  elevationUrban[is.nan(elevationUrban)] = 0
  elevationUrban[is.na(elevationUrban)] <- 0
  # 
  elevationRural <- raster::extract(elevationRaster, coordsRuralDegree, ncol=2)
  elevationRural[is.nan(elevationRural)] = 0
  elevationRural[is.na(elevationRural)] <- 0
  
  # Access to the cities (predicted travel time in minutes to the nearest city)
  
  accessCitiesUrban <- data.frame(raster::extract(travelTimeRaster, coordsUrbanDegree, ncol=2))
  accessCitiesUrban = accessCitiesUrban[,1]
  accessCitiesUrban[is.nan(accessCitiesUrban)] = 0
  accessCitiesUrban[is.na(accessCitiesUrban)] <- 0
  # 
  accessCitiesRural <- data.frame(raster::extract(travelTimeRaster, coordsRuralDegree, ncol=2)) 
  accessCitiesRural = accessCitiesRural[,1]
  accessCitiesRural[is.nan(accessCitiesRural)] = 0
  accessCitiesRural[is.na(accessCitiesRural)] <- 0
  # 
  #
  nLoc_urban = length(coordsUrbanDegree@coords[,1])
  nLoc_rural = length(coordsRuralDegree@coords[,1])
  #
  desMatrixJittUrban = cbind(rep(1, nLoc_urban), minDistRiverLakeUrban, accessCitiesUrban, elevationUrban, populationUrban, urbanicityUrban)
  desMatrixJittRural = cbind(rep(1, nLoc_rural), minDistRiverLakeRural, accessCitiesRural, elevationRural, populationRural, urbanicityRural)
  #
  n_integrationPointsUrban = ncol(wUrban)
  n_integrationPointsRural = ncol(wRural)
  #
  #
  # # Construct projection matrices, and get other relevant info for TMB
  out = makeJitterDataForTMB(intPointInfo, ys , urbanVals, ns, spdeMesh = mesh.s)
  ysUrban = out$ysUrban
  ysRural = out$ysRural
  nsUrban = out$nsUrban
  nsRural = out$nsRural
  AUrban = out$AUrban
  ARural = out$ARural
  # 
  # Inputs for TMB
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
                             beta_pri = c(0, sqrt(1000)), ## normal
                             matern_pri = c(rangeMaternPri, 0.5, USpatial = USpatial , alphaSpatial = alphaSpatial) 
  )
  
  return(list(data = data, mesh.s = mesh.s))
}

#...............................................................................
# prepare TMB input w.r.t Gaussian/Binomial response
# flag2-->0! for Gaussian/Binomial response   
# sim.data--> contains spatial field, simulated Gaussian and Binomial responses

prepare_inputSmoothed = function(sim.data, locObs, modelParams, otherValues, jScale, urban, mesh.s, 
                         adminMap=NULL, nSubAPerPoint=10, nSubRPerPoint=10, testMode=FALSE,
                         covariateData, ns, rangeMaternPri, populationSmoothRaster){
  
  # extract arguments
  USpatial = otherValues[["USpatial"]]
  alphaSpatial = otherValues[["alphaSpatial"]]
  flag2 = otherValues[["flag2"]]
  flagRandomField = otherValues[["flagRandomField"]]
  flagCovariates = otherValues[["flagCovariates"]]
  #flag1 = otherValues[["flag1"]]
  
  range.sim = modelParams[["range.sim"]]
  
  # number of observed locations
  nLoc = length(locObs[,1])
  
  #response variable Gaussian/Binomial
  if (flag2 == 0){
    ys = sim.data[["ys"]]
    ns = rep(1, nLoc)
  } else {
    ys = sim.data[["ys"]]
    ns = ns
  }
 
  # spde components
  #
  #jittering the points a bit just to make a mesh
  spde = getSPDEPrior(mesh.s, U=USpatial, alpha=alphaSpatial)
  A.proj = inla.spde.make.A(mesh = mesh.s, loc = cbind(locObs[,1], locObs[,2]))
  #
  # TMB input for the model that accounts for jittering
  
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
  
  
  if(testMode) {
    return(intPointInfo)
  }
  
  
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
  
  # Covariate values at jittered locations and integration points:
  
  # Convert them into degrees and Mollweide
  coordsUrbanDegree = convertKMToDeg(coordsUrban)
  coordsRuralDegree = convertKMToDeg(coordsRural)
  
  # Convert them into SpatialPoints 
  coordsUrbanDegree = SpatialPoints(cbind(coordsUrbanDegree[,1], coordsUrbanDegree[,2]), proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"), bbox = NULL)
  coordsRuralDegree = SpatialPoints(cbind(coordsRuralDegree[,1], coordsRuralDegree[,2]), proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"), bbox = NULL)
  
  # Extract the corresponding covariate values seperately for urban/rural
  
  populationSmoothRaster = populationSmoothRaster
  urbanizationSmoothRaster = smoothedRasters[["urbanizationSmoothRaster"]]
  elevationSmoothRaster = smoothedRasters[["elevationSmoothRaster"]]
  travelTimeSmoothRaster = smoothedRasters[["travelTimeSmoothRaster"]]
  distRiverLakeSmoothRaster = smoothedRasters[["distRiverLakeSmoothRaster"]]
  
  # Minimum distance to rivers & lakes
  
  minDistRiverLakeUrban <- raster::extract(distRiverLakeSmoothRaster, coordsUrbanDegree, ncol=2)
  minDistRiverLakeUrban[is.nan(minDistRiverLakeUrban)] = 0
  minDistRiverLakeUrban[is.na(minDistRiverLakeUrban)] <- 0
  # 
  minDistRiverLakeRural <- raster::extract(distRiverLakeSmoothRaster, coordsRuralDegree, ncol=2)
  minDistRiverLakeRural[is.nan(minDistRiverLakeRural)] = 0
  minDistRiverLakeRural[is.na(minDistRiverLakeRural)] <- 0
  
  # Population
  
  populationUrban <- raster::extract(populationSmoothRaster, coordsUrbanDegree, ncol=2)
  populationUrban[is.nan(populationUrban)] = 0
  populationUrban[is.na(populationUrban)] <- 0
  #
  populationRural <- raster::extract(populationSmoothRaster, coordsRuralDegree, ncol=2)
  populationRural[is.nan(populationRural)] = 0
  populationRural[is.na(populationRural)] <- 0
  
  # Urbanicity
  
  urbanicityRural <- raster::extract(urbanizationSmoothRaster, coordsRuralDegree, ncol=2)
  urbanicityRural[is.nan(urbanicityRural)] = 0
  urbanicityRural[is.na(urbanicityRural)] <- 0
  # 
  urbanicityUrban <- raster::extract(urbanizationSmoothRaster, coordsUrbanDegree, ncol=2)
  urbanicityUrban[is.nan(urbanicityUrban)] = 0
  urbanicityUrban[is.na(urbanicityUrban)] <- 0
  
  # Elevation
  
  elevationUrban <- raster::extract(elevationSmoothRaster, coordsUrbanDegree, ncol=2)
  elevationUrban[is.nan(elevationUrban)] = 0
  elevationUrban[is.na(elevationUrban)] <- 0
  # 
  elevationRural <- raster::extract(elevationSmoothRaster, coordsRuralDegree, ncol=2)
  elevationRural[is.nan(elevationRural)] = 0
  elevationRural[is.na(elevationRural)] <- 0
  
  # Access to the cities (predicted travel time in minutes to the nearest city)
  
  accessCitiesUrban <- data.frame(raster::extract(travelTimeSmoothRaster, coordsUrbanDegree, ncol=2))
  accessCitiesUrban = accessCitiesUrban[,1]
  accessCitiesUrban[is.nan(accessCitiesUrban)] = 0
  accessCitiesUrban[is.na(accessCitiesUrban)] <- 0
  # 
  accessCitiesRural <- data.frame(raster::extract(travelTimeSmoothRaster, coordsRuralDegree, ncol=2)) 
  accessCitiesRural = accessCitiesRural[,1]
  accessCitiesRural[is.nan(accessCitiesRural)] = 0
  accessCitiesRural[is.na(accessCitiesRural)] <- 0
  # 
  nLoc_urban = length(coordsUrbanDegree@coords[,1])
  nLoc_rural = length(coordsRuralDegree@coords[,1])
  #
  desMatrixJittUrban = cbind(rep(1, nLoc_urban), minDistRiverLakeUrban, accessCitiesUrban, elevationUrban, populationUrban, urbanicityUrban)
  desMatrixJittRural = cbind(rep(1, nLoc_rural), minDistRiverLakeRural, accessCitiesRural, elevationRural, populationRural, urbanicityRural)
  #
  n_integrationPointsUrban = ncol(wUrban)
  n_integrationPointsRural = ncol(wRural)
  #
  # # Construct projection matrices, and get other relevant info for TMB
  out = makeJitterDataForTMB(intPointInfo, ys , urbanVals, ns, spdeMesh = mesh.s)
  ysUrban = out$ysUrban
  ysRural = out$ysRural
  nsUrban = out$nsUrban
  nsRural = out$nsRural
  AUrban = out$AUrban
  ARural = out$ARural
  # 
  # Compile inputs for TMB
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
               beta_pri = c(0, sqrt(1000)), ## normal
               matern_pri = c(rangeMaternPri, 0.5, USpatial = USpatial , alphaSpatial = alphaSpatial) 
  )
  
  return(list(data = data, mesh.s = mesh.s))
}

#...............................................................................
# Model fitting, posterior sampling and prediction with standard model
FitSamplePredict = function(nLoc, theIntercept, dataList, parameters, random,  flag2, predCoords, mesh.s, u.sim, covariatesAtPred,
                            betaSim){
  
  start_time <- Sys.time()
  
  obj <- MakeADFun(data=dataList,
                   parameters=parameters,
                   random = random,
                   hessian=TRUE,
                   DLL='simulations')
  
  
  obj <- normalize(obj, flag="flag1", value = 0)
  
  opt0 = optim(par=obj$par, fn = obj$fn, gr = obj$gr,
               method = c("BFGS"), hessian = FALSE, control=list(parscale=c(.1, .1)))
  
  
  par <- obj$env$last.par
  Qtest = obj$env$spHess(par, random = TRUE)
  
  end_time <- Sys.time()
  
  runTime = end_time - start_time
  
  #Sampling 
  mu <- c(par[-c(7,8)]) #(7th and 8th are log_tau and log_kappa)
  
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
  t.draws <- rmvnorm_prec(mu = mu , chol_prec = L, n.sims = 10000)
  
  parnames <- c(names(mu))
  
  epsilon_draws  <- t.draws[parnames == 'Epsilon_s',]
  beta_draws<- t.draws[parnames == 'beta',]
  
  eta.samples = covariatesAtPred%*%beta_draws + as.matrix(A.pred%*%epsilon_draws)
  # 
  trueValues = covariatesAtPred%*%betaSim + u.sim
  trueValues = trueValues[,1]
  # 
  nPred = length(eta.samples[,1])
  coverage = 0
  for (m in 1:nPred){
    qnt = quantile(eta.samples[m,], c(0.025, 0.975))
    if ((qnt[[1]]<trueValues[[m]])&(qnt[[2]]>trueValues[[m]])){
      coverage = coverage + 1
    }
  }
  coverage = coverage/nPred
  # 
  Logscores = logs_sample(y = trueValues, dat = eta.samples)
  Logscores = mean(Logscores)
  # 
  CRPS = crps_sample(y = trueValues, dat = eta.samples, method = "edf")
  CRPS = mean(CRPS)
  # 
  BIAS = mean(rowMeans(eta.samples)-trueValues)
  #
  RMSE = sqrt(mean((rowMeans(eta.samples)-trueValues)^2))
  #
  if (flag2 != 0){
    #   # Convert to probability scale
    eta.samples = expit(eta.samples)
  }
  # # Find the median and sd across draws, as well as 95% intervals
  PredictedResponses <- cbind(mean = (apply(eta.samples, 1, mean)),
                              median = (apply(eta.samples, 1, median)),
                              sd     = (apply(eta.samples, 1, sd)),
                              lower = (apply(eta.samples, 1, quantile, .025, na.rm = TRUE)), 
                              upper = (apply(eta.samples, 1, quantile, .975, na.rm = TRUE)))
  intercept = beta_draws[1,]
  beta_distRiversLakes = beta_draws[2,]
  beta_accessCities = beta_draws[3,]
  beta_elevation = beta_draws[4,]
  beta_population = beta_draws[5,]
  beta_urbanicity = beta_draws[6,]
  
  
  interceptBIAS = mean(intercept - betaSim[[1]])
  beta_distRiversLakesBIAS = mean(beta_distRiversLakes - betaSim[[2]])
  beta_accessCitiesBIAS = mean(beta_accessCities - betaSim[[3]])
  beta_elevationBIAS = mean(beta_elevation - betaSim[[4]])
  beta_populationBIAS = mean(beta_population - betaSim[[5]])
  beta_urbanicityBIAS = mean(beta_urbanicity - betaSim[[6]])
  
  
  interceptRMSE = sqrt(mean((intercept - betaSim[[1]])^2))
  beta_distRiversLakesRMSE = sqrt(mean((beta_distRiversLakes - betaSim[[2]])^2))
  beta_accessCitiesRMSE = sqrt(mean((beta_accessCities - betaSim[[3]])^2))
  beta_elevationRMSE = sqrt(mean((beta_elevation - betaSim[[4]])^2))
  beta_populationRMSE = sqrt(mean((beta_population - betaSim[[5]])^2))
  beta_urbanicityRMSE = sqrt(mean((beta_urbanicity - betaSim[[6]])^2))
  
    # 
  SampledParameters = cbind(median = c(intercept = median(intercept), beta_distRiversLakes = median(beta_distRiversLakes), beta_accessCities = median(beta_accessCities),
                                       beta_elevation = median(beta_elevation), beta_population = median(beta_population), beta_urbanicity = median(beta_urbanicity)),
                            lower = c(quantile(intercept, probs = 0.025), quantile(beta_distRiversLakes, probs = 0.025), quantile(beta_accessCities, probs = 0.025),
                                      quantile(beta_elevation, probs = 0.025), quantile(beta_population, probs = 0.025), quantile(beta_urbanicity, probs = 0.025)),
                            upper = c(quantile(intercept, probs = 0.975), quantile(beta_distRiversLakes, probs = 0.975), quantile(beta_accessCities, probs = 0.975),
                                      quantile(beta_elevation, probs = 0.975), quantile(beta_population, probs = 0.975), quantile(beta_urbanicity, probs = 0.975)),
                            length = c((quantile(intercept, probs = 0.975)- c(quantile(intercept, probs = 0.025))), (quantile(beta_distRiversLakes, probs = 0.975)- quantile(beta_distRiversLakes, probs = 0.025)),
                                       (quantile(beta_accessCities, probs = 0.975) - quantile(beta_accessCities, probs = 0.025)), (quantile(beta_elevation, probs = 0.975) - quantile(beta_elevation, probs = 0.025)),
                                       (quantile(beta_population, probs = 0.975) - quantile(beta_population, probs = 0.025)),(quantile(beta_urbanicity, probs = 0.975) - quantile(beta_urbanicity, probs = 0.025))))
  
  # 
  return(list(runTime = runTime,
              interceptBIAS = interceptBIAS,
              beta_distRiversLakesBIAS = beta_distRiversLakesBIAS,
              beta_accessCitiesBIAS = beta_accessCitiesBIAS,
              beta_elevationBIAS = beta_elevationBIAS,
              beta_populationBIAS = beta_populationBIAS,
              beta_urbanicityBIAS = beta_urbanicityBIAS,
              interceptRMSE = interceptRMSE,
              beta_distRiversLakesRMSE = beta_distRiversLakesRMSE,
              beta_accessCitiesRMSE = beta_accessCitiesRMSE,
              beta_elevationRMSE = beta_elevationRMSE,
              beta_populationRMSE = beta_populationRMSE,
              beta_urbanicityRMSE = beta_urbanicityRMSE,
              coverage = coverage,
              Logscores = Logscores,
              CRPS = CRPS,
              SampledParameters = SampledParameters,
              PredictedResponses = PredictedResponses,
              prec = prec,
              mu = mu,
              BIAS = BIAS,
              RMSE = RMSE,
              fixed.par = par[c(7,8)]))   # c(7,8) are log_tau and log_kappa
}































