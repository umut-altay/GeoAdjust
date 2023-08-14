
## Initialization
# Load libraries

library(raster)
library(terra)

# Load the data rasters :
load("processedCovariateData.RData")
load("croppedNotScaledUrbanization.RData")


populationRaster = processedCovariateData[["population"]]
urbanizationRaster = croppedNotScaledUrbanization
elevationRaster = processedCovariateData[["elevation"]]
travelTimeRaster = processedCovariateData[["travelTime"]]
distRiverLakeRaster = processedCovariateData[["minDistRiverlakes"]]


#smoothing the rasters
hmm = focalWeight(elevationRaster,d = 5/111, type = "circle")
hmm = hmm/max(hmm)
elevationSmoothRaster = focal(elevationRaster, hmm, fun = mean, na.rm = TRUE)

urbanLongLatRaster = projectRaster(urbanizationRaster, crs = crs(elevationRaster), method = "ngb")
hmm = focalWeight(urbanLongLatRaster, d = 5/111, type = "circle")
hmm = hmm/max(hmm)
urbanizationSmoothRaster = focal(urbanLongLatRaster, hmm, fun = mean, na.rm = TRUE)

hmm = focalWeight(travelTimeRaster,d = 5/111, type = "circle")
hmm = hmm/max(hmm)
travelTimeSmoothRaster = focal(travelTimeRaster, hmm, fun = mean, na.rm = TRUE)

hmm = focalWeight(distRiverLakeRaster,d = 5/111, type = "circle")
hmm = hmm/max(hmm)
distRiverLakeSmoothRaster = focal(distRiverLakeRaster, hmm, fun = mean, na.rm = TRUE)

smoothedRasters = list(elevationSmoothRaster = elevationSmoothRaster,
                       urbanizationSmoothRaster = urbanizationSmoothRaster,
                       travelTimeSmoothRaster = travelTimeSmoothRaster,
                       distRiverLakeSmoothRaster = distRiverLakeSmoothRaster)

save(smoothedRasters, file = "smoothedRasters.RData")

# "terra" package version of same operation (population raster is big, 
# raster package was writing it on disk instead of writing to memory,
# that didn't allow R to properly read it)
library(terra)
hmm = focalMat(populationRaster, 5/111, type = "circle", fillNA=FALSE)
hmm = hmm/max(hmm)
populationSmoothRaster = focal(populationRaster, hmm, fun = mean, na.rm = TRUE)
populationSmoothRaster <- readAll(populationSmoothRaster)
save(populationSmoothRaster, file = "populationSmoothRaster.RData")



# cross-check the results from raster and terra for population raster:

# #raster package
# > populationSmoothRaster
# class      : RasterLayer 
# dimensions : 11547, 14414, 166438458  (nrow, ncol, ncell)
# resolution : 0.0008333, 0.0008333  (x, y)
# extent     : 2.668418, 14.6796, 4.272374, 13.89449  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# source     : memory
# names      : layer 
# values     : -0.997586, 7.321401  (min, max)
# 
# 
# #terra package:
# > View(smoothedRasters)
# > smoothedRasters[["populationSmoothRaster"]]
# class      : RasterLayer 
# dimensions : 11547, 14414, 166438458  (nrow, ncol, ncell)
# resolution : 0.0008333, 0.0008333  (x, y)
# extent     : 2.668418, 14.6796, 4.272374, 13.89449  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# source     : r_tmp_2022-07-01_105052_651_53046.grd 
# names      : layer 
# values     : -0.997586, 7.321401  (min, max)
# 







