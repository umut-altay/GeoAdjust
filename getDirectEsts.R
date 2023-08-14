# Preparing data
library(rgdal)
library(haven)
library(SUMMER)
library(rgeos)

# read DHS survey
educationData = read_dta("NGIR7BDT/NGIR7BFL.DTA")

# read cluster coordinates 

corList = readOGR(dsn = "dataFiles/DHS/NG_2018_DHS_02242022_98_147470/NGGE7BFL",
                  layer = "NGGE7BFL")

# admin 1 level borders

NGA_1 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                layer = "gadm40_NGA_1")

NGA_1_trnsfrmd = spTransform(NGA_1,proj)


# weights
populationRaster <- raster::raster(x = "NGA_ppp_v2c_2015.tif") 
pop <- raster::extract(populationRaster, clustCoord, ncol=2)

# Create weight variable
wt <- educationData$v005/1000000


smallGeo = data.frame(clusterIdx = corList$DHSCLUST, urban = corList$URBAN_RURA,
                      long = corList$LONGNUM, lat = corList$LATNUM,
                      admin1 = corList$ADM1NAME)

myData = data.frame(clusterIdx = educationData$v001, householdIdx = educationData$v002,
                    wt = educationData$v005/1000000,
                    stratum = educationData$v023, 
                    age = educationData$v012,
                    secondaryEducation = educationData$v106)


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

weigths = aggregate(myData$wt,
                    by = list(clusterID = myData[, 1]),
                    FUN = sum)

answers_joint = merge(answers_x, answers_n, by="clusterID")

answersWithWeights = merge(answers_joint, weigths, by="clusterID")

colnames(answersWithWeights) = c("clusterID", "y", "n", "weight")

nigeria.data = data.frame(clusterID = corList@data[["DHSCLUST"]], long = corList@coords[,1], lat = corList@coords[,2])
nigeria.data = merge(nigeria.data, answersWithWeights, by="clusterID", all=T) 

colnames(nigeria.data)[1] = "clustID"
colnames(nigeria.data)[6] = "samplingWeight"


clustCoord = cbind(nigeria.data$long, nigeria.data$lat)
clustCoord = SpatialPoints(clustCoord, proj4string=CRS("+proj=longlat +datum=WGS84"))

areas = over(clustCoord, NGA_1, returnList = FALSE)

nigeria.data$area = areas[,"NAME_1"]
  
#urbanRural from DHS data set
nigeria.data$urbanRuralDHS = corList@data[["URBAN_RURA"]]
nigeria.data$urban = rep(0, length(nigeria.data$urbanRuralDHS))

# convert urban U/R into TRUE/FALSE
for (i in 1:length(nigeria.data$urbanRuralDHS)){
  if (nigeria.data$urbanRuralDHS[i]=='U'){
    nigeria.data$urban[i]='TRUE'
  }else{
    nigeria.data$urban[i]='FALSE'
  }
}


# Remove NA rows
nigeria.data = nigeria.data[complete.cases(nigeria.data$y), ]
nigeria.data = nigeria.data[complete.cases(nigeria.data$long), ]
nigeria.data = nigeria.data[complete.cases(nigeria.data$lat), ]
nigeria.data = nigeria.data[complete.cases(nigeria.data$samplingWeight), ]


# admin 2 level borders
nigeriaMap_admin2 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                            layer = "gadm40_NGA_2")

# Remove Lake Chad (Water body)
nigeriaMap_admin2 = nigeriaMap_admin2[-160,]

true_latLon = cbind(nigeria.data[,"long"], nigeria.data[,"lat"])
colnames(true_latLon) = c("long", "lat")
true_latLon = SpatialPoints(true_latLon, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"), bbox = NULL)

nigeriaMap_admin2@data[["OBJECTID"]] =1:774 #(originally 775, but we removed the 160th polygon, which was a lake)
check1 <- over(true_latLon, nigeriaMap_admin2, returnList = FALSE)

# > which(is.na(check1$NAME_2))
# [1]   48  122  205  848  857 1116 1122 1287 1328

nigeria.data = nigeria.data[-c(48, 122,  205,  848,  857, 1116, 1122, 1287, 1328),]

nigeria.data$east = rep(NA, length(nigeria.data$long))
nigeria.data$north = rep(NA, length(nigeria.data$long)) 

# Assign UTM37 coordinates to clusters
nigeria.data[,c("east", "north")] = convertDegToKM(nigeria.data[,c("long", "lat")])

directEst = getDirectEsts(clustDat = nigeria.data, divideWeight=TRUE, signifs=c(.95), 
                          stratByUrbanRural=TRUE, customStratVarName=NULL)

save(directEst, file = "")
#################################################################################
# library(haven)
library(survey)

# Gets direct estimates for all areas in the input cluster level dataset.
# clustDat: a cluster level data.frame with elements (in any order):
#    clustID
#    y
#    n
#    urban
#    samplingWeight
#    area
#    other optional elements that shouldn't be divided by n at individual level
# divideWeight: if TRUE, divides clustDat$samplingWeight by n for each individual in the cluster, 
#               if FALSE, gives individuals in the cluster the same sampling weight as the cluster
# signifs: significance levels of the CIs to generate
# stratByUrbanRural: if customStratVar isn't input, whether to stratify by 
#                    urban/rural x area or just area
# customStratVarName: name of a variable in the cluster level dataset to use for 
#                     stratification if stratification by area x urban/rural 
#                     isn't desired
# References:
# https://www.jstor.org/stable/pdf/26408229.pdf (method for svyglm)
# https://www.stat.umn.edu/geyer/5601/notes/sand.pdf (notes on sandwich estimation)
getDirectEsts = function(clustDat, divideWeight=TRUE, signifs=c(.5, .8, .9, .95), 
                         stratByUrbanRural=TRUE, customStratVarName=NULL) {
  
  clustIDs = clustDat$clustID
  indivDat = extendDataset(clustDat, clustIDs=clustIDs, divideWeight=divideWeight)
  
  # pick stratification variable. Either custom, area, or area x urban/rural
  if(!is.null(customStratVarName)) {
    # use a custom stratVar if user requests
    stratVar = indivDat[[customStratVarName]]
  } else {
    # add urban/rural stratification if necessary
    if(stratByUrbanRural) {
      indivDat$regionRural <- with(indivDat, interaction(area, urbanRural), drop=TRUE)
      stratVar = indivDat$regionRural
    } else {
      stratVar = indivDat$area
    }
  }
  
  res = defineSurvey(indivDat, 
                     stratVar=stratVar,
                     useSamplingWeights = TRUE, 
                     signifs=signifs)
  
  res
}

# helper functions -----
# function to extend full DHS dataset to binary form (i.e. from cluster level to individual level)
# clustDat: a dataset of cluster level data.frame with elements:
#    y
#    n
#    urban
#    samplingWeight
#    other optional elements that shouldn't be divided by n at individual level
# clustIDs: cluster IDs. v001 in DHS data
# divideWeight: if TRUE, divides clustDatRow$samplingWeight by n for each individual in the cluster, 
#               if FALSE, gives individuals in the cluster the same sampling weight as the cluster
extendDataset <- function(clustDat, clustIDs, divideWeight=TRUE){
  extendRow = function(r) {
    extendDataRow(data.frame(clustDat[r,]), clustID=clustIDs[r], divideWeight=divideWeight)
  }
  
  do.call("rbind", lapply(1:nrow(clustDat), extendRow))
}

# function to help extend DHS dataset to binary form (i.e. from cluster level to individual level)
# clustDatRow: a row of cluster level data.frame with elements:
#    y
#    n
#    urban
#    samplingWeight
#    other optional elements that shouldn't be divided by n at individual level
# clustID: cluster ID. v001 in DHS data
# divideWeight: if TRUE, divides clustDatRow$samplingWeight by n for each individual in the cluster, 
#               if FALSE, gives individuals in the cluster the same sampling weight as the cluster
extendDataRow <- function(clustDatRow, clustID, divideWeight=TRUE){
  
  # add extra columns for ageMonth, ageGrpD, v001, v002
  n = clustDatRow$n
  # tmp = data.frame(clustDatRow[c(1, 6:16)])
  tmp = clustDatRow
  tmp$clustID = clustID
  
  clustID = rep(clustID, n)
  
  # All 25 households are sampled
  hhID = 1:n
  
  y = c(rep(0,n-clustDatRow$y), rep(1, clustDatRow$y))
  if(clustDatRow["urban"][1,1]){
    urbanRural = rep("urban", n)
  } else {
    urbanRural = rep("rural", n)
  }
  # area = rep(clustDatRow$area, n)
  tmp$y = NULL
  tmp$n = NULL
  res = merge(data.frame(y, clustID, hhID, urbanRural), tmp, by="clustID")
  
  if(divideWeight)
    res$samplingWeight = res$samplingWeight / n
  
  return(res)
}

# - a function that reads in a glm or svyglm - #
# - object and returns the estimate and SE - #
# - specifics in the supplementary materials - #
## This function returns summary statistics about the estimate
get.est<-function(glm.ob, signifs=c(.95)) {
  
  beta<-summary(glm.ob)$coef[,1]
  
  est <-expit(beta)
  logit.var <- vcov(glm.ob)[1,1]
  
  lowerSignifs = (1-signifs)/2
  upperSignifs = 1-(1-signifs)/2
  
  # compute CI intervals
  lower <- logit(est)+qnorm(c(lowerSignifs))*sqrt(logit.var)
  upper <- logit(est)+qnorm(c(upperSignifs))*sqrt(logit.var)
  
  # calculate normal approximate on probability scale
  probEst = logitNormMeanSimple(cbind(logit(est), sqrt(logit.var)))
  # probVar = logitNormSqMeanSimple(cbind(logit(est), sqrt(logit.var))) - probEst^2
  probVar = logitNormVarSimple(cbind(logit(est), sqrt(logit.var)))
  
  out = c(probEst,probVar, logit(est),logit.var,lower, upper)
  names(out) = c("est", "var", "logit.est", "logit.var", 
                 paste("logit.lower", 100*signifs, sep=""), 
                 paste("logit.upper", 100*signifs, sep=""))
  return(out)
}

# -- a function to subset the design based on a region and time period -- #
# -- and then run the svyglm function and return the get.est() results -- #

## First line in function allows you to subset your data and ALSO the specified
## svydesign object into area (usually v024 variable in DHS) 
## and time (per5 is a variable we construct for the 5-year periods in the Stata step)
## Second line fits the survey-weighted glm

region.time.HTDat<-function(dataobj, svydesign, area, nationalEstimate, signifs=.95) {
  
  if(!nationalEstimate) {
    thisArea=area
    tmp<-subset(svydesign, (area==thisArea))
    
    tt2 <- tryCatch(glmob<-svyglm(y~1,
                                  design=tmp,family=quasibinomial, maxit=50), 
                    error=function(e) e, warning=function(w) w)
  } else {
    thisUrban = area == 1
    tmp<-subset(svydesign, (urban==thisUrban))
    tt2 <- tryCatch(glmob<-svyglm(y~1,
                                  design=tmp,family=quasibinomial, maxit=50), 
                    error=function(e) e, warning=function(w) w)
  }
  
  if(is(tt2, "warning")){
    if(grepl("agegroups", tt2)){
      res <- get.est(glmob, signifs=signifs)
      res = c(res, 2)
    } else {
      res = c(rep(NA, 5), 3)
    }
    return(res)
  }
  if(is(tt2,"error")){
    res = c(rep(NA, 5), 1)
    return(res)
  } else {
    res <- get.est(glmob, signifs=signifs)
    res = c(res, 0)
    return(res)
  }
}

defineSurvey <- function(dat_obj, stratVar, useSamplingWeights=TRUE, nationalEstimate=FALSE, 
                            getContrast=nationalEstimate, signifs=.95){
  
  options(survey.lonely.psu="adjust")
  
  # --- setting up a place to store results --- #
  regions <- sort(unique(dat_obj$area))
  regions_num  <- 1:length(regions)
  
  if(!nationalEstimate) {
    results = matrix(nrow=length(regions), ncol=6 + length(signifs)*2)
    colnames(results) = c("area", "est", "var", "logit.est", "logit.var", 
                          paste("logit.lower", 100*signifs, sep=""), 
                          paste("logit.upper", 100*signifs, sep=""), 
                          "converge")
    results = data.frame(results)
    results$area=regions
  }
  else {
    results = matrix(nrow=2, ncol=6 + length(signifs)*2)
    colnames(results) = c("urban", "est", "var", "logit.est", "logit.var", 
                          paste("lower", 100*signifs, sep=""), 
                          paste("upper", 100*signifs, sep=""), 
                          "converge")
    results = data.frame(results)
    results$urban=c(TRUE, FALSE)
  }
  
  if(useSamplingWeights){
    dat_obj$wt <- dat_obj$samplingWeight
  } else {
    dat_obj$wt <- NULL
  }
  
  if(is.null(stratVar)){
    # --- setting up the design object --- #
    ## NOTE: -the clustID denote
    ##        one stage cluster design (clustID is cluster)
    ##       -This call below specifies our survey design
    
    my.svydesign <- svydesign(id= ~clustID,
                              strata =NULL,
                              weights=NULL, data=dat_obj)
  } else {
    ## not in all surveys does v022 contain the correct sampling strata
    ## Thus, the correct vector has to be provided externally
    dat_obj$strat <- stratVar
    
    # --- setting up the design object --- #
    ## NOTE: -the clustID denote
    ##        one stage cluster design (clustID is cluster)
    ##       -This call below specifies our survey design
    ##        nest = T argument nests clusters within strata
    my.svydesign <- svydesign(id= ~clustID,
                              strata=~strat, nest=T, 
                              weights=~wt, data=dat_obj)
  }
  
  for(i in 1:nrow(results)){
    if(!nationalEstimate) {
      results[i, -1] <- region.time.HTDat(dataobj=dat_obj, svydesign=my.svydesign, 
                                           area=results$area[i], nationalEstimate=nationalEstimate, 
                                           signifs=signifs)
    }
    else {
      results[i, -1] <- region.time.HTDat(dataobj=dat_obj, svydesign=my.svydesign, 
                                           area=i, nationalEstimate=nationalEstimate, 
                                           signifs=signifs)
    }
  }
  
  if(getContrast) {
    # out = svyby(~y, by = ~urban, design = svydesign, svymean)
    glmob<-svyglm(y~urban,
                  design=my.svydesign,family=quasibinomial, maxit=50)
    
    # get contrast mean and variance
    est = glmob$coefficients[2]
    urbanVar = vcov(glmob)[2,2]
    
    # get confidence interval
    lowerSignifs = (1 - signifs)/2
    upperSignifs = 1 - (1 - signifs)/2
    lower = est + qnorm(lowerSignifs, sd=sqrt(urbanVar))
    upper = est + qnorm(upperSignifs, sd=sqrt(urbanVar))
    contrastStats = list(est=est, sd=sqrt(urbanVar), lower=lower, upper=upper, signifs=signifs)
    return(list(results=results, contrastStats=contrastStats))
  } else {
    return(results)
  }
  
}


# adapted from logitnorm package.  Calculates the mean of a distribution whose 
# logit is Gaussian. Each row of muSigmaMat is a mean and standard deviation 
# on the logit scale
logitNormMeanSimple = function(muSigmaMat, logisticApproximation=FALSE, ...) {
  if(length(muSigmaMat) > 2) {
    apply(muSigmaMat, 1, logitNormMeanSimple, logisticApproximation=logisticApproximation, ...)
  }
  else {
    mu = muSigmaMat[1]
    sigma = muSigmaMat[2]
    if(sigma == 0)
      expit(mu)
    else {
      if(any(is.na(c(mu, sigma))))
        NA
      else if(!logisticApproximation) {
        # numerically calculate the mean
        fExp <- function(x) exp(plogis(x, log.p=TRUE) + dnorm(x, mean = mu, sd = sigma, log=TRUE))
        integrate(fExp, mu-10*sigma, mu+10*sigma, abs.tol = 0, ...)$value
      } else {
        # use logistic approximation
        warning("logistic approximation is not always very accurate...")
        k = 16 * sqrt(3) / (15 * pi)
        expit(mu / sqrt(1 + k^2 * sigma^2))
      }
    }
  }
}

# Calculates the second moment of a distribution whose 
# logit is Gaussian. Each row of muSigmaMat is a mean and standard deviation 
# on the logit scale of the Gaussian that is squared.
logitNormSqMeanSimple = function(muSigmaMat, ...) 
{
  if (length(muSigmaMat) > 2) {
    apply(muSigmaMat, 1, logitNormSqMeanSimple, ...)
  }
  else {
    mu = muSigmaMat[1]
    sigma = muSigmaMat[2]
    if (sigma == 0) 
      SUMMER::expit(mu)^2
    else {
      if (any(is.na(c(mu, sigma)))) 
        NA
      else {
        # plogis(x) = 1/(1 + e^-x)
        # fExp(x) = exp{2 * log(1/(1 + e^-x)) + log(phi(x; mu, sigma))}
        #         = (1/(1 + e^-x))^2 * phi(x; mu, sigma)
        #         = expit(x)^2 * phi(x; mu, sigma)
        fExp <- function(x) exp(stats::plogis(x, log.p = TRUE) * 2 + 
                                  stats::dnorm(x, mean = mu, sd = sigma, log = TRUE))
        stats::integrate(fExp, mu - 10 * sigma, mu + 
                           10 * sigma, abs.tol = 0, ...)$value
      }
    }
  }
}

# Calculates the second moment of a distribution whose 
# logit is Gaussian. Each row of muSigmaMat is a mean and standard deviation 
# on the logit scale of the Gaussian that is squared.
# NOTE: could use logitNormSqMeanSimple - logitNormMeanSimple^2, but that can be 
#       negative, whereas this function cannot.
logitNormVarSimple = function(muSigmaMat, ...) 
{
  if (length(muSigmaMat) > 2) {
    apply(muSigmaMat, 1, logitNormVarSimple, ...)
  }
  else {
    mu = muSigmaMat[1]
    sigma = muSigmaMat[2]
    if (sigma == 0) 
      SUMMER::expit(mu)^2
    else {
      if (any(is.na(c(mu, sigma)))) 
        NA
      else {
        # plogis(x) = 1/(1 + e^-x) = expit(x)
        # fExp(x) = (expit(x) - probMean)^2 * phi(x; mu, sigma)
        probMean = logitNormMeanSimple(muSigmaMat)
        fExp <- function(x) exp(log((plogis(x) - probMean)^2) + stats::dnorm(x, mean = mu, sd = sigma, log = TRUE))
        stats::integrate(fExp, mu - 10 * sigma, mu + 
                           10 * sigma, abs.tol = 0, ...)$value
      }
    }
  }
}

############## PLOTTING #####################
library(ggplot2)
library(plyr)

NGA_1 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                layer = "gadm40_NGA_1")

NGA_1@data[["OBJECTID"]] =1:37

proj = "+units=km +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +no_defs"
NGA_1_trnsfrmd = spTransform(NGA_1,proj)
NAME_1 = NGA_1_trnsfrmd@data[["NAME_1"]]
NGA_1_trnsfrmd@data$id <- rownames(NGA_1_trnsfrmd@data)

pred = directEst$est
pred_df<-data.frame(NAME_1, pred)
cv = (sqrt(directEst$var)/directEst$est)*100
cv_df<-data.frame(NAME_1, cv)


NGA_1_trnsfrmd@data <- join(NGA_1_trnsfrmd@data, pred_df, by="NAME_1")
NGA_1_trnsfrmd@data <- join(NGA_1_trnsfrmd@data, cv_df, by="NAME_1")

dfNigeria <- fortify(NGA_1_trnsfrmd)
dfNigeria <- join(dfNigeria,NGA_1_trnsfrmd@data, by="id")


NGA_2 = readOGR(dsn = "dataFiles/gadm40_NGA_shp",
                            layer = "gadm40_NGA_2")

NGA_2_trnsfrmd = spTransform(NGA_2,proj)

polyg = NGA_2_trnsfrmd[160,] # the lake

dfpolyg <- fortify(polyg)


# direct estimates
> summary(pred)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.05359 0.26507 0.53085 0.50357 0.73028 0.89386


g1 = ggplot() + 
  geom_polygon(data = dfNigeria, aes(x = long, y = lat, group = group, fill =
                                       pred), color = "black", size = 0.25) + 
  geom_polygon(data = dfpolyg, aes(x = long, y = lat), colour="black",  linewidth = 0.25, fill="white") + theme_bw() + # fill the lake with white color
  coord_fixed() +
  scale_fill_viridis_c(limits = c(0.05, 0.90), option = "viridis") +  #limit from 0 to 0.99 is the limit of unaggregated ones. It is used here as well, to make the color scale the same.
  theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3)))+
  xlab("Easting (km)") + 
  ylab("Northing (km)")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(legend.text=element_text(size=35), legend.title=element_text(size=25)) +
  guides(fill = guide_colourbar(barwidth = 2.5, barheight = 25, title = labs("direct est."), title.vjust=1) ) + 
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))  

ggsave("directEstimates.pdf", path = "", width = 30.24 , height = 22.34, units = "cm") 


# > summary(cv)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.815   4.411   7.717  10.377  14.489  29.669 

g2 = ggplot() + 
  geom_polygon(data = dfNigeria, aes(x = long, y = lat, group = group, fill =
                                       cv), color = "black", size = 0.25) + 
  geom_polygon(data = dfpolyg, aes(x = long, y = lat), colour="black",  linewidth = 0.25, fill="white") + theme_bw() + # fill the lake with white color
  coord_fixed() +
  scale_fill_viridis_c(limits = c(1.8, 30), option = "viridis") +  #limit from 0 to 0.99 is the limit of unaggregated ones. It is used here as well, to make the color scale the same.
  theme(axis.text.y = element_text(size = 35), axis.text.x = element_text(size = 35)) +
  theme(axis.title.x=element_text(size = rel(3))) + theme(axis.title.y=element_text(size = rel(3)))+
  xlab("Easting (km)") + 
  ylab("")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(legend.text=element_text(size=35), legend.title=element_text(size=25)) +
  guides(fill = guide_colourbar(barwidth = 2.5, barheight = 25, title = labs("cv (%)"), title.vjust=1) ) + 
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))  

ggsave("cvDirectEst.pdf", path = "", width = 30.24 , height = 22.34, units = "cm") 

# grid plot
library(gridExtra)
library(grid)
pdf("DirectEstimatesTwoPlots.pdf", width = 61 , height = 22.34, units = "cm")
grid.arrange(g1, g2, nrow = 1, ncol = 2)
dev.off()












