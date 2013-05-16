# fish derivative examples; 
# **jread-usgs 2013-04-07
source('../Source/GlmPhysicalDerivatives.R')
source('../Source/GLMnetCDF.R')
library(ncdf4)

# set folder for output file (will be WBIC name, but different for examples)
# choose year for analysis
lakeFolder  <-  '/Users/jread/Desktop/Science Projects/WiLMA/Results data/'
year  <-  '1991'

#get GLMnc
GlmNc <- nc_open(paste(c(lakeFolder,"output.nc"),collapse=""))

# get water temperature and ice cover ouputs
GLMwtr  <-  getGLMwtr(GlmNc)
GLMice  <-  getGLMice(GlmNc)

# subset datasets to only contain the year of interest
startTime <-  paste(c(year,'01-01'),collapse='-')
stopTime <- paste(c(year,'12-31'),collapse='-')
GLMwtr  <-  subsetTime(GLMwtr,startTime,stopTime)
GLMice  <-  subsetTime(GLMice,startTime,stopTime)

#---days of water column exceeding 29 degrees C; Fang et al 2004
numAbove29  <-  getDaysAboveT(GLMwtr,29,anyDep=FALSE) 
#---mean july  surface temperature  Jones et al 2006
JulStart  <- paste(c(year,'07-01'),collapse='-')
JulEnd  <- paste(c(year,'07-31'),collapse='-')
meanJulSurf <- mean(getSurfaceT(subsetTime(GLMwtr,JulStart,JulEnd)))
#---Maximum observed surface temperature  Shuter et al 2002, Shuter and Ing 1997
peakSumT  <- getTempMax(GLMwtr)
#---days with water between 18.2 & 28.2 degrees C  Fang et al 2004
numBetween18_2_28_2 <-  getDaysBetweenT(GLMwtr,18.2,28.2,anyDep=TRUE)
#---days with water between 22 & 23 degrees C  Fang et al 2004
numBetween22_23 <-  getDaysBetweenT(GLMwtr,22,23,anyDep=TRUE)
#---Duration of stratified period  de Stasio et al 1996
durStrat <- getStratifiedDuration(GLMwtr,GLMice,minStrat=0.5)


