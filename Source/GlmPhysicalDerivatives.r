# fish code for WiLMA 
# **jread-usgs 2013-04-07

#source("GLMnetCDF.R")

# -- shared variables --
timeID  <-  "DateTime"
iceID <-  "Ice"
folder  <-  "../Data/"

getGLMnc  <-  function(folder=folder){
  
  GLMnc <- getGLMnc(fileName='output.nc',folder=folder)
  return(GLMnc)
}


getGLMwtr  <-  function(GLMnc){
  GLMwtr <-	resampleGLM(GLMnc)
  return(GLMwtr)
}

getGLMice  <-  function(GLMnc){
  
  GLMice <-  data.frame("DateTime"=getTimeGLMnc(GLMnc),"Ice"=getIceGLMnc(GLMnc))
  return(GLMice)
}

subsetTime <- function(GLM,startDate,stopDate){
  # gets rid of GLM simulation results that are < startDate and > stopDate
  # startDate and stopDate must be of type "character"
  dates <-  GLM$DateTime
  useI  <-  (dates >= as.POSIXct(startDate)) & (dates <= as.POSIXct(stopDate))
  GLM <-  GLM[useI,]
  return(GLM)
}

getTemp   <- function(GLMwtr){
  drops <- c(timeID)
  temp <- GLMwtr[,!(names(GLMwtr) %in% drops)]
  return(temp)
}

getDailyTempMax <- function(GLMwtr){
  temp <- getTemp(GLMwtr)
  dailyTempMax <-  apply(temp,1,function(x) max(x,na.rm=TRUE))
  return(dailyTempMax)
}

getDailyTempMin <- function(GLMwtr){
  temp <- getTemp(GLMwtr)
  dailyTempMin <-  apply(temp,1,function(x) min(x,na.rm=TRUE))
  return(dailyTempMin)
}

getTempMax <- function(GLMwtr){
  dailyTempMax <-  getDailyTempMax(GLMwtr)
  tempMax <-  max(dailyTempMax)
  return(tempMax)
}

getTempMin <- function(GLMwtr){
  dailyTempMin <-  getDailyTempMin(GLMwtr)
  tempMin <-  min(dailyTempMin)
  return(tempMin)
}

getDaysAboveT <- function(GLMwtr,temperature,anyDep=TRUE){
  # ANY or ALL depths, default is ANY
  if (anyDep==TRUE){
    refTemp  <-  getDailyTempMax(GLMwtr)
  }else {
    refTemp <-  getDailyTempMin(GLMwtr)
  }

  tempAboveCount  <-  sum(refTemp > temperature,na.rm=TRUE)
  return(tempAboveCount)
}

getDaysBelowT <- function(GLMwtr,temperature,anyDep=TRUE){
  # ANY or ALL depths, default is ANY
  temp <- getTemp(GLMwtr)
  if (anyDep==TRUE){
    refTemp  <-  getDailyTempMin(GLMwtr)
  }else {
    refTemp <-  getDailyTempMax(GLM)
  }
  
  tempBelowCount  <-  sum(refTemp < temperature,na.rm=TRUE)
  return(tempBelowCount)
}

getDaysBetweenT <-  function(GLMwtr,temperatureLow,temperatureHigh,anyDep=TRUE){
  # ANY or ALL depths, default is ANY
  temp <- getTemp(GLMwtr)
  if (anyDep==TRUE){
    tempRangeCount  <-  sum(apply(temp,1,function(x) any(x>=temperatureLow,na.rm=TRUE) & any(x<=temperatureHigh,na.rm=TRUE)))
  }else{
    tempRangeCount  <-  sum(apply(temp,1,function(x) all(x>=temperatureLow,na.rm=TRUE) 
      & all(x<=temperatureHigh,na.rm=TRUE)))
  }
  return(tempRangeCount)
}

getMaxTempIdx <-  function(GLMwtr){
  dailyTempMax  <-  getDailyTempMax(GLMwtr)
  maxTempIdx  <-  which.max(dailyTempMax)
  return(maxTempIdx)
}

getSurfaceT <- function(GLMwtr){
  temp <- getTemp(GLMwtr)
  surfaceTemp <- apply(temp,1,function(x) x[max(which(!is.na(x)))])
  return(surfaceTemp)
}

getBottomT <- function(GLMwtr){
  temp <- getTemp(GLMwtr)
  bottomTemp <- apply(temp,1,function(x) x[min(which(!is.na(x)))])
  return(bottomTemp)
}

getIceOffDate <- function(GLMice,GLMwtr){
  if(diff(range(GLMwtr$DateTime)) > as.difftime(366,units="days")){
    stop("GLM ice time series must be equal or shorter than one year")
  }
  maxTempIdx <-  as.numeric(getMaxTempIdx(GLMwtr))
    # now, look backwards
  iceOffIdx <-  max(which(GLMice[iceID][1:maxTempIdx,]!=0))+1
  iceOffDOY <-  GLMice[timeID][iceOffIdx,]
  return(iceOffDOY)
}

getIceOnDate  <-  function(GLMice,GLMwtr){
  if(diff(range(GLMwtr$DateTime)) > as.difftime(366,units="days")){
    stop("GLM ice time series must be equal or shorter than one year")
  }
  maxTempIdx <-  as.numeric(getMaxTempIdx(GLMwtr))
    # now, look forwards
  iceOnIdx <-  min(which(GLMice[iceID][maxTempIdx:nrow(GLMice[iceID]),]!=0))+(maxTempIdx-1)
  iceOnDOY <-  GLMice[timeID][iceOnIdx,]
  
  return(iceOnDOY)
}

getLastDayAboveT <-  function(GLMwtr,temperature,anyDep=TRUE){
  if(diff(range(GLMwtr$DateTime)) > as.difftime(366,units="days")){
    stop("GLM ice time series must be equal or shorter than one year")
  }
  if (anyDep==TRUE){
    tempRef <-  getDailyTempMax(GLMwtr)
    lastIdx <-  max(which(tempRef>temperature))
  }
  else{
    tempRef <-  getDailyTempMin(GLMwtr)
    lastIdx <-  max(which(tempRef>temperature))
  }
  lastDOYabove  <-  GLMwtr[timeID][lastIdx,]
  return(lastDOYabove)
}

getFirstDayAboveT <-  function(GLMwtr,temperature,anyDep=TRUE){
  if(diff(range(GLMwtr$DateTime)) > as.difftime(366,units="days")){
    stop("GLM ice time series must be equal or shorter than one year")
  }
  if (anyDep==TRUE){
    tempRef <-  getDailyTempMax(GLMwtr)
    lastIdx <-  min(which(tempRef>temperature))
  }
  else{
    tempRef <-  getDailyTempMin(GLMwtr)
    lastIdx <-  min(which(tempRef>temperature))
  }
  firstDOYabove  <-  GLMwtr[timeID][lastIdx,]
  return(firstDOYabove)
}


getStratifiedDuration <-  function(GLMwtr,GLMice,minStrat){
  if(diff(range(GLMwtr$DateTime)) > as.difftime(366,units="days")){
    stop("GLM ice time series must be equal or shorter than one year")
  }
  # advised that the input is shortened to the ice-free period,
  startDate <- as.character(getIceOffDate(GLMice,GLMwtr))
  stopDate <- as.character(getIceOnDate(GLMice,GLMwtr))
  GLMwtr <- subsetTime(GLMwtr,startDate,stopDate)
  tempMxMn <- cbind(getDailyTempMax(GLMwtr),getDailyTempMin(GLMwtr)) 
  stratDur  <-  sum(tempMxMn[,1]-tempMxMn[,2]>=minStrat)
  return(stratDur)
}

getStratifiedStartEnd <-  function(GLMwtr,GLMice,minStrat){
  if(diff(range(GLMwtr$DateTime)) > as.difftime(366,units="days")){
    stop("GLM ice time series must be equal or shorter than one year")
  }
  # advised that the input is shortened to the ice-free period,
  startDate <- as.character(getIceOffDate(GLMice,GLMwtr))
  stopDate <- as.character(getIceOnDate(GLMice,GLMwtr))
  GLMwtr <- subsetTime(GLMwtr,startDate,stopDate)
  tempMxMn <- cbind(getDailyTempMax(GLMwtr),getDailyTempMin(GLMwtr)) 
  startEndI <-  which(tempMxMn[,1]-tempMxMn[,2]>=minStrat)
  
  return(GLMwtr$DateTime[c(min(startEndI),max(startEndI))])
}


