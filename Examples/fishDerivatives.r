# fish code for WiLMA 

# -- shared variables --
timeID  <-  "DateTime"
iceID <-  "Ice"
folder  <-  "../Data/"

getGLMnc  <-  function(folder=folder){
  source('../Source/GLMnetCDF.R')
  GLMnc <- getGLMnc(fileName='output.nc',folder=folder)
  return(GLMnc)
}


getGLMwtr  <-  function(GLMnc){
  GLMwtr <-	resampleGLM(GLMnc)
  return(GLMwtr)
}

getGLMice  <-  function(GLMnc,folder=folder){
  source('../Source/GLMnetCDF.R')
  GLMice <-  data.frame("DateTime"=getTimeGLMnc(GLMnc,folder=folder),"Ice"=getIceGLMnc(GLMnc))
  return(GLMice)
}

subsetTime <- function(GLM,startDate,stopDate){
  # gets rid of GLM simulation results that are < startDate and > stopDate
  # startDate and stopDate must be of type "Date"
  dates <-  GLM[timeID]
  useI  <-  (dates >= startDate) & (dates <= stopDate)
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
  if (anyDep==TRUE){refTemp  <-  getDailyTempMax(GLMwtr)}
  else {refTemp <-  getDailyTempMin(GLMwtr)}

  tempAboveCount  <-  sum(refTemp > temperature,na.rm=TRUE)
  return(tempAboveCount)
}

getDaysBelowT <- function(GLMwtr,temperature,anyDep=TRUE){
  # ANY or ALL depths, default is ANY
  temp <- getTemp(GLMwtr)
  if (anyDep==TRUE){refTemp  <-  getDailyTempMin(GLMwtr)}
  else {refTemp <-  getDailyTempMax(GLM)}
  
  tempBelowCount  <-  sum(refTemp < temperature,na.rm=TRUE)
  return(tempBelowCount)
}

getDaysBetweenT <-  function(GLMwtr,temperatureLow,temperatureHigh,anyDep=TRUE){
  # ANY or ALL depths, default is ANY
  temp <- getTemp(GLMwtr)
  if (anyDep==TRUE){
    tempRangeCount  <-  sum(apply(temp,1,function(x) any(x>=temperatureLow) & any(x<=temperatureHigh)))
  }
  else{
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
  if(nrow(GLMice[iceID])>366) {stop("GLM ice time series must be equal or shorter than one year")}
  maxTempIdx <-  as.numeric(getMaxTempIdx(GLMwtr))
    # now, look backwards
  iceOffIdx <-  max(which(GLMice[iceID][1:maxTempIdx,]!=0))+1
  iceOffDOY <-  GLMice[timeID][iceOffIdx,]
  return(iceOffDOY)
}

getIceOnDate  <-  function(GLM){
  if(nrow(GLMice[iceID])>366) {stop("GLM ice time series must be equal or shorter than one year")}
  maxTempIdx <-  as.numeric(getMaxTempIdx(GLMwtr))
    # now, look forwards
  iceOnIdx <-  min(which(GLMice[iceID][maxTempIdx:nrow(GLMice[iceID]),]!=0))+(maxTempIdx-1)
  iceOnDOY <-  GLMice[timeID][iceOnIdx,]
  return(iceOnDOY)
}

getLastDayAboveT <-  function(GLM,temperature,anyDep=TRUE){
  return(lastDOYabove)
}

getFirstDayAboveT <-  function(GLM){
  return(firstDOYabove)
}

getStratifiedDuration <-  function(GLM){
  return(stratDur)
}

isStratified <- function(GLM){
  return(stratified)
}