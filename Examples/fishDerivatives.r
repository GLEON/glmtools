# fish code for Gretchen 


# -- shared variables --
timeID  <-  "DateTime"

getGLM  <-  function(folder){
  source('../Source/GLMnetCDF.R')
  GLM  <-	resampleGLM(fileName='output.nc',folder=folder)
  return(GLM)
}

subsetTime <- function(GLM,startDate,stopDate){
  # gets rid of GLM simulation results that are < startDate and > stopDate
  # startDate and stopDate must be of type "Date"
  dates <-  GLM[timeID]
  useI  <-  (dates >= startDate) & (dates <= stopDate)
  GLM <-  GLM[useI,]
  return(GLM)
}

getTemp   <- function(GLM){
  drops <- c(timeID)
  temp <- GLM[,!(names(GLM) %in% drops)]
  return(temp)
}

getDailyTempMax <- function(GLM){
  temp <- getTemp(GLM)
  dailyTempMax <-  apply(temp,1,function(x) max(x,na.rm=TRUE))
  return(dailyTempMax)
}

getDailyTempMin <- function(GLM){
  temp <- getTemp(GLM)
  dailyTempMin <-  apply(temp,1,function(x) min(x,na.rm=TRUE))
  return(dailyTempMin)
}

getTempMax <- function(GLM){
  dailyTempMax <-  getDailyTempMax(GLM)
  tempMax <-  max(dailyTempMax)
  return(tempMax)
}

getTempMin <- function(GLM){
  dailyTempMin <-  getDailyTempMin(GLM)
  tempMin <-  min(dailyTempMin)
  return(tempMin)
}

getDaysAboveT <- function(GLM,temperature,anyDep=TRUE){
  # ANY or ALL depths, default is ANY
  if (anyDep==TRUE){refTemp  <-  getDailyTempMax(GLM)}
  else {refTemp <-  getDailyTempMin(GLM)}

  tempAboveCount  <-  sum(refTemp > temperature,na.rm=TRUE)
  return(tempAboveCount)
}

getDaysBelowT <- function(GLM,temperature,anyDep=TRUE){
  # ANY or ALL depths, default is ANY
  if (anyDep==TRUE){refTemp  <-  getDailyTempMin(GLM)}
  else {refTemp <-  getDailyTempMax(GLM)}
  
  tempBelowCount  <-  sum(refTemp < temperature,na.rm=TRUE)
  return(tempBelowCount)
}

getDaysBetweenT <-  function(GLM,temperatureLow,temperatureHigh,anyDep=TRUE){
  # ANY or ALL depths, default is ANY
  temp <- getTemp(GLM)
  if (anyDep==TRUE){
    tempRangeCount  <-  sum(apply(temp,1,function(x) any(x>=temperatureLow) & any(x<=temperatureHigh)))
  }
  else{
    tempRangeCount  <-  sum(apply(temp,1,function(x) all(x>=temperatureLow,na.rm=TRUE) 
      & all(x<=temperatureHigh,na.rm=TRUE)))
  }
  return(tempRangeCount)
}

getSurfaceT <- function(GLM){
  surfaceTemp <- apply(temp,1,function(x) x[max(which(!is.na(x)))])
  return(surfaceTemp)
}

getBottomT <- function(GLM){
  bottomTemp <- apply(temp,1,function(x) x[min(which(!is.na(x)))])
  return(bottomTemp)
}

getIceOffDate <- function(GLM){
  return(iceOffDOY)
}

getIceOnDate  <-  function(GLM){
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