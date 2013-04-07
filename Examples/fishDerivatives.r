# fish code for Gretchen 

subsetTime <- function(GLM,startDate,stopDate){
  # gets rid of GLM simulation results that are < startDate and > stopDate
  
}

getTempMax <- function(GLM){
  return(tempMax)
}

getTempMin <- function(GLM){
  return(tempMin)
}


getDaysAboveT <- function(GLM,temperature){
  return(tempAboveCount)
}

getDaysBelowT <- function(GLM,temperature){
  return(tempBelowCount)
}

getDaysBetweenT <-  function(GLM,temperatureLow,temperatureHigh){
  return(tempRangeCount)
}

getSurfaceT <- function(GLM){
  return(surfaceTemp)
}

getIceOffDate <- function(GLM,year){
  return(iceOffDOY)
}

getIceOnDate  <-  function(GLM,year){
  return(iceOnDOY)
}

getLastDayAboveT <-  function(GLM){
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