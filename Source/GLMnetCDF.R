
resampleGLM	<-	function(fileName,lyrDz=0.25){
	require("ncdf")
	GLM.nc	<- 	open.ncdf(fileName)
	elev	<- 	get.var.ncdf(GLM.nc, "z" )
	dates	<- 	get.var.ncdf(GLM.nc, "time")
	E		<- 	get.var.ncdf(GLM.nc, "evap")
	wtr		<- 	get.var.ncdf(GLM.nc, "temp")
	rmvI	<- 	which(wtr>=1e30 | elev>=1e30)
	elev[rmvI]	<- NA
	mxElv	<-	max(elev,na.rm = TRUE)+lyrDz
	mnElv	<-	min(elev,na.rm = TRUE)-lyrDz

	elevOut	<-	seq(mnElv,mxElv,lyrDz)
	numStep <-	ncol(wtr)
  timeInfo <- getTimeInfo('../Data/glm.nml')  # should find dir from fileName if possible...
  time <- seq(timeInfo$startDate,timeInfo$stopDate,timeInfo$dt)
	numDep	<-	length(elevOut)

	wtrOut	<-	matrix(nrow=numStep,ncol=numDep)

	for (tme in 1:numStep){
		useI	<- which(wtr[,tme]<1e30 & elev[,tme]<1e30)
		useI	<- which(wtr[,tme]<1e30 & elev[,tme]<1e30)
		x		<- elev[useI,tme]
		y		<- wtr[useI,tme]
		if (length(y)>0){
			ap		<-	approx(c(mnElv,x),c(y[1],y),xout=elevOut)
			wtrOut[tme,1:length(ap$y)]	<- ap$y}
	}
	GLM	<- list("Time"=dates,"Elevation"=elevOut,"Temperature"=wtrOut)
	return(GLM)
}

getTextUntil <- function(readText,openStr,closeStr=FALSE){
  # get text between FIRST startStr and the FIRST occurance of endStr
  openI <- head(unlist(gregexpr(openStr,readText)),n=1)+nchar(openStr)
  if(closeStr!=FALSE) {
    closeBlck <-  unlist(gregexpr(closeStr,readText))
    closeI  <-  head(closeBlck[closeBlck > openI], n=1)-nchar(closeStr)}
  else {closeI <- nchar(readText)+1}
  return(substring(readText,openI,closeI))
}

getTimeInfo <- function(fileName){
  daySecs <- 86400
  # returns start time and dt as a date from the *.nml file
  blockOpen   <-  '&time' 
  blockClose  <-  '/' 
  # find and read the time block
  c <- file(fileName,"r") 
  fileLines <- paste(readLines(c),collapse='')
  close(c)
  timeText <-  paste(getTextUntil(fileLines,blockOpen,blockClose),collapse='')
  timeText <-  gsub(" ","",timeText)
  dt<-  as.numeric(getTextUntil(timeText,'dt='))/daySecs
  timeInfo  <-  data.frame("dt"=dt)
  timeInfo  <-  cbind(timeInfo,"startDate"=as.Date(getTextUntil(timeText,"start='","'")))
  timeInfo  <-  cbind(timeInfo,"stopDate"=as.Date(getTextUntil(timeText,"stop='","'")))
  return(timeInfo)
}
