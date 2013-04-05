
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

getTxtUntil <- function(readText,openStr,closeStr){
  # get text between FIRST startStr and the FIRST occurance of endStr
  openI <- head(grep(openStr,readText),n=1)
  closeBlck <-  grep(closeStr,readText)
  closeI  <-  head(closeBlck[closeBlck > openI], n=1)
  return(readText[(openI+1):(closeI-1)])
}

getTimeInfo <- function(fileName){
  # returns start time and dt as a date from the *.nml file
  blockOpen   <-  '&time' 
  blockClose  <-  '/' 
  # find and read the time block
  c <- file(fileName,"r") 
  fileLines <- readLines(c)
  close(c)
  openI <- grep(blockOpen,fileLines)
  closeBlck <-  grep(blockClose, fileLines)
  closeI  <-  head(closeBlck[closeBlck > openI], n=1)
  timeTxt <-  paste(fileLines[(openI+1):(closeI-1)],collapse = "")
  timeTxt <-  gsub(" ","",timeTxt)
  startI  <-  greb(timeTxt,'start=')
  dtI <-  greb(timeTxt,'dt=')
  return(timeTxt)
  
  
}
