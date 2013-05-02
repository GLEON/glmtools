timeID	<-	"DateTime"
elvID	<-	"elv_"
depID	<-	"wtr_"


getGLMnc  <-  function(fileName='output.nc',folder='../Data/'){
	require("ncdf")
	filePath<-  paste(c(folder,fileName),collapse="")
	GLMnc	<- 	open.ncdf(filePath)
	return(GLMnc)
}

getTimeGLMnc  <-  function(GLMnc){
	hoursSince  <-   get.var.ncdf(GLMnc, "time")
	timeInfo <- getTimeInfo(GLMnc)
  
	time <- timeInfo$startDate + timeInfo$dt * hoursSince * 60*60*24
    
	return(time)
}

getIceGLMnc <-  function(GLMnc){
	ice  	<- 	get.var.ncdf(GLMnc, "hice")+get.var.ncdf(GLMnc, "hwice")+get.var.ncdf(GLMnc, "hsnow")
	return(ice)
}

getWndGLMnc <-  function(GLMnc){
	wnd  	<- 	get.var.ncdf(GLMnc, "wind")
	return(wnd)
}

subsampleGLM	<-	function(GLM, sampleTime, sampleDepths){
	# sample at depths of 'sampleElev' at time 'sampleDepths'
	glmElev	<-	getElevGLM(GLM)
	surfaceElevs <-  getSurfaceElevGLM(GLM)
	dates	<-	GLM$DateTime
	time	<-	as.POSIXct(sampleTime)
	diffs	<-	abs(dates-time)
	uIndx	<-	which.min(diffs)	# NEED good way to interpolate temporally to get exact time...
	interpElevs	<-	surfaceElevs[uIndx]-sampleDepths	# now are elevations
	drops <- c(timeID)
	temp <- as.numeric(GLM[uIndx,!(names(GLM) %in% drops)])
	wtr	<-	approx(glmElev,temp,xout=interpElevs)
	if (as.numeric(diffs[uIndx])<24){
		return(wtr$y)
	}
	else{return(wtr*NA)}
}

depthsampleGLM	<-	function(GLM, sampleDepths){
	# sample at depths of 'sampleDepths' at time all sample times
	GLMnew	<-	GLM$DateTime
	wtrOut	<-	matrix(nrow=length(GLMnew),ncol=length(sampleDepths))
	frameNms<-letters[seq( from = 1, to = (length(sampleDepths)+1))]
  	frameNms[1] <- timeID
	for (z in 2:(length(sampleDepths)+1)){
    	frameNms[z]  <- paste(c(depID,as.character(sampleDepths[z-1])),collapse="")
  	}
	wtrOut <-	data.frame(wtrOut)
	GLMnew	<-	cbind(GLMnew,wtrOut)
	for (tme in 1:nrow(GLMnew)){
		GLMnew[tme,2:(length(sampleDepths)+1)]	<-	subsampleGLM(GLM,GLM$DateTime[tme],sampleDepths)
	}
	names(GLMnew)	<-	frameNms
	GLM	<-	GLMnew
	return(GLM)
}


resampleGLM	<-	function(GLMnc, lyrDz=0.25){
	# uniform grid resampling of GLMnc
	elev	<- 	get.var.ncdf(GLMnc, "z" )
	
	wtr		<- 	get.var.ncdf(GLMnc, "temp")
	rmvI	<- 	which(wtr>=1e30 | elev>=1e30)
	elev[rmvI]	<- NA
	mxElv	<-	max(elev,na.rm = TRUE)+lyrDz
	mnElv	<-	min(elev,na.rm = TRUE)-lyrDz

	elevOut	<-	seq(mnElv,mxElv,lyrDz)
	
	time <- getTimeGLMnc(GLMnc)
	numStep <- length(time)
  	numDep	<-  length(elevOut)
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
  	GLM <- data.frame(time)
  	GLM <- cbind(GLM,wtrOut)
	frameNms<-letters[seq( from = 1, to = numDep+1 )]
  	frameNms[1] <- timeID

  	for (z in 1:numDep){
    	frameNms[z+1]  <- paste(c(elvID,as.character(elevOut[z])),collapse="")
  	}
	names(GLM)	<- frameNms
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

getTimeInfo <- function(GLMnc){
  daySecs = 86400
  
  #The units attribute on the 
  timeUnits <- att.get.ncdf(GLMnc,'time','units')$value
  
  tiCheck <- regexpr('(hours since) (.*)' ,timeUnits, perl=TRUE)
  
  #make sure the unit string is as expected. I think 
  # the timestep is always in hours
  if(attr(tiCheck,'capture.start')[1] < 0 || attr(tiCheck,'capture.start')[2] < 0){
	  stop('Unexpected time unit in NetCDF file')
  }
  
  # Get the epoch from the unit string
  epoch <- substr(timeUnits, attr(tiCheck,'capture.start')[2], attr(tiCheck,'capture.start')[2] + attr(tiCheck,'capture.length')[2])
  
  #get the length of the time data, will use this later
  tLen <- GLMnc$dim[["time"]][["len"]]
  
  # Assume dt is one hour (in fraction of day)
  dt <- 3600/daySecs
  timeInfo  <-  data.frame("dt"=dt)
  timeInfo  <-  cbind(timeInfo,"startDate"=as.POSIXct(epoch))
  
  #End date/time 
  endT <- timeInfo$startDate + get.var.ncdf(GLMnc,'time',start=tLen, count=1) * dt * daySecs
  
  timeInfo  <-  cbind(timeInfo,"stopDate"=endT)
  return(timeInfo)
}

writeGLM  <- function(GLM,fileName="GLMout.txt",folder=""){
  # writes GLM file to directory
  fileOut <- paste(c(folder,fileName),collapse="")
  write.table(GLM,file=fileOut,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}

writeWTR  <- function(GLM,fileName="GLM.wtr",folder=""){
  # writes GLM file to directory
  fileOut <- paste(c(folder,fileName),collapse="")
  write.table(GLM,file=fileOut,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}

writeWND  <- function(GLMnc,fileName="GLM.wnd",folder=""){
  # writes GLM file to directory
	GLM	<-	data.frame(getTimeGLMnc(GLMnc))
	GLM	<-	cbind(GLM,getWndGLMnc(GLMnc))
	names(GLM)	<-	c(timeID,"wnd")
	fileOut <- paste(c(folder,fileName),collapse="")
	write.table(GLM,file=fileOut,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}

getSurfaceElevGLM	<-	function(GLM){
	# returns a vector of elevations that correspond to the water surface
	elevs	<-	getElevGLM(GLM)
	drops	<-	c(timeID)
	temp	<-	GLM[,!(names(GLM) %in% drops)]
	surface <- apply(temp,1,function(x) elevs[max(which(!is.na(x)))])
	return(surface)
}

getElevGLM <- function(GLM){
  colNames <- names(GLM)
  elevs <- gsub(elvID,"",colNames[2:length(colNames)])
  return(as.numeric(elevs))
}
  
plotGLM  <- function(GLM,figName="glmPlot",folder="",cLim=c(0,30)){
  # saves GLM plot to directory
  
  elevs <-  getElevGLM(GLM)
  lvls  <-  seq(cLim[1],cLim[2])
  figW  <-  8
  figH  <-  3.5
  lM    <-  .95
  bM    <-  .55
  rM    <-  .15
  tM    <-  0.25
  fRes  <-  200
  fontN <-  11
  xL    <-  c(as.numeric(min(GLM$DateTime)),as.numeric(max(GLM$DateTime)))
  yL    <-  c(min(elevs,na.rm=TRUE),max(elevs,na.rm=TRUE))
  cMap  <-  rev(rainbow(length(lvls),s=1,v=1,start=0,end=4/6))
  
  vals <- data.matrix(GLM)
  output = paste(folder,figName,".png", sep = "")
  png(output, width=figW, height=figH, units="in",res=fRes)
  par(mai=c(bM,lM,rM,tM),usr=c(xL[1],xL[2],yL[1],yL[2]))
  wtr <- vals[,2:(length(elevs)+1)]
  filled.contour(x=GLM$DateTime,y=elevs,z=wtr,col = cMap,
    	levels=lvls,xaxs = "i",plot.title = title(ylab = "Elevation from bottom (m)"),
    	xlim=xL, ylim=yL, xaxp = c(xL[1],xL[2],50))
  dev.off()
}
