
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

resampleGLM	<-	function(GLMnc, lyrDz=0.25){
	
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
	frameNms<-letters[seq( from = 1, to = numDep )]
  frameNms[1] <- "DateTime"

  for (z in 1:numDep){
    frameNms[z+1]  <- paste(c("wtr_",as.character(elevOut[z])),collapse="")
  }
	names(GLM)<- frameNms
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

getElevGLM <- function(GLM){
  colNames <- names(GLM)
  elevs <- gsub("wtr_","",colNames[2:length(colNames)])
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
