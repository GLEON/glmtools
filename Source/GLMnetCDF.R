
resampleGLM	<-	function(fileName='output.nc',folder='../Data/',lyrDz=0.25){
	require("ncdf")
  filePath<-  paste(c(folder,fileName),collapse="")
	GLM.nc	<- 	open.ncdf(filePath)
	elev	<- 	get.var.ncdf(GLM.nc, "z" )
	dateIdx	<- 	get.var.ncdf(GLM.nc, "time")
	wtr		<- 	get.var.ncdf(GLM.nc, "temp")
	rmvI	<- 	which(wtr>=1e30 | elev>=1e30)
	elev[rmvI]	<- NA
	mxElv	<-	max(elev,na.rm = TRUE)+lyrDz
	mnElv	<-	min(elev,na.rm = TRUE)-lyrDz

	elevOut	<-	seq(mnElv,mxElv,lyrDz)
	numStep <-	ncol(wtr)
  timeInfo <- getTimeInfo(paste(c(folder,'glm.nml'),collapse=""))  # should find dir from fileName if possible...
  time <- seq(timeInfo$startDate,timeInfo$stopDate,timeInfo$dt)
  time <- time[dateIdx]
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

getTimeInfo <- function(nmlFileName){
  daySecs <- 86400
  # returns start time and dt as a date from the *.nml file
  blockOpen   <-  '&time' 
  blockClose  <-  '/' 
  # find and read the time block
  c <- file(nmlFileName,"r") 
  fileLines <- paste(readLines(c),collapse='')
  close(c)
  timeText <-  paste(getTextUntil(fileLines,blockOpen,blockClose),collapse='')
  timeText <-  gsub(" ","",timeText)
  dt  <-  as.numeric(getTextUntil(timeText,'dt='))/daySecs
  timeInfo  <-  data.frame("dt"=dt)
  timeInfo  <-  cbind(timeInfo,"startDate"=as.Date(getTextUntil(timeText,"start='","'")))
  timeInfo  <-  cbind(timeInfo,"stopDate"=as.Date(getTextUntil(timeText,"stop='","'")))
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
