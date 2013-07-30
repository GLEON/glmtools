

################################################################################
#
################################################################################
getGLMnc  <-  function(fileName='output.nc',folder='../Data/'){
	require(ncdf4)
	filePath<-  paste(c(folder,fileName),collapse="")
	GLMnc	<- 	nc_open(filePath)
	return(GLMnc)
}

################################################################################
#
################################################################################
# Summary: Returns the converted time vector in R format
getTimeGLMnc  <-  function(GLMnc){
  require(ncdf4)
	hoursSince  <-   ncvar_get(GLMnc, "time")
	timeInfo <- getTimeInfo(GLMnc)

	time <- timeInfo$startDate + timeInfo$dt * hoursSince * 60*60*24

	return(time)
}


################################################################################
#
################################################################################
getIceGLMnc <-  function(GLMnc){
  require(ncdf4)
	ice  	<- 	ncvar_get(GLMnc, "hice")+ncvar_get(GLMnc, "hwice")+ncvar_get(GLMnc, "hsnow")
	return(ice)
}

getWndGLMnc <-  function(GLMnc){
  require(ncdf4)
	wnd  	<- 	ncvar_get(GLMnc, "wind")
	return(wnd)
}

subsampleGLM	<-	function(GLM, sampleTime, sampleDepths){
  
	# sample at depths of 'sampleElev' at time 'sampleDepths'
	glmElev	<-	getElevGLM(GLM)
	surfaceElevs <-  getSurfaceElevGLM(GLM)
	dates	<-	GLM$DateTime
	times	<-	as.POSIXct(sampleTime)
	output = matrix(NaN, nrow=length(sampleTime), ncol=length(sampleDepths))
  
  for (i in 1:length(times)){
    time = times[i]
	  diffs	<-	abs(dates-time)
	  uIndx	<-	which.min(diffs)	# NEED good way to interpolate temporally to get exact time...
    
    
	  interpElevs	<-	surfaceElevs[uIndx]-sampleDepths	# now are elevations
	  drops <- c("DateTime")
	  temp <- as.numeric(GLM[uIndx,!(names(GLM) %in% drops)])
	
		if (length(temp[!is.na(temp)])>1){
			wtr	<-	approx(glmElev,temp,xout=interpElevs)
		} else {
			wtr	<-	data.frame(y=interpElevs*NA)
			}
	  
    
    if (as.numeric(diffs[uIndx])<24){
      output[i,] = wtr$y
    }
    
  }
  
	return(output)
}

depthsampleGLM	<-	function(GLM, sampleDepths){
	# sample at depths of 'sampleDepths' at time all sample times
	GLMnew	<-	GLM$DateTime
	wtrOut	<-	matrix(nrow=length(GLMnew),ncol=length(sampleDepths))
	frameNms<-letters[seq( from = 1, to = (length(sampleDepths)+1))]
  	frameNms[1] <- "DateTime"
	for (z in 2:(length(sampleDepths)+1)){
    	frameNms[z]  <- paste(c("wtr_",as.character(sampleDepths[z-1])),collapse="")
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


################################################################################
#
################################################################################
getTempGLMnc <-  function(GLMnc, lyrDz=0.5, lyr.elevations){
  require(ncdf4)
  
	#The last useful index
	NS	<- 	ncvar_get(GLMnc, "NS")
	
	#The max index of valid data, clip the input matricies
	maxInd = max(NS)
	
	
	#Get the surface elevation vector from the NetCDF file
	elev	<- 	ncvar_get(GLMnc, "z" )
	elev	<-	elev[1:maxInd,]
	
	#Grab water temperature from NC file
	wtr	<- 	ncvar_get(GLMnc, "temp")
	wtr 	<-	wtr[1:maxInd,]
	
	#No temperature or elevation should be > 1e30, should be converted to NA
	rmvI	<- 	which(wtr>=1e30 | elev>=1e30)
	elev[rmvI]	<- NA
	mxElv	<-	max(elev,na.rm = TRUE)+lyrDz
	mnElv	<-	min(elev,na.rm = TRUE)-lyrDz
	 
  if(missing(lyr.elevations)){
	  elevOut	<-	seq(mnElv,mxElv,lyrDz)
  }else{
  	elevOut = lyr.elevations
  }
  
  #We want to include time with the output as well
  time <- getTimeGLMnc(GLMnc)
  numStep = length(time)  
  
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
    frameNms[z+1]  <- paste(c("elv_",as.character(elevOut[z])),collapse="")
  }
  names(GLM)<- frameNms
  return(GLM)
}


################################################################################
#
################################################################################
# Summary: Determines the epoch and timestep to properly convert the model date/time
#
# Input:
#	GLMnc:	The ncdf file object reference, from nc_open
# 	
getTimeInfo <- function(GLMnc){
  require(ncdf4)
	daySecs = 86400

	#The units attribute on the time variable has basically the info we need
	timeUnits <- ncatt_get(GLMnc,'time','units')$value

	#It is written in prose instead of machine-readable format. Check to makes sure
	# it says "hours since ", then we know the timestep is hours. As far as I know, 
	# this never changes
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

	# Assume dt is one hour (in fraction of day) because it didn't fail earlier
	dt <- 3600/daySecs
	timeInfo  <-  data.frame("dt"=dt)
	timeInfo  <-  cbind(timeInfo,"startDate"=as.POSIXct(epoch))

	#End date/time 
	endT <- timeInfo$startDate + ncvar_get(GLMnc,'time',start=tLen, count=1) * dt * daySecs

	timeInfo  <-  cbind(timeInfo,"stopDate"=endT)
	return(timeInfo)
}


getSurfaceElevGLM	<-	function(GLM){
	# returns a vector of elevations that correspond to the water surface
	elevs	<-	getElevGLM(GLM)
	drops	<-	c("DateTime")
	temp	<-	GLM[,!(names(GLM) %in% drops)]
	surface <- apply(temp,1,function(x) elevs[max(which(!is.na(x)))])
	return(surface)
}

################################################################################
#
################################################################################
getElevGLM <- function(GLM){
  colNames <- names(GLM)
  elevs <- gsub("elv_","",colNames[2:length(colNames)])
  return(as.numeric(elevs))
}

################################################################################
#
################################################################################
