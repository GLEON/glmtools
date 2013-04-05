
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
	GLM	<- list(Time=dates,Elevation=elevOut,Temperature=wtrOut)
	return(GLM)
}