# ------Helper functions for interacting with GLM for running Lake Analyzer-----
# ------Jordan and Luke 2013
source('GLMnetCDF.R')
source('GLMnml.R')

getLke	<-	function(){
	lke	<-	list(LA_out = paste('metaB','SmetaB','SmetaT','SthermD','SLn','SW','SN2',sep=", "),
		outRes = 86400,
		totalDep = NA,
		wndHeight = 2,
		wndAve	= 86400,
		thermalAve	= 86400,
		outlierWin	= 21600,
		maxT	= 40,
		minT	= -12,
		maxU	= 98,
		minU	= 0,
		metaSlp	= 0.1,
		mixDif	= 0.5,
		plotFig = 'Y',
		writeRes= 'Y')
}

	
getLkeMeta	<-	function(){
	lkeMeta	<-	list(LA_out = "#outputs",
			outRes = "#output resolution (s)",
			totalDep = "#total depth (m)",
			wndHeight = "#height from surface for wind measurement (m)",
			wndAve	= "#wind averaging (s)",
			thermalAve	= "#thermal layer averaging (s)",
			outlierWin	= "#outlier window (s)",
			maxT	= "#max water temp (°C)    inf if none",
			minT	= "#min water temp (°C)    -inf if none",
			maxU	= "#max wind speed (m/s)   inf if none",
			minU	= "#min wind speed (m/s)   -inf if none",
			metaSlp	= "#meta min slope (drho/dz per m)",
			mixDif	= "#mixed temp differential (°C)",
			plotFig = "#plot figure (Y/N)",
			writeRes= "#write results to file (Y/N)")
	return(lkeMeta)
}

getLVL	<-	function(GLMnc,nml){
	DateTime	<-	getTimeGLMnc(GLMnc)
	GLM	<-	getTempGLMnc(GLMnc)
	surfaceElv	<-	getSurfaceElevGLM(GLM)
	mxElv	<-	getMaxElevation(nml)
	
	lvl	<-	data.frame(DateTime)
	lvl	<-	cbind(lvl,(mxElv-surfaceElv))
	names(lvl)	<-	c("DateTime","level(positive Z down in meters)")
	return(lvl)
}

################################################################################
# Summary: gets bathymetry formatted as depths and areas
#
# Input:
#	nml: the glm.nml R "list" of parameter names and values
getBTH	<-	function(nml){
	
	mxElv	<-	nml$crest_elev
	heights	<-	nml$H
	bthA	<-	rev(nml$A*1000) # now m2
	bthZ	<-	rev(mxElv-heights)
	bth	<-	data.frame(bthZ,bthA)
	colnames(bth)	<-	c("Bathymetry depths (m)","Bathymetry areas (m2)")
	return(bth)
}

setLKE	<-	function(lke,argName,argVal){
	lke[argName]	<-	argVal
	return(lke)
}

writeLKE	<-	function(lke,lakeName = 'lake',folder='../Supporting Files/'){	
	lkeMeta	<-	getLkeMeta()
	
	if (any(is.na(lke))){stop("no lke parameters can be NA")}
	fileName	<-	gsub(" ","",paste(c(lakeName,'.lke'),collapse=""))
	sink(paste(c(folder,fileName),collapse=""))
	cat(c("Configuration file for",lakeName,"\n","\n"))
	for (ln in 1:length(lke)){
		cat(as.character(lke[[ln]]))
		cat(c("\t","\t",lkeMeta[[names(lke[ln])]],"\n"))
	}
	sink()
}

writeBTH	<-	function(bth,fileName='lake',folder='../Supporting Files/'){	
	fileN	<-	paste(c(folder,lakeName,'.bth'),collapse="")
	write.table(bth,file=fileN,col.names=TRUE, quote=FALSE, row.names=FALSE, sep=",")
}

################################################################################
# Summary: writes GLM file that has been inverted using depthsampleGLM to directory
#
# Input:
#	GLM: a GLM water temp dataframe that has been inverted using depthsampleGLM()
writeWTR  <- function(GLM,fileName="GLM.wtr",folder=""){
  fileOut <- paste(c(folder,fileName),collapse="")
  write.table(GLM,file=fileOut,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}

writeLVL  <- function(lvl,fileName="GLM.lvl",folder=""){
  fileOut <- paste(c(folder,fileName),collapse="")
  write.table(lvl,file=fileOut,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}

################################################################################
# Summary: writes GLM wind file to directory
#
# Input:
#	GLM: a GLM netCDF object created with getGLMnc()
writeWND  <- function(GLMnc,fileName="GLM.wnd",folder=""){
  # writes GLM file to directory
	GLM	<-	data.frame(getTimeGLMnc(GLMnc))
	GLM	<-	cbind(GLM,getWndGLMnc(GLMnc))
	names(GLM)	<-	c(timeID,"wnd")
	fileOut <- paste(c(folder,fileName),collapse="")
	write.table(GLM,file=fileOut,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}