# ------Helper functions for interacting with GLM for running Lake Analyzer-----
# ------Jordan and Luke 2013

init.lke	<-	function(){
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
	return(lke)
}

	
init.lkeMeta	<-	function(){
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

get.lvl	<-	function(GLMnc,nml){
	DateTime	<-	getTimeGLMnc(GLMnc)
	GLM	<-	getTempGLMnc(GLMnc,lyrDz=0.25)
	surfaceElv	<-	getSurfaceElevGLM(GLM)
	mxDep	<-	getMaxDepth(nml)
	dif	<-	mxDep-surfaceElv
	dif[dif<0]	<-	0
	lvl	<-	data.frame(DateTime)
	lvl	<-	cbind(lvl,dif)
	names(lvl)	<-	c("DateTime","level(positive Z down in meters)")
	return(lvl)
}

################################################################################
# Summary: gets bathymetry formatted as depths and areas
#
# Input:
#	nml: the glm.nml R "list" of parameter names and values
get.bth	<-	function(nml){
	
	mxElv	<-	get.nml(nml,'crest_elev')
	heights	<-	get.nml(nml,'H')
	bthA	<-	rev(get.nml(nml,'A')*1000) # now m2
	bthZ	<-	rev(mxElv-heights)
	bth	<-	data.frame(bthZ,bthA)
	colnames(bth)	<-	c("Bathymetry depths (m)","Bathymetry areas (m2)")
	return(bth)
}

get.wtr	<-	function(GLMnc,depths,lyrDz=0.25){
	GLMwtr	<-	getTempGLMnc(GLMnc,lyrDz)
	wtr	<-	depthsampleGLM(GLMwtr, depths)
	return(wtr)
}

get.wnd	<-	function(GLMnc){
	wnd	<-	data.frame(getTimeGLMnc(GLMnc))
	wnd	<-	cbind(wnd,getWndGLMnc(GLMnc))
	names(wnd)	<-	c(timeID,"wnd")
	return(wnd)
}

set.lke	<-	function(lke,argName,argVal){
	lke[argName]	<-	argVal
	return(lke)
}

write.lke	<-	function(lke,lakeName = 'lake',folder='../Supporting Files/'){	
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

write.bth	<-	function(bth,lakeName='lake',folder='../Supporting Files/'){	
	fileN	<-	paste(c(folder,lakeName,'.bth'),collapse="")
	write.table(bth,file=fileN,col.names=TRUE, quote=FALSE, row.names=FALSE, sep=",")
}

################################################################################
# Summary: writes GLM file that has been inverted using depthsampleGLM to directory
#
# Input:
#	wtr: a GLM wtr data frame (see getWTR)
write.wtr  <- function(wtr,lakeName='lake',folder='../Supporting Files/'){
  fileOut <- paste(c(folder,lakeName,'.wtr'),collapse="")
  write.table(wtr,file=fileOut,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}

write.lvl  <- function(lvl,lakeName='lake',folder='../Supporting Files/'){
  fileOut <- paste(c(folder,lakeName,'.lvl'),collapse="")
  write.table(lvl,file=fileOut,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}

################################################################################
# Summary: writes GLM wind file to directory
#
# Input:
#	wnd: a GLM wnd data frame (see getWND)
write.wnd  <- function(wnd,lakeName='lake',folder='../Supporting Files/'){
  # writes GLM file to directory
	fileOut <- paste(c(folder,lakeName,'.wnd'),collapse="")
	write.table(wnd,file=fileOut,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
}

zipLA	<-	function(lakeName='lake',folder='../Supporting Files/',destFldr='../Supporting Files/'){
	# finds all folders in the directory, zips and moves to destFldr. Fails w/o .lke file
	files	<-	list.files(folder,pattern=lakeName,full.names=TRUE)
	if (!any(files==paste(c(folder,lakeName,'.lke'),collapse=''))){stop("need *.lke file for zip")}
	
	zipFile	<-	paste(c(destFldr,lakeName),collapse='')
	zip(zipFile,files)
	# return success message?
}

runRemoteLA	<-	function(zipPath, destFldr='../Supporting Files'){
	require(RCurl)
	file_h	<-	postForm("http://lakeanalyzer.gleon.org/postRunModel.php",
	          runzip = fileUpload(zipPath), binary=TRUE)
	# now move file_h to destFldr
  attributes(file_h) = NULL
  writeBin(file_h, file.path(destFldr,'output.zip'))
}
