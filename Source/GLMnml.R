lke	<-	list(LA_out = c('metaB','SmetaB','SmetaT','SthermD','SLn','SW','SN2'),
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



LA_out	<-	c('metaB','SmetaB','SmetaT','SthermD','SLn','SW','SN2')

setLke	<-	function(argName,argVal){
	# set vals here!
}

getMaxDepth	<-	function(nml){
	
	return(maxDepth)
}

writeLKE	<-	function(lke,outputs=stdLA_out)
{
	# write the file!
}