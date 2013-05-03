# ------Helper functions for interacting with glm.nml files-----
# ------Jordan and Luke 2013

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

getBTH	<-	function(nml){
	mxElv	<-	nml$crest_elev
	heights	<-	nml$H
	bthA	<-	rev(nml$A*1000) # now m2
	bthZ	<-	rev(mxElv-heights)
	bth	<-	data.frame(bthZ,bthA)
	colnames(bth)	<-	c("Bathymetry depths (m)","Bathymetry areas (m2)")
	return(bth)
}

getNML	<-	function(folder='../Data/',fileName='glm.nml'){
	# skip all commented lines, return all variables and associated values
	# requires NO return line variables (all variables must be completely defined on a single line)
	fileN	<-	paste(c(folder,fileName),collapse="")
	c <- file(fileN,"r") 
	fileLines <- readLines(c)
	close(c)
	ignoreLn	<-	sort(c(grep("!",fileLines),grep("/",fileLines),grep("&",fileLines)))
	
	# will still include blank lines
	nml	<-	list()
	for (i in 1:length(fileLines)){
		if (!any(i==ignoreLn)){
			if (nchar(fileLines[i])>2){	# get rid of blank lines **issue w/ data format!**
				# replace blanks
				fileLines[i]	<-	gsub("\t","",gsub(" ","",fileLines[i]))
				nml	<-	buildNML(nml,fileLines[i])
			}
		}
	}	
	return(nml)
}

buildNML	<-	function(nml,textLine){
	#-----function appends nml list with new values-----
	if (!any(grep("=",textLine))){stop(c("no hanging lines allowed in .nml, used ",textLine))}
	params	<-	strsplit(textLine,"=") # break text at "="
	parNm	<-	params[[1]][1]
	parVl	<-	params[[1]][2]
	# figure out what parval is...if string, remove quotes and keep as string
	# ***for boolean text, use "indentical" so that 0!= FALSE
	# can be: string, number, comma-sep-numbers, or boolean
	if (any(grep("'",parVl))){
		parVl	<-	gsub("'","",parVl)
	}else if (any(grep(".true.",parVl))){
		parVl	<-	TRUE
	}else if (any(grep(".false.",parVl))){
		parVl	<-	FALSE
	}else if (any(grep(",",parVl))){	# comma-sep-nums
		parVl	<-	c(as.numeric(unlist(strsplit(parVl,","))))
	}else {	# test for number
		parVl	<-	as.numeric(parVl)
	}
	addI	<-	length(nml)+1
	oldNms	<-	names(nml)
	nml[[addI]]	<-	parVl
	names(nml)	<-	c(oldNms,parNm)
	return(nml)
}

setLKE	<-	function(lke,argName,argVal){
	lke[argName]	<-	argVal
	return(lke)
}

getLakeName	<-	function(nml){
	lakeName	<-	nml$lake_name
	return(lakeName)
}

getMaxDepth	<-	function(nml){
	mxElv	<-	nml$crest_elev
	mnElv	<-	nml$base_elev
	maxDepth	<-	mxElv-mnElv
	return(maxDepth)
}

writeLKE	<-	function(lke,folder='../Supporting Files/',fileName='lake.lke'){	
	lkeMeta	<-	getLkeMeta()
	if (any(is.na(lke))){stop("no lke parameters can be NA")}
	
	sink(paste(c(folder,fileName),collapse=""))
	cat(c("Configuration file for Lake X","\n","\n"))
	for (ln in 1:length(lke)){
		cat(as.character(lke[[ln]]))
		cat(c("\t","\t",lkeMeta[[names(lke[ln])]],"\n"))
	}
	sink()
}

writeBTH	<-	function(bth,folder='../Supporting Files/',fileName='lake.bth'){	
	fileN	<-	paste(c(folder,fileName),collapse="")
	write.table(bth,file=fileN,col.names=TRUE, quote=FALSE, row.names=FALSE, sep=",")
}