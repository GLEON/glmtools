# ------Helper functions for interacting with glm.nml files-----
# ------Jordan and Luke 2013


getNML	<-	function(fileName='glm.nml',folder='../Data/'){
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
setNML	<-	function(nml,argName,argVal){
	nml[argName]	<-	argVal
	return(nml)
}

writeNML	<-	function(nml,fileName='glm.nml',folder='../Data/'){
	sink(paste(c(folder,fileName),collapse=''))
	for (i in 1:length(names(nml))){
		cat(names(nml[i]))
		cat('=')
		if (length(nml[[i]]>1)){
			writer	<-	paste(nml[[i]],colapse=',')
		} else {write	<-	nml[[1]]}
		cat(nml[[i]])
		cat('\n')
	}	
	sink()
}

getLakeName	<-	function(nml){
	lakeName	<-	nml$lake_name
	return(lakeName)
}

getMaxElevation	<-	function(nml){
	maxElevation	<-	nml$crest_elev
	return(maxElevation)
}

getMaxDepth	<-	function(nml){
	mxElv	<-	nml$crest_elev
	mnElv	<-	nml$base_elev
	maxDepth	<-	mxElv-mnElv
	return(maxDepth)
}

