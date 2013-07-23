# ------Helper functions for interacting with glm.nml files-----
# ------Jordan and Luke 2013
commentStr	<-	'!'

getNML	<-	function(fileName='glm.nml',folder='../Data/'){
	# skip all commented lines, return all variables and associated values
	# requires NO return line variables (all variables must be completely defined on a single line)
	fileN	<-	paste(c(folder,fileName),collapse="")
	c <- file(fileN,"r") 
	fileLines <- readLines(c)
	close(c)
	lineStart	<-	substr(fileLines,1,1)
	ignoreLn	<-	lineStart==commentStr | fileLines==""
	lineStart	<-	lineStart[!ignoreLn]
	fileLines	<-	fileLines[!ignoreLn]
	# find all lines which start with "&" * requires FIRST char to be value
	
	
	lineIdx		<- seq(1,length(lineStart))
	blckOpen	<-	lineIdx[lineStart=="&"]
	blckClse	<-	lineIdx[lineStart=="/"]
	
	
	nml <- list()
	for (i in 1:length(blckOpen)){
		blckName	<-	substr(fileLines[blckOpen[i]],2,nchar(fileLines[blckOpen[i]]))
		oldNms	<-	names(nml)
		nml[[i]]	<-	list()
		names(nml)	<-	c(oldNms,blckName)
		for (j in (blckOpen[i]+1):(blckClse[i]-1)){
			textLine	<-	gsub("\t","",gsub(" ","",fileLines[j]))
			if(substr(textLine,1,1)!=commentStr){ 
				# else, line is commented out
				lineVal	<-	buildVal(textLine)
				nml[[i]]	<-	c(nml[[i]],lineVal)
				}
		}
	}
	return(nml)
}
buildVal	<-	function(textLine){
	#-----function appends nml list with new values-----
	# remove all text after comment string
	textLine	<-	strsplit(textLine,commentStr)[[1]][1]
	
	if (!any(grep("=",textLine))){stop(c("no hanging lines allowed in .nml, used ",textLine))}
	params	<-	strsplit(textLine,"=") # break text at "="
	parNm	<-	params[[1]][1]
	parVl	<-	params[[1]][2]
	# figure out what parval is...if string, remove quotes and keep as string
	# ***for boolean text, use "indentical" so that 0!= FALSE
	# can be: string, number, comma-sep-numbers, or boolean
	
	# special case for date:
	if (nchar(parVl>17) & substr(parVl,14,14)==':' & substr(parVl,17,17)==':'){
		parVl<-paste(c(substr(parVl,1,11),' ',substr(parVl,12,nchar(parVl))),collapse='')
		}
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
	lineVal	<-	list(parVl)
	names(lineVal)	<-	parNm
	return(lineVal)
}

setNML	<-	function(nml,argName,argVal){
	nml[argName]	<-	argVal
	return(nml)
}

writeNML	<-	function(nml,fileName='glm.nml',folder='../Data/'){
	sink(paste(c(folder,fileName),collapse=''))
	for (i in 1:length(names(nml))){ # these are the blocks
		blckNm	<-	names(nml)[i]
		cat("&")
		cat(blckNm)
		cat('\n')
		blckList	<-	nml[[i]]
		for (j in 1:length(names(blckList))){
			cat('   ')
			cat(names(blckList)[j])
			cat(' = ')
			if (length(blckList[[j]]>1)){
				writer	<-	paste(blckList[[j]],colapse=',')
			} else {writer	<-	blckList[[j]]}
			cat(writer)
			cat('\n')
		}
		cat('/\n')
	}	
	sink()
}

