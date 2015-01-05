

# private function
buildVal	<-	function(textLine){
	#-----function appends nml list with new values-----
	# remove all text after comment string
	textLine	<-	strsplit(textLine,'!')[[1]][1]
  
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
	}else if (any(grep("\"",parVl))){
	  parVl  <-	gsub("\"","",parVl)
	}else if (any(grep("”",parVl))){
	  parVl  <-	gsub("”","",parVl)
	  parVl  <- gsub("“","",parVl)
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

# private function
findBlck	<-	function(nml,argName){
	
  # test for argName being a string
  if (!is.character(argName)){stop(c("parameter name must be a string"))}
  fau <- " "
  fault.string <- rep(fau,100) # names fault matrix, only returned when empty match
	blockNames	<-	names(nml)
	blckI	<-	NULL
	for (i in 1:length(blockNames)){
		if (any(argName %in% names(nml[[i]]))){
			blckI	<- i
			break
		} else {
      one.i <- which(fault.string==fau)[1]
		  fault.string[one.i:(one.i+length(names(nml[[i]]))-1)]=names(nml[[i]])
		}
    
	}
  fault.string <- fault.string[!fault.string==fau] # is empty if found
  # test to see if a block match was made
	if (is.null(blckI)){stop(c("parameter name ",argName," not found in nml. Possible names:",paste(fault.string,collapse=', ')))}
	return(blckI)
}

# private function
setnmlList <- function(glm_nml,arg_list){
  if (!is.list(arg_list)){stop("argList must be a list")}
  
  arg_names  <-	names(arg_list)
  for (i in 1:length(arg_names)){
    glm_nml <- set_nml(glm_nml,arg_name=arg_names[i],arg_val=arg_list[[i]])
  }
  return(glm_nml)
}

# private function
is_nml_file <- function(nml_file){
  
  is_nml <- FALSE
  fl_ext <- tail(strsplit(nml_file, "\\.")[[1]],1)
  
  if (fl_ext == 'nml'){
    is_nml <- TRUE
  }
  return(is_nml)
}

