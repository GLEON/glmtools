#'@title read in a GLM simulation *.nml file
#'@description 
#'read in a GLM simulation *.nml file and create a list.  \cr
#'
#'
#'@param file a string with the path to the GLM glm.nml file
#'@return glm_nml a nml (a list) for GLM config
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{get_nml_value}
#'@examples 
#'file = '../resources/glm.nml'
#'glm_nml <- read_nml(file)
#'pretty_nml(glm_nml)
#'@export
read_nml  <-	function(file = '../resources/glm.nml'){
  # skip all commented lines, return all variables and associated values
  # requires NO return line variables (all variables must be completely defined on a single line)
  c <- file(file,"r") 
  fileLines <- readLines(c)
  close(c)
  lineStart	<-	substr(fileLines,1,1)
  ignoreLn	<-	lineStart=='!' | fileLines==""
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
    
    carryover = ''
    
    for (j in (blckOpen[i]+1):(blckClse[i]-1)){
      
      textLine	<-	paste(carryover, gsub("\t","",gsub(" ","",fileLines[j])), sep='')
      #cat(textLine,'\n')
      #browser()
      if(substr(textLine,1,1)!='!'){
        # Add a check here, sometimes, if there is a hanging comma, 
        #and only sumtimes that means add next row
        if(substr(textLine,nchar(textLine), nchar(textLine)) == ',' && 
             j+1 <= length(fileLines) && !any(grep("=",fileLines[j+1])) && !any(grep("/",fileLines[j+1]))){
          
          carryover = textLine
          next
        }else{
          carryover = ''
        }
        # else, line is commented out
        lineVal	<-	buildVal(textLine)
        nml[[i]]	<-	c(nml[[i]],lineVal)
      }
    }
  }
  return(nml)
}