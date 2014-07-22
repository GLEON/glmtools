#'@title prints R nml list with pretty formatting
#'@description This function prints the specified values from the nml list for GLM.
#'@param glm_nml a nml (a list) for GLM config
#'@author
#'Jordan S. Read
#'@examples
#'file = '../resources/glm.nml'
#'glm_nml <- read_nml(file)
#'pretty_nml(glm_nml)
#'@export
pretty_nml <- function(nml){
  for (i in 1:length(names(nml))){ # these are the blocks
    blckNm  <-	names(nml)[i]
    cat("&")
    cat(blckNm)
    cat('\n')
    blckList	<-	nml[[i]]
    for (j in 1:length(names(blckList))){
      cat('   ')
      cat(names(blckList)[j])
      cat(' = ')
      if (length(blckList[[j]])>1){
        writer	<-	paste(c(blckList[[j]]),collapse=', ')
      } else if (is.character(blckList[[j]])) {
        charText	<-	strsplit(blckList[[j]],',')
        writer	<-	paste(c("'",paste(c(charText[[1]]),collapse="','"),"'"),collapse='')
      } else if (is.logical(blckList[[j]]) & blckList[[j]]){
        writer	<-	".true."
      } else if (is.logical(blckList[[j]]) & !blckList[[j]]){
        writer	<-	".false."
      } else {
        writer	<-	blckList[[j]]
      }
      cat(writer)
      cat('\n')
    }
    cat('/\n')
  }	
}