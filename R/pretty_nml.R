#'@title prints R nml list with pretty formatting
#'@description This function prints the specified values from the nml list for GLM.
#'@param glm_nml a nml (a list) for GLM config
#'@author
#'Jordan S. Read
#'@examples
#'nml_file <- system.file('extdata', 'glm.nml', package = 'glmtools')
#'glm_nml <- read_nml(nml_file)
#'pretty_nml(glm_nml)
#'@export
pretty_nml <- function(glm_nml){
  for (i in 1:length(names(glm_nml))){ # these are the blocks
    blckNm  <-	names(glm_nml)[i]
    cat("&")
    cat(blckNm)
    cat('\n')
    blckList	<-	glm_nml[[i]]
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