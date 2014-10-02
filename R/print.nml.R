#'@export
print.nml <- function(x, ...){
  glm_nml <- x
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
#'@export
summary.nml <- function(object,...){
  print(object,...)
}