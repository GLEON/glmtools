#'@export
print.nml <- function(x, ...){
  glm_nml <- x
  for (i in seq_len(length(names(glm_nml)))){ # these are the blocks
    blckNm  <-	names(glm_nml)[i]
    cat("&")
    cat(blckNm)
    cat('\n')
    blckList	<-	glm_nml[[i]]
    for (j in seq_len(length(names(blckList)))){
      cat('   ')
      cat(names(blckList)[j])
      cat(' = ')
      if (length(blckList[[j]])>1){
        if (is.logical(blckList[[j]])){
          charText <- to.glm_boolean(blckList[[j]])
        } else {
          charText <- c(blckList[[j]])
        }
        writer	<-	paste(charText,collapse=', ')
      } else if (is.character(blckList[[j]])) {
        charText <- strsplit(blckList[[j]],',')
        writer <- paste(c("'",paste(c(charText[[1]]),collapse="','"),"'"),collapse='')
      } else if (is.logical(blckList[[j]])){
        writer <- to.glm_boolean(blckList[[j]])
      } else {
        writer <- blckList[[j]]
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