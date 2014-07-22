#'@title sets values in nml object
#'@description This function sets values in nml object for GLM config.
#'@param glm_nml a nml (a list) for GLM config
#'@param arg_name a string representing a valid field in glm_nml
#'@param arg_val value for the valid field in glm_nml specified by \code{arg_name}
#'@param arg_list, a list made up of valid \code{arg_name}s and \code{arg_val}s
#'@return glm_nml a modified nml
#'@author
#'Jordan S. Read
#'@export
set_nml  <-	function(glm_nml,arg_name,arg_val,arg_list=NULL){
  
  if (missing(argName)){
    return(setnmlList(glm_nml,arg_list))
  }
  
  if (!is.null(arg_list) & argName %in% names(arg_list)){
    warning(c("duplicate names given to argName and arg_list.", 
              " argName and arg_val values will overwrite duplicate arg_list values."))
    glm_nml <- setnmlList(glm_nml,arg_list)
  }
  
  # get appropriate block to place val within ** assumes no duplicate param names in other blocks **
  blckI	<-	findBlck(glm_nml,argName)
  
  currVal	<-	glm_nml[[blckI]][[argName]]
  typeError	<-	"input must be of same data type as current value"
  if (is.logical(currVal) & !is.logical(arg_val)){
    stop(c(typeError,' (logical)'))
  } else if (is.character(currVal) & !is.character(arg_val)){
    stop(c(typeError,' (character)'))
  } else if (is.numeric(currVal) & !is.numeric(arg_val)){
    stop(c(typeError,' (numeric)'))
  }
  
  glm_nml[[blckI]][[argName]]	<- arg_val
  return(glm_nml)
}