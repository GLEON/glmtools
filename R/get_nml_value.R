
#'@title gets a nml value according to an arg_name
#'@description This function returns an nml value according to the arg_name nml list for GLM.
#'@param glm_nml a nml (a list) for GLM config
#'@param arg_name a string representing a valid field in glm_nml
#' @param \dots additional arguments passed to \code{get_block}, such as warn=TRUE
#'@return arg_val value for the valid field in glm_nml specified by \code{arg_name}
#'@author
#'Jordan S. Read
#'@seealso \link{read_nml}, \link{set_nml}
#'@import GLMr
#'@examples
#'# read in default nml template from GLMr
#'glm_nml <- read_nml()
#'get_nml_value(glm_nml,arg_name = 'Kw')
#'@export
get_nml_value  <-	function(glm_nml, arg_name, ...){
  
  
  blck = get_block(glm_nml, arg_name, ...)
  arg_name = 'get_arg_name(arg_name)'
  return(glm_nml[[blck]][[arg_name]])
}
