
#'@title gets a nml value according to an arg_name
#'@description This function returns an nml value according to the arg_name nml list for GLM.
#'@param glm_nml a nml (a list) for GLM config
#'@param arg_name a string representing a valid field in glm_nml
#'@param \dots additional arguments passed to \code{get_block}, such as warn=TRUE
#'@param nml_file a string with the path to the GLM glm2.nml file or 
#'\code{'template'} for loading the GLM template nml file with GLM3r (default)
#'@return arg_val value for the valid field in glm_nml specified by \code{arg_name}
#'@author
#'Jordan S. Read
#'@seealso \link{read_nml}, \link{set_nml}
#'@examples
#'# read in default nml template from GLM3r
#'glm_nml <- read_nml()
#'get_nml_value(glm_nml,arg_name = 'Kw')
#'@export
get_nml_value  <-	function(glm_nml = NA, arg_name, nml_file = 'template', ...){
  
  if(!all(is.na(glm_nml)) & nml_file != 'template'){
    stop("Must specify either an nml object via 'glm_nml' or 
         an nml file path via 'nml_file'")
  }
  
  if(all(is.na(glm_nml))){
    glm_nml <- read_nml(nml_file)
  }
  
  blck = get_block(glm_nml, arg_name, ...)
  arg_name = get_arg_name(arg_name)
  return(glm_nml[[blck]][[arg_name]])
}
