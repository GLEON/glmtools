
#'@title gets a nml value according to an arg_name
#'@description This function returns an nml value according to the arg_name nml list for GLM.
#'@param glm_nml a nml (a list) for GLM config
#'@param arg_name a string representing a valid field in glm_nml
#'@return arg_val value for the valid field in glm_nml specified by \code{arg_name}
#'@author
#'Jordan S. Read
#'@seealso \link{read_nml}, \link{set_nml}
#'@examples
#'nml_file <- system.file('extdata', 'glm.nml', package = 'glmtools')
#'glm_nml <- read_nml(nml_file)
#'get_nml_value(glm_nml,arg_name = 'Kw')
#'@export
get_nml_value  <-	function(glm_nml, arg_name){
  
  blckI	<-	findBlck(glm_nml,arg_name)
  
  arg_val	<-	glm_nml[[blckI]][[arg_name]]
  return(arg_val)
}
