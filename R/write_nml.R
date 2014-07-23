#'@title write GLM *.nml for a GLM simulation
#'@description 
#'Creates a *.nml file running a GLM simulation.  \cr
#'
#'
#'@param glm_nml a nml (a list) for GLM config
#'@param file a string with the path to the glm.nml file to write
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{get_nml_value}, \link{read_nml}, \link{pretty_nml}
#'@examples 
#'file = '../resources/glm_test.nml'
#'glm_nml <- read_nml('../resources/glm.nml')
#'write_nml(glm_nml, file= file)
#'@export
write_nml  <-	function(glm_nml,file='../resources/glm_test.nml'){
  sink(file)
  
  pretty_nml(glm_nml)
  sink()
}

