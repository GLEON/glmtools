#'@title write GLM .nml for a GLM simulation
#'@description 
#'Creates a .nml file running a GLM simulation.  \cr
#'
#'
#'@param glm_nml a nml (a list) for GLM config
#'@param file a string with the path to the glm.nml file to write
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{get_nml_value}, \link{read_nml}
#'@examples 
#'glm_nml <- read_nml()
#'write_nml(glm_nml, file = '../glm2_test.nml')
#'@export
write_nml  <-	function(glm_nml,file){
  sink(file)
  
  print(glm_nml)
  sink()
}

