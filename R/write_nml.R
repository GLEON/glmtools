#'@title write GLM .nml for a GLM simulation
#'@description 
#'Creates a .nml file running a GLM simulation.  \cr
#'
#'
#'@param glm_nml a nml (a list) for GLM config
#'@param file a string with the path to the glm2.nml file to write
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{get_nml_value}, \link{read_nml}
#'@examples 
#'glm_nml <- read_nml()
#'write_path <- paste0(tempdir(),'glm2.nml')
#'write_nml(glm_nml, file = write_path)
#'print(read_nml(write_path))
#'@export
write_nml  <-	function(glm_nml,file){
  sink(file)
  
  print(glm_nml)
  sink()
}

