#'@title write GLM *.nml for a GLM simulation
#'@description 
#'Creates a *.nml file running a GLM simulation.  \cr
#'An existing glm.nml will be overwritten if it shares the same 
#'path as \code{file_out}
#'
#'@param glm_nml a nml (a list) for GLM config
#'@param file_out a string with the path to the glm.nml file to write
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{get_nml_value}, \link{read_nml}, \link{pretty_nml}
#'@examples 
#'nml_file <- system.file('extdata', 'glm.nml', package = 'rGLM')
#'glm_nml <- read_nml(nml_file)
#'file_out <- system.file('extdata', 'glm_test.nml', package = 'rGLM')
#'write_nml(glm_nml, file_out = file_out)
#'@export
write_nml  <-	function(glm_nml, file_out){
  sink(file_out)
  
  pretty_nml(glm_nml)
  sink()
}

