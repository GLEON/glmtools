#'@title run GLM on a mac
#'@param sim_folder the directory where simulation files are contained
#'@param verbose should output of GLM be shown
#'@keywords methods
#'@seealso \link{compare_to_field}, \link{resample_to_field}, \link{read_nml}, \link{get_metrics}
#'@author
#'Jordan S. Read
#'@examples 
#'\dontrun{
#'sim_folder <- system.file('extdata/sim/', package = 'glmtools')
#'run_glmOSx(sim_folder)
#'out_file <- file.path(sim_folder,'output.nc')
#'nml_file <- system.file('extdata/sim', 'glm.nml', package = 'glmtools')
#'field_file <- system.file('extdata', 'field_data.tsv', package = 'glmtools')
#'plot_temp(file = out_file, fig_path = 'test_temp.png')
#'model_diagnostics(out_file, field_file, nml_file = nml_file,
#'    metrics = c('center.buoyancy','thermo.depth', 'schmidt.stability'),
#'    fig_path = 'test_diagnostic.png')
#' }
#'@export
run_glmOSx <- function(sim_folder, verbose = TRUE){
  glm_path <- system.file('extdata/sim/glm', package = 'glmtools')
  origin <- getwd()
  setwd(sim_folder)

  tryCatch({
    if (verbose){
      out <- system2(glm_path, wait = TRUE, stdout = stdout, stderr = stderr)
    } else {
      out <- system2(glm_path, wait = TRUE, stdout = NULL, stderr = NULL)
    }
    setwd(origin)
  }, error = function(err) {
    print(paste("GLM_ERROR:  ",err))
    setwd(origin)
  })
}