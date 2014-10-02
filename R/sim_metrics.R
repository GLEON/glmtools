#'@title get possible metrics for comparing GLM outputs to field
#'@description
#'returns a character array of metric names that can be supported by \code{compare_to_field}. 
#'This function is only designed to handle calls to physical metrics that return a single value.
#'An example of this behavior is thermocline depth (but not water density for all depth-resolved measurements).
#'@param with_nml include metrics that require information in the nml file
#'@return a character array of metrics that can be used
#'@keywords methods
#'@seealso \link{compare_to_field}, \link{resample_to_field}, \link{read_nml}
#'@author
#'Jordan S. Read
#'@examples 
#'sim_metrics()
#'sim_metrics(with_nml = TRUE)
#'@import rLakeAnalyzer
#'@export
sim_metrics <- function(with_nml = FALSE){
  library(rLakeAnalyzer)
  package_name <- 'rLakeAnalyzer'
  funs <- fun_from_package(package_name)
  
  arg_names <- c('wtr', 'depths')
  metrics <- match_inputs(funs, arg_names)
  
  if (with_nml){
    arg_names = c('wtr', 'depths', 'bthA', 'bthD')
    metrics <- c(metrics, match_inputs(funs, arg_names))
  }
  
  
  return(metrics)
}

fun_from_package <- function(package_name, private = FALSE){
  
  if (private){
    funs <- as.character(unclass(lsf.str(envir = asNamespace(package_name), all = T)))
  } else {
    library(rLakeAnalyzer)
    funs <- ls(paste0('package:', package_name)) # get public functions
  }
  
  return(funs)
}

match_inputs <- function(funs, arg_names){
  
  ignore_args = c('ts', 'plot')
  
  fun_valid <- vector(length = length(funs))
  for (i in 1:length(funs)){
    all_args <- names(formals(funs[i]))
    def_args <- names(Filter(function(x) !identical(x, quote(expr = )), formals(funs[i])))
    req_args <- all_args[!all_args %in% def_args]
    fun_valid[i] = all(req_args %in% arg_names)
  }
  
  valid_funs <- funs[fun_valid]
  rmv_i <- vector(length=length(valid_funs))
  for (i in 1:length(ignore_args)){
    rmv_i <- rmv_i | grepl(ignore_args[i],valid_funs)
  }
  
  
  return(valid_funs[!rmv_i])

}