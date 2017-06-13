#holder for glmtools metrics that follow rLA conventions
#'Mimic rLakeAnalyzer function
#'@details used to mimic an rLakeAnalyzer function, so that it will 
#'be recognized by a call to \code{\link{sim_metrics}}
#'@param wtr a water temperature data.frame
#'
#'@seealso \code{\link{sim_metrics}}, \code{\link{compare_to_field}}, \code{\link{validate_sim}}
#'@export
water.temperature <- function(wtr){
  wtr
}