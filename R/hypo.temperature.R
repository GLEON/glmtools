#'@title Get volumetrically averaged hypolimnion temp
#'@details A metric function used like an rLakeAnalyzer function to be 
#'called by \code{\link{sim_metrics}}
#'@param wtr a water temperature vector
#'
#'@import rLakeAnalyzer
#'
#'@seealso \code{\link{sim_metrics}}, \code{\link{compare_to_field}}, \code{\link{validate_sim}}
#'@export
hypo.temperature <- function(wtr, depths, bthA, bthD){
	
	md = rLakeAnalyzer::meta.depths(wtr, depths)
	
	if(is.na(md[1])){
		avg_temp = NA
	}else{
		avg_temp = layer.temperature(md[2],max(depths), wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
	}
	return(avg_temp)
}
