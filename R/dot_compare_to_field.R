#' @title Internal field data compare function. 
#' 
#' @description Used in \link{compare_to_field}. Accepts data.frame instead 
#' of nc and field file locations
#' 
#' @inheritParams compare_to_field
#' @param compare_data A data.frame with columns "DateTime, "Depth", "Obs" and "Mod", in that order.
#' @param bthA a numeric vector of cross sectional areas (m^2) corresponding to bthD depths
#' @param bthD a numeric vector of depths (m) which correspond to areal measures in bthA 
#'
#' @return See return options for \link{compare_to_field}
#' @keywords internal
#' @noRd
.compare_to_field = function(compare_data, bthA, bthD, metric, as_value = FALSE, na.rm = TRUE){
	
	as_mat = ifelse(output_dim(metric) > 1,TRUE, FALSE)
	
	un_dates <- unique(compare_data$DateTime)
	mod_metric <- vector('numeric', length = length(un_dates))
	obs_metric <- vector('numeric', length = length(un_dates))
	dates <- as.POSIXct(rep(NA, length = length(un_dates)))
	
	for (j in seq_len(length(un_dates))){
		date <- un_dates[j]
		u_i <- compare_data$DateTime == date
		depths <- compare_data$Depth[u_i]
		temp_obs <- compare_data[u_i, 3]
		temp_mod <- compare_data[u_i, 4]
		
		rmv_i <- is.na(temp_obs + temp_mod)
		mod_list <- list(wtr=temp_mod[!rmv_i], depths = depths[!rmv_i], bthA = bthA, bthD = bthD)
		obs_list <- list(wtr=temp_obs[!rmv_i], depths = depths[!rmv_i], bthA = bthA, bthD = bthD)
		use_names <- names(mod_list) %in% names(formals(metric)) # test to only use list elements that are inluded in the function args
		if (sum(rmv_i) == length(rmv_i)){
			mod_num <- NA
			obs_num <- NA
		} else {
			mod_num <- do.call(get(metric), mod_list[use_names]) 
			obs_num <- do.call(get(metric), obs_list[use_names]) 
		}
		
		
		if (as_mat == TRUE) { # ~!!! first date as single value will not be properly handled. !!!
			if (j == 1){
				# if as_mat, need a much larger matrix to support. Buffer and fill
				cnt = 1
				mod_metric <- vector(length = length(un_dates)*200)*NA
				obs_metric <- vector(length = length(un_dates)*200)*NA
				dates <- as.POSIXct(rep(NA, length = length(un_dates)*200))
			} 
			mod_metric[cnt:(cnt+length(mod_num)-1)] = mod_num
			obs_metric[cnt:(cnt+length(mod_num)-1)] = obs_num
			dates[cnt:(cnt+length(mod_num)-1)] <- rep(un_dates[j], length(mod_num))
			cnt = cnt+length(mod_num)
		} else {
			mod_metric[j] <- mod_num
			obs_metric[j] <- obs_num
			dates[j] <- un_dates[j]
		}
		
	}
	
	if (as_mat == TRUE) {
		dates <- dates[1:cnt-1]
		obs_metric <- obs_metric[1:cnt-1]
		mod_metric <- mod_metric[1:cnt-1]
	}
	if (as_value){
		compare.df <- data.frame('DateTime' = dates, 'obs' = obs_metric, 'mod' = mod_metric)
		if (na.rm){
			na_i <- is.na(compare.df[, 2]) | is.na(compare.df[, 3])
			compare.df <- compare.df[!na_i, ]
		}
		return(compare.df)
	} else {
		RMSE <- sqrt(mean((mod_metric-obs_metric)^2 , na.rm = na.rm))
		return(RMSE)
	}

}