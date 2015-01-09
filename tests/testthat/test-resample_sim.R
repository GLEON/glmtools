context("resample simulation")

test_that("running glm simulation", {
  sim_folder <- run_example_sim(verbose = F)
  nc_file <- file.path(sim_folder, 'output.nc')
  temp_surf <- get_temp(nc_file, reference = 'surface', z_out = c(0,1,2))
  t_out <- as.POSIXct(c("1900-01-01"))
  # date won't be found
  expect_warning(resample_sim(df = temp_surf, t_out = t_out))
  
  # two on the same day w/ precision = 'days'
  t_out <- c("2011-04-01 10:00", "2011-04-05 08:15", 
         "2011-06-14 10:30", "2011-04-05 10:21", 
         "2011-07-28 10:00")
  # warning for "'days' precision resulted in duplicate date values"
  expect_warning(resample_sim(df = temp_surf, t_out = t_out, precision = 'days'))
  
  #unsupported method
  expect_error(resample_sim(df = temp_surf, t_out = t_out, method = 'new_method'))

  
  t_out <- as.POSIXct(c("2011-04-01 10:00", "2011-04-05 08:15", 
             "2011-06-14 10:30", "2011-04-05 10:21", 
             "2011-07-28 10:00"),tz = 'GMT') 
  df <- resample_sim(df = temp_surf, t_out = t_out, method = 'interp', precision = 'exact')
  expect_true(all(t_out == df[,1]))
  
  df_min <- resample_sim(df = temp_surf, t_out = t_out, method = 'interp', precision = 'mins')
  
  expect_true(all(df_min[,1] == df[,1]))
  
  t_out <- as.POSIXct(c("2011-04-01 10:00:00", "2011-04-05 08:15:00", 
                        "2011-06-14 10:30:00", "2011-04-05 08:15:14", 
                        "2011-07-28 10:00:00"),tz = 'GMT') 
  #no error
  df <- resample_sim(df = temp_surf, t_out = t_out, method = 'interp', precision = 'exact')
  # error
  expect_warning(df_min <- resample_sim(df = temp_surf, t_out = t_out, method = 'interp', precision = 'mins'))
  expect_less_than(length(unique(df_min[,1])),length(unique(df[,1])))
})

