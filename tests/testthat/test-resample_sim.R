context("resample simulation")

test_that("running glm simulation", {
  sim_folder <- run_example_sim(verbose = F)
  nc_file <- file.path(sim_folder, 'output/output.nc')
  temp_surf <- get_temp(nc_file, reference = 'surface', z_out = c(0,1,2))
})

context('resample_sim testing for empty returns and warnings')
test_that('testing for empty returns and warnings', {
  t_out <- as.POSIXct(c("1900-01-01", "1900-01-02"))
  
  # date won't be found
  expect_warning(df <- resample_sim(df = temp_surf, t_out = t_out))
  expect_warning(df2 <- resample_sim(df = temp_surf, t_out = '2010-05-01 08:15', method = 'match', precision = 'hours'))
  
  # dates won't be found. No extrapolation
  expect_warning(resample_sim(df = temp_surf, t_out = t_out, method = 'interp'))
  
  expect_equal(df,df2)
  
})
  

context('resample_sim testing duplicate dates')
test_that('testing for duplicate date warnings', {
  # two on the same day w/ precision = 'days'
  t_out <- c("2015-04-16 10:00", "2015-04-16 10:00", 
         "2015-06-14 10:30", "2015-04-05 10:21", 
         "2015-07-28 10:00")
  # warning for "'days' precision resulted in duplicate date values"
  expect_error(resample_sim(df = temp_surf, t_out = t_out, precision = 'days'))
})

context('resample_sim testing unsupported methods')
test_that('testing unsupported methods', {
  t_out <- as.POSIXct(c("1900-01-01"))
  #unsupported method
  expect_error(resample_sim(df = temp_surf, t_out = t_out, method = 'new_method'))
  expect_error(resample_sim(df = temp_surf, t_out = t_out, precision = 'decades'))
  expect_error(resample_sim(df = temp_surf, t_out = t_out, precision = 'hour'))
})

context('resample_sim testing interpolation')
test_that('testing interpolation', { 
  t_out <- as.POSIXct(c("2015-05-05 08:15", 
             "2015-06-14 10:30", "2015-04-16 10:21", 
             "2015-07-28 10:00"),tz = 'GMT') 
  df <- resample_sim(df = temp_surf, t_out = t_out, method = 'interp', precision = 'exact')
  expect_true(all(t_out == df[,1]))
  
  df_min <- resample_sim(df = temp_surf, t_out = t_out, method = 'interp', precision = 'mins')
  expect_true(all(df_min[,1] == df[,1]))

  t_out <- as.POSIXct(c("2015-05-05 08:15:00", 
  											"2015-06-14 10:30:00", "2015-05-05 08:15:14", 
  											"2015-07-28 10:00:00"),tz = 'GMT') 
  
  #no error
  df <- resample_sim(df = temp_surf, t_out = t_out, method = 'interp', precision = 'exact')
  
  # Should throw duplicate value warning with minute precision (2010-05-05 8:15 appears twice)
  expect_warning(df_min <- resample_sim(df = temp_surf, t_out = t_out, method = 'interp', precision = 'mins'))
  expect_lt(length(unique(df_min[,1])),length(unique(df[,1])))
})

