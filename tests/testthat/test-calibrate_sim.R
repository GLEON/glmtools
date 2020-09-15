test_that("calibrate_sim works", {
  # calibrate_sim is expensive to test
  skip_on_ci()
  skip_on_cran()
  
  calib_setup <- get_calib_setup()
  print(calib_setup)
  
  #Example calibration
  #Copy files into temporary directory
  sim_folder      <- tempdir() #simulation path
  glmtools_folder <- system.file('extdata', package = 'glmtools')
  
  file.copy(
    list.files(glmtools_folder,full.names = TRUE), 
    sim_folder, overwrite = TRUE)
  
  field_file  <- file.path(sim_folder, 'LakeMendota_field_data_hours.csv')
  nml_file    <- file.path(sim_folder, 'glm3.nml')
  driver_file <- file.path(sim_folder, 'LakeMendota_NLDAS.csv')
  period      <- get_calib_periods(nml_file = nml_file, ratio = 1)
  output      <- file.path(sim_folder, 'output/output.nc')
  
  var = 'temp' # variable to apply the calibration procedure
  
  res <- calibrate_sim(var = var, path = sim_folder, field_file = field_file,
                  nml_file = nml_file, calib_setup = calib_setup,
                  glmcmd = NULL,
                  first.attempt = TRUE, period = period, method = 'CMA-ES',
                  scaling = TRUE, #scaling should be TRUE for CMA-ES
                  verbose = FALSE,
                  metric = 'RMSE',plotting = FALSE,
                  target.fit = 1.5,
                  target.iter = 50, output = output)
  
  expect_gt(ncol(res), ncol(calib_setup))
  expect_type(res$calibrated, "double")
})
