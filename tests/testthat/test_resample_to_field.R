context("resample to field observations")

test_that("run glm simulation", {
  sim_folder <<- run_example_sim(verbose = F)
  nc_file <<- file.path(sim_folder, 'output/output.nc')
  field_file <<- file.path(sim_folder, 'LakeMendota_field_data_hours.csv')
  field_obs <<- read_field_obs(field_file, var_name = 'temp')
  expect_is(field_obs, 'data.frame')
})

test_that('test for non-overlapping simulation and observation dates', {
  
  field_obs = field_obs[2000,]
  write.csv(field_obs, file.path(sim_folder, 'LakeMendota_field_data_hours_no2015.csv'), row.names = F)
  
  # warning for "duplicate date values"
  expect_error(resample_to_field(nc_file, field_file = file.path(sim_folder, 'LakeMendota_field_data_hours_no2015.csv')))
  
})


test_that('testing for duplicate date warnings', {
  # duplicated field obs 
  field_obs[3,] = field_obs[4,]
  write.csv(field_obs, file.path(sim_folder, 'LakeMendota_field_data_hours_error.csv'), row.names = F)
  
  # warning for "duplicate date values"
  expect_error(resample_to_field(nc_file, field_file = file.path(sim_folder, 'LakeMendota_field_data_hours_error.csv')))
})

test_that('testing unsupported methods', {
  #unsupported method
  expect_error(resample_to_field(nc_file, field_file, precision = 'decades'))
})

