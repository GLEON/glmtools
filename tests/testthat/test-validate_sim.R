test_that("validate_sim works", {
  sim_folder <- run_example_sim(verbose = FALSE)
  nc_file    <- file.path(sim_folder, 'output/output.nc')
  nml_file   <- file.path(sim_folder, 'glm3.nml')
  field_file <- file.path(sim_folder, 'LakeMendota_field_data_hours.csv')

  res <- validate_sim(nc_file, field_file, nml_file = nml_file, report = TRUE,
                            metrics = c('thermo.depth', 'schmidt.stability'))
  
  expect_type(res, "list")
  expect_length(res, 2)
})
