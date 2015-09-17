context("aed nmls can be parsed")

test_that("normal aed parse works",{
  
  aed_nml <<- read_nml(system.file('extdata','fabm.nml', package='glmtools'))
  
  expect_equal(get_nml_value(aed_nml, 'num_tracers'), 1)
})

test_that("aed block::arg_name works",{
  
  expect_equal(get_nml_value(aed_nml, 'aed_oxygen::Fsed_oxy'), -50)
  expect_equal(get_nml_value(aed_nml, 'aed_sed_constant::Fsed_oxy'), -3)
  
  
})

test_that("aed parse warns w/ more than one match",{
  expect_warning(get_nml_value(aed_nml, 'Fsed_oxy'))
})


context("aed nmls can be set")

test_that("aed set warns w/ more than one match",{
  expect_warning(set_nml(aed_nml, 'Fsed_oxy', arg_val = 1.4))
})

test_that("setter works with default case",{
  nml = set_nml(aed_nml, 'Fsed_oxy', arg_val = 1.4)
  expect_equal(get_nml_value(nml, 'Fsed_oxy'), 1.4)
})

test_that("setter works with new syntax",{
  expect_false(get_nml_value(aed_nml, 'aed_sed_constant::Fsed_oxy') == 1.5)
  nml = set_nml(aed_nml, 'aed_sed_constant::Fsed_oxy', arg_val = 1.5)
  expect_equal(get_nml_value(nml, 'aed_sed_constant::Fsed_oxy'), 1.5)
  #the default
  expect_false(get_nml_value(nml, 'aed_oxygen::Fsed_oxy')== 1.5)
})