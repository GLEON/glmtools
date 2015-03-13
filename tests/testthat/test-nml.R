context("testing read_nml")

test_that("reading nml file from template", {
    nml <<- read_nml()
    expect_is(nml, 'nml')
})

context("testing set_nml")
test_that("set_nml() with different datatypes", {
    # check logicals
    expect_error(set_nml(nml, arg_name = 'met_sw', arg_val = 'yes'))
    expect_error(set_nml(nml, arg_name = 'met_sw', arg_val = 1))
    expect_error(set_nml(nml, arg_name = 'met_sw', arg_val = c(1,2,3)))
    
})

context("testing validate nml")
test_that(".validate nml works",{
  nml_bad <- nml
  nml_bad$time <- NULL # remove a required block
  expect_error(glmtools:::.validate_nml(nml_bad))
  expect_true(glmtools:::.validate_nml(nml))
  
  
})

context("reading a bad nml file")
test_that("file errors out",{
  expect_error(read_nml(system.file('extdata','bad_glm2.nml',package='glmtools')))
})

context("set_nml() and get_nml_value() with lists and arguments")
test_that("reading and setting nml works as expected", {
    # set string to a string
    nml <- set_nml(nml, arg_name = 'start', arg_val = 'yes')

    expect_match(get_nml_value(nml, 'start'), 'yes')
    
    # use arg_list
    t_val <- 'no'
    nml <- set_nml(nml, arg_list = list('start'= t_val, 'stop'='2000-08-01 00:00:00'))
    expect_match(get_nml_value(nml, 'start'), t_val)

    expect_error(set_nml(nml, arg_list = list('start','yes',TRUE)))
    
    # use arg_list and arg_name (conflict)
    expect_error(set_nml(nml, arg_name = 'start',arg_name = 'yes', arg_list = list('start'='no')))
    # pass list to arg_name (should be char)
    expect_error(set_nml(nml, arg_name = list('start'='no')))
})