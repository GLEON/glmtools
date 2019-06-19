
<!-- README.md is generated from README.Rmd. Please edit that file -->
glmtools
========
[![status](https://img.shields.io/badge/USGS-Research-blue.svg)](https://owi.usgs.gov/R/packages.html#research)

[![Build status](https://ci.appveyor.com/api/projects/status/j5mscylmkssa0esf)](https://ci.appveyor.com/project/jread-usgs/glmtools) [![Build Status](https://travis-ci.org/USGS-R/glmtools.svg)](https://travis-ci.org/USGS-R/glmtools)  [![Coverage Status](https://img.shields.io/coveralls/USGS-R/glmtools.svg)](https://coveralls.io/r/USGS-R/glmtools)

Tools for interacting with the [General Lake Model (GLM)](http://aed.see.uwa.edu.au/research/models/GLM/ "General Lake Model's website") in R. `glmtools` includes some basic functions for calculating physical derivatives and thermal properties of model output, and some plotting functionality (see example image below).

`glmtools` Functions (as of v0.2.5.2)
=====================================

| Package  | Topic                    | Title                                                        |
|:---------|:-------------------------|:-------------------------------------------------------------|
| glmtools | compare\_to\_field       | compare metric for GLM vs field observations                 |
| glmtools | convert\_sim\_var        | convert an existing simulation variable into a different one |
| glmtools | epi.temperature          | Get volumetrically averaged epilimnion temp                  |
| glmtools | from.glm\_boolean        | go from glm2.nml logical vectors to R logicals               |
| glmtools | get\_evaporation         | get evaporation from GLM simulation                          |
| glmtools | get\_hypsography         | retrieve hypsography information                             |
| glmtools | get\_ice                 | get ice depth from GLM simulation                            |
| glmtools | get\_nml\_value          | gets a nml value according to an arg\_name                   |
| glmtools | get\_raw                 | get raw data from GLM simulation                             |
| glmtools | get\_surface\_height     | get surface height from GLM simulation                       |
| glmtools | get\_temp                | get water temperatures from a GLM simulation                 |
| glmtools | get\_var                 | get variable from a GLM simulation                           |
| glmtools | get\_wind                | get wind speed from GLM simulation                           |
| glmtools | hypo.temperature         | Get volumetrically averaged hypolimnion temp                 |
| glmtools | plot\_compare\_stage     | Plot simulated and observed stage data                       |
| glmtools | plot\_meteo              | Plot meterological drivers from a csv file                   |
| glmtools | plot\_temp               | Plot water temperatures from a GLM simulation                |
| glmtools | plot\_temp\_compare      | Plot matching heatmaps for modeled and observed temp         |
| glmtools | plot\_validate\_profiles | Plot validation and model temperature profiles               |
| glmtools | plot\_var                | Plot variables from a GLM simulation                         |
| glmtools | plot\_var\_compare       | Plot matching heatmaps for modeled and observed temp         |
| glmtools | read\_field\_obs         | Read in field data into a data.frame                         |
| glmtools | read\_field\_stage       | Read in a field stage file                                   |
| glmtools | read\_nml                | read in a GLM simulation \*.nml file                         |
| glmtools | resample\_sim            | get subset of time from a generic timeseries data.frame      |
| glmtools | resample\_to\_field      | Match GLM water temperatures with field observations         |
| glmtools | run\_example\_sim        | Run example simulation                                       |
| glmtools | set\_nml                 | sets values in nml object                                    |
| glmtools | sim\_metrics             | get possible metrics for comparing GLM outputs to field      |
| glmtools | sim\_var\_longname       | Get long name of variable from a GLM simulation              |
| glmtools | sim\_var\_units          | Get units of variable from a GLM simulation                  |
| glmtools | sim\_vars                | Get list of variables from a GLM simulation                  |
| glmtools | summarize\_sim           | Creates GLM simulation summary outputs                       |
| glmtools | validate\_sim            | Run diagnostics on model results vs observations             |
| glmtools | water.temperature        | Mimic rLakeAnalyzer function                                 |
| glmtools | whole.lake.temperature   | Get volumetrically averaged whole lake temperature           |
| glmtools | write\_nml               | write GLM .nml for a GLM simulation                          |

FAQ
===

How do I install `glmtools`? It isn't on CRAN!
==============================================

`glmtools` can be installed from the [USGS GRAN](http://owi.usgs.gov/R/gran.html) repository. To install `glmtools` and all dependencies, follow the appropriate instructions below:

1.  Setup R to add GRAN using the instructions on the [USGS R page](http://owi.usgs.gov/R/gran.html).

2.  After GRAN is setup (usually requires a restart of R), then use the following command in R:

    `install.packages("glmtools")`

What libraries does `glmtools` need?
====================================

This version requires the NetCDF version R library (called `ncdf4`), and `rLakeAnalyzer`, and `tools` if you would like to run all examples.

| package       | version     |
|:--------------|:------------|
| R             | &gt;= 3.0   |
| GLMr          | \*          |
| rLakeAnalyzer | &gt;= 1.8.3 |
| ncdf4         | \*          |
| tools         | \*          |
| akima         | \*          |
| lazyeval      | \*          |
| methods       | \*          |

How do I contribute new code back to the glmtools project?
==========================================================

In order to contribute to this code, we recommend the following workflow:

1.  "fork" this repository to your own personal github account

2.  clone the github repository to your computer:

    $git clone <https://github.com/%7Busername%7D/glmtools.git>

3.  modify code or add new functionality, save the code

4.  add the repository master to a remote master called "upstream"

    $cd glmtools

    $git remote add upstream <https://github.com/USGS-R/glmtools.git>

5.  before pushing your changes to your repository, pull in the current version of the GLEON master:

    $git fetch upstream

6.  merge these differences with your own "master" version:

    $git merge upstream/master

7.  push your changes to your github repository, in addition to changes made by pulling in the GLEON master:

    $git push

8.  submit a pull request to GLEON master using your account at github.com
