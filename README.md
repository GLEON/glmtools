GLM-R
=====

Tools for interacting with the General Lake Model (GLM) in R. Includes some basic functions for calculating physical derivatives and thermal properties of model output.

In order to contribute to this code, we recommend the following workflow: 

1) "fork" this repository to your own personal github account

2) clone the github repository to your computer:

	$git clone https://github.com/username/GLM-R.git

3) modify code or add new functionality, save the code

4) add the repository master to a remote master called "upstream"

	$cd GLM-R

	$git remote add upstream https://github.com/GLEON/GLM-R.git

5) before pushing your changes to your repository, pull in the current version of the GLEON master:

	$git fetch upstream

6) merge these differences with your own "master" version:

	$git merge upstream/master

7) push your changes to your github repository, in addition to changes made by pulling in the GLEON master:

	$git push

8) submit a pull request to GLEON master using your account at github.com

FAQ
=====

What libraries do I need?
===

This version requires the NetCDF version 4 R library (called ncdf4).

How do I install ncdf4? 
===

On Mac or Linux: Simply type 

    install.packages('ncdf4')

On Windows: This is trickier. CRAN does not contain a Windows binary install for 
the ncdf4 library (as of right now, 2013-05-02). I have included a separately distributed version
in the "lib" subdirectory that works on the 32-bit version of Windows R (I think v2.15-ish). To install, 
type

    install.packages('%path%/lib/ncdf4_1.4.zip')

where %path% is the location of the GLM-r directory.