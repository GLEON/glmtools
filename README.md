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