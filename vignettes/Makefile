all: calibration_walkthrough.html

calibration_walkthrough.html: calibration_walkthrough.Rmd
	Rscript -e "rmarkdown::render('$<')"
