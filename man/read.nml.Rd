\name{read.nml}
\alias{read.nml}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{reads .nml file and builds R list}
\description{This function reads in a specified .nml file for GLM, and turns it into an R list. All data types are converted into logical, numerical, or character arrays}
\usage{
read.nml(fileName,folder))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fileName}{ a .nml file}
  \item{folder}{ location of .nml file}
}
\value{
list of nml parameters
}
\author{
Jordan S. Read
}

\examples{
	fileName<-	'glm.nml'
	folder	<-	'../Data/'
	nml	<-	read.nml(fileName,folder)
	print(nml)

}

\keyword{rGLM}
