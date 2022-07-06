\name{elli.f}
\alias{elli.f}

\title{
	Dimensionality reduction methods in metagenomic analysis.
}
\description{
	A collection of suitable dimensionality reduction techniques from R packages in metagenomic analysis.
}
\usage{
    elli.f(x,y)
}
\arguments{
  \item{x}{
    a vector or a N x 2 matrix or data frame (rows: N points; columns: x coordinate value,y coordinate value) with one header c('x','y').
  }
  \item{y}{
    a vector. if is NULL, x must be a N x 2 matrix or data frame. 
  }  
}
\details{
  This function returns six parameter values "c(A,B,C,D,E,F)" of the general equation of the ellipse. The general equation of the ellipse is "Ax^2+Bxy+Cy^2+Dx+Ey+F=0".
}
\author{
%%  ~~who you are~~
Wen Luo(design, implementation, testing)
}

\examples{
data(PRJEB17643.pcoaData)
test = PRJEB17643.pcoaData[PRJEB17643.pcoaData$HostID=='Subj1',c('x','y')]
f = elli.f(test)
f
}
