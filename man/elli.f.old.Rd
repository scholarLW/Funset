\name{elli.f.old}
\alias{elli.f.old}

\title{
	Dimensionality reduction methods in metagenomic analysis.
}
\description{
	A collection of suitable dimensionality reduction techniques from R packages in metagenomic analysis.
}
\usage{
    elli.f.old(x,y,perc=0.2,ang=60)
}
\arguments{
  \item{x}{
    a vector or a N x 2 matrix or data frame (rows: N points; columns: x coordinate value,y coordinate value) with one header c('x','y').
  }
  \item{y}{
    a vector. if is NULL, x must be a N x 2 matrix or data frame. 
  }  
  \item{perc}{
	A specified distance, used to capture a set of points that meet certain conditions, such as a symmetrical point about a straight line. 
}
  \item{ang}{
	A specified angle, used to capture a set of points that meet certain conditions, such as a symmetrical point about a straight line.
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
f = elli.f.old(test,perc=0.2,ang=60)
f
}
