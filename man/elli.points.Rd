\name{elli.points}
\alias{elli.points}

\title{
	Elliptic Curve Segmentation.
}
\description{
	A function that splits the elliptic curve into fragments.
}
\usage{
    elli.points(f,segments=1000)
}
\arguments{
  \item{f}{
	A vectors of six parameter value of the ellipse general equation.
}
  \item{segments}{
	Number of fragmentation. 
}

}
\details{

}
\author{
%%  ~~who you are~~
Wen Luo(design, implementation, testing)
}

\examples{
data(PRJEB17643.pcoaData)
test = PRJEB17643.pcoaData[PRJEB17643.pcoaData$HostID=='Subj1',c('x','y')]
f = elli.f(test,perc=0.2,ang=60)
ellipse = elli.points(f,segments=1000)
ellipse[1:10,]
xmin = min(ellipse[,1])
xmax = max(ellipse[,1])
ymin = min(ellipse[,2])
ymax = max(ellipse[,2])
plot(0,0,type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
lines(ellipse, col='red')
}
