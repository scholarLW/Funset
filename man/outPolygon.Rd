\name{outPolygon}
\alias{outPolygon}
\alias{outpolygon}
\title{
Point Set of Convex Polygon 
}
\description{
  Filter out the set of points that can generate a peripheral convex polygon from a specified set of points.
}
\usage{
  outPolygon(da,deviation=10)
}

\arguments{
  \item{da}{
    a N x 2 matrix or data frame (rows: N points; columns: x coordinate value,y coordinate value) with one header c('x','y').
  }
  \item{deviation}{
   Angle difference, used to evaluate error terms for three-points collinear.
}
}

\details{
  
}

\author{
%%  ~~who you are~~
Wen Luo(design, implementation, testing)
}

\examples{

}