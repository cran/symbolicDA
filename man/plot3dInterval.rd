\name{plot3dInterval}
\alias{plot3dInterval}
\title{3D plot for symbolic interval-valued data}
\description{3-dimensional plot for symbolic objects described by interval-valued variables}
\usage{plot3dInterval(data, colors)}
\arguments{
\item{data}{symbolic data table consists of a set of symbolic objects described by interval-valued variables}
\item{colors}{set of colors (see \code{\link{colors}}) to mark symbolic objects}
}
\value{
\item{}{3-dimensional plot for symbolic interval-valued data in which each axis represents a symbolic interval-valued variable and each cuboid represents a symbolic object}
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl}, Justyna Wilk \email{justyna.wilk@ue.wroc.pl}
Department of Econometrics and Computer Science, Wroclaw University of Economics, Poland \url{http://keii.ue.wroc.pl/symbolicDA}
}
\references{
Bock, H.H., Diday, E. (eds.) (2000), \emph{Analysis of symbolic data. Explanatory methods for extracting statistical information from complex data}, Springer-Verlag, Berlin.

Diday, E., Noirhomme-Fraiture, M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.
}
\seealso{
\code{\link{SClust}}; \code{plotInterval} in \code{clusterSim} library
}
\examples{
# LONG RUNNING - UNCOMMENT TO RUN
#means <- matrix(c(0,0,0,
#0,0,6,
#0,6,0,
#0,6,6,
#6,0,0,
#6,0,6,
#6,6,0,
#6,6,6),8,3,byrow=TRUE)
#means<-means*1.5
#means[5:8,1]<-means[5:8,1]-2
#means[5:8,3]<-means[5:8,3]-2
#cov <- matrix(c(1,0,0,0,1,0,0,0,1),3,3)
#t<-cluster.Gen(model=2, means=means, cov=cov, dataType="s", numObjects=10)
#plot3dInterval(t$data, colors=rainbow(8)[t$clusters])
#rgl.viewpoint(15,20,30)
#rgl.snapshot("8_clusters_3d.jpg")
}
\keyword{hplot}
