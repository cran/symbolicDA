\name{index.G1d}
\alias{index.G1d}
\title{Calinski-Harabasz pseudo F-statistic based on distance matrix}
\description{Calculates Calinski-Harabasz pseudo F-statistic based on distance matrix}
\usage{index.G1d (d,cl)}
\arguments{
\item{d}{distance matrix (see \code{\link{dist.SDA}})}
\item{cl}{a vector of integers indicating the cluster to which each object is allocated}
}
\value{
value of Calinski-Harabasz pseudo F-statistic based on distance matrix
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl}, Justyna Wilk \email{justyna.wilk@ue.wroc.pl}
Department of Econometrics and Computer Science, Wroclaw University of Economics, Poland \url{http://keii.ue.wroc.pl/symbolicDA}
}
\references{
Calinski, R.B., Harabasz, J. (1974), \emph{A Dendrite Method for Cluster Analysis}, "Communications in Statistics", Vol. 3, pp. 1-27.

Everitt, B.S., Landau, E., Leese, M. (2001), \emph{Cluster Analysis}, Arnold, London, p. 103.

Gordon, A.D. (1999), \emph{Classification}, Chapman & Hall/CRC, London, p. 62.

Milligan, G.W., Cooper, M.C. (1985), \emph{An Examination of Procedures of Determining the Number of Clusters in a Data Set}, "Psychometrika", Vol. 50, No. 2, pp. 159-179.

Diday, E., Noirhomme-Fraiture, M. (Eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester, pp. 236-262.

Dudek, A. (2007), \emph{Cluster Quality Indexes for Symbolic Classification. An Examination}, In: H.H.-J. Lenz, R. Decker (Eds.), Advances in Data Analysis, Springer-Verlag, Berlin, pp. 31-38. 
}
\details{
See file \url{../doc/indexG1d_details.pdf} for further details
}
\seealso{
\code{\link{DClust}}, \code{\link{SClust}}; \code{index.G2}, \code{index.G3}, \code{index.S}, \code{index.H},\code{index.KL},\code{index.Gap}, \code{index.DB} in \code{clusterSim} library
}
\examples{
# LONG RUNNING - UNCOMMENT TO RUN
# Example 1
#library(stats)
#data("cars",package="symbolicDA")
#x<-cars
#d<-dist.SDA(x, type="U_2")
#wynik<-hclust(d, method="ward", members=NULL)
#clusters<-cutree(wynik, 4)
#G1d<-index.G1d(d, clusters)
#print(G1d)

# Example 2


#data("cars",package="symbolicDA")
#md <- dist.SDA(cars, type="U_3", gamma=0.5, power=2)
# nc - number_of_clusters
#min_nc=2
#max_nc=10
#res <- array(0,c(max_nc-min_nc+1,2))
#res[,1] <- min_nc:max_nc
#clusters <- NULL
#for (nc in min_nc:max_nc)
#{
#cl2 <- pam(md, nc, diss=TRUE)
#res[nc-min_nc+1,2] <- G1d <- index.G1d(md,cl2$clustering)   
#clusters <- rbind(clusters, cl2$clustering)
#}
#print(paste("max G1d for",(min_nc:max_nc)[which.max(res[,2])],"clusters=",max(res[,2])))
#print("clustering for max G1d")
#print(clusters[which.max(res[,2]),])
#write.table(res,file="G1d_res.csv",sep=";",dec=",",row.names=TRUE,col.names=FALSE)
#plot(res, type="p", pch=0, xlab="Number of clusters", ylab="G1d", xaxt="n")
#axis(1, c(min_nc:max_nc))



}
\keyword{cluster}
