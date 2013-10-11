\name{HINoV.SDA}
\alias{HINoV.SDA}
\title{Modification of HINoV method for symbolic data}
\description{
Carmone, Kara \& Maxwell's Heuristic Identification of Noisy Variables (HINoV) method for symbolic data}
\usage{
HINoV.SDA(table.Symbolic, u=NULL, distance="H", Index="cRAND",method="pam",...)
}
\arguments{
\item{table.Symbolic}{symbolic data table}
\item{u}{number of clusters}
\item{distance}{symbolic distance measure as parameter type in \code{\link{dist.SDA}}}
\item{method}{clustering method: "single", "ward", "complete", "average", "mcquitty", "median", "centroid", "pam" (default), "SClust", "DClust"}
\item{Index}{"cRAND" - adjusted Rand index (default); "RAND" - Rand index}
\item{...}{additional argument passed to \code{\link{dist.SDA}} function}
}
\value{
\item{parim}{\emph{m} x \emph{m} symmetric matrix (\emph{m} - number of variables). Matrix contains pairwise adjusted Rand (or Rand) indices for partitions formed by the \emph{j}-th variable with partitions formed by the \emph{l}-th variable}
\item{topri}{sum of rows of \code{parim}}
\item{stopri}{ranked values of \code{topri} in decreasing order}
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl}, Justyna Wilk \email{justyna.wilk@ue.wroc.pl}
Department of Econometrics and Computer Science, Wroclaw University of Economics, Poland \url{http://keii.ue.wroc.pl/symbolicDA}
}
\references{
Bock, H.H., Diday, E. (Eds.) (2000), \emph{Analysis of Symbolic Data. Explanatory Methods for Extracting Statistical Information from Complex Data}, Springer-Verlag, Berlin.

Diday, E., Noirhomme-Fraiture, M. (Eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.

Carmone, F.J., Kara, A., Maxwell, S. (1999), \emph{HINoV: a New Method to Improve Market Segment Definition by Identifying Noisy Variables}, "Journal of Marketing Research", November, vol. 36, pp. 501-509.

Rand, W.M. (1971), \emph{Objective Criteria for the Evaluation of Clustering Methods}, "Journal of the American Statistical Association", no. 336, pp. 846-850.

Walesiak, M., Dudek, A. (2008), \emph{Identification of Noisy Variables for Nonmetric and Symbolic Data in Cluster Analysis},In: C. Preisach, H. Burkhardt, L. Schmidt-Thieme, R. Decker (Eds.), Data Analysis, Machine Learning and Applications, Studies in Classification, Data Analysis, and Knowledge Organization, Springer-Verlag, Berlin-Heilderberg, pp. 85-92.
}
\details{
For HINoV in symbolic data analysis there can be used methods based on distance matrix such as hierarchical ("single", "ward", "complete", "average", "mcquitty", "median", "centroid") and optimization methods ("pam", "DClust") and also methods based on symbolic data table ("SClust").

See file \url{../doc/HINoVSDA_details.pdf} for further details
}
\seealso{
\code{DClust}, \code{SClust}, \code{dist.SDA}; \code{HINoV.Symbolic}, \code{dist.Symbolic} in \code{clusterSim} library; \code{hclust} in \code{stats} library; \code{pam} in \code{cluster} library
}
\examples{
# LONG RUNNING - UNCOMMENT TO RUN
#data("cars",package="symbolicDA")
#r<- HINoV.SDA(cars, u=3, distance="U_2")
#print(r$stopri)
#plot(r$stopri[,2], xlab="Variable number", ylab="topri",
#xaxt="n", type="b")
#axis(1,at=c(1:max(r$stopri[,1])),labels=r$stopri[,1])
}
\keyword{cluster}
