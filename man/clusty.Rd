\name{clusty}
\alias{clusty}
\docType{package}
\title{
\packageTitle{clusty}
}
\description{
\packageDescription{clusty}
}
\details{
The following pacakge is used for evaluating cluster with non-overlapping membership. It was initially designed as a heuristic assessment for the K-Means algorithm. A efficacious cluster is one where the features of members within the cluster are significantly similar to each other AND significantly differentiated from members in other clusters. This is not obvious using typical assessment methods (e.g. WCSS). Using a heat map of cluster distances, a highly ranked diagonal in the summaryheat function is characterized by strong within-cluster homogeneity; a highly ranked upper or lower triangle conversely is characterized by strong inter-cluster heterogeneity. In this way, cluster differentiation and homogeneity can be assessed in one visual. The distance metrics used for the visual can be extracted and inspected using the bigextract function. The bigheat function provides a granular view at how well differentiated instance vectors are within and between clusters. It also has the option to condense instance vectors for efficiently handling large datasets. 

These functions are intended for heuristically evaluation, augmenting traditional clustering evaluation methods such as WCSS, gap statistic, and silhouette width.
}
\author{
\packageAuthor{clusty}

}
\references{

}

\keyword{ package }
\seealso{
summaryheat, bigextract, bigheat
}
\examples{
##see function documentation##
}
