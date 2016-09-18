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
The following function is used for evaluating partitioning-based clustering with non-overlapping cluster membership. Specifically, it was designed for assessing K-Means clustering as the objective function is based on distance. Effective clustering is thus where instances within clusters are significantly similar and instances between clusters are significantly differentiated. Thus, using a heat map of cluster distance, a highly ranked diagonal in the summaryheat is characterized by strong intra-cluster homogeneity; a highly ranked upper or lower triangle of the heat map is characterized by strong inter-cluster heterogeneity. The distance metrics used for this heat map can be extracted and inspected using bigextract. Rather than summarizing the average distance within clusters, bigheat uses condensed instance vectors to visualize the clustering (i.e. intra-cluster homogeneity and inter-cluster heterogeneity) at the instance level. This provides a granular look at how well differentiated instance vectors are within and between clusters. 

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
