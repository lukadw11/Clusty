\name{summaryheat}
\alias{summaryheat}
\title{
Within-cluster homogeneity and between-cluster heterogeneity plot
}
\description{
This function takes a data frame of instances characterized by cluster IDs, condenses the instance vectors, creates a distance matrix between each instance grouped by cluster, and visualizes the clustering summary using a heat map gradient from very similar (blue) to very different (red). The diagonal is then ranked by most to least homogeneous cluster. The upper and lower symmetrical triangular matrices are ranked by most to least heterogeneous. The intuition is that a highly homogeneous cluster (i.e. with similar instances) corresponds to strong clustering, and likewise with highly differentiated cluster comparisons.
}
\usage{
summaryheat(df, compare_metric = "Mean", ranks = TRUE, dist_metric = "euclidean", axislabs = "Aggregated Cluster Distance", title = "Differentiation Rank", interactive = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
(REQUIRED) Data frame containing numeric features and a cluster ID column. Cluster ID column must be labelled "Cluster" and contain exclusive cluster IDs of type numeric, integer, or factor.
}
  \item{compare_metric}{
(OPTIONAL) Character argument for how the distance between cluster instances should be compared. The options are "mean","sd", "range", and "median". The assumption with each metric is that a higher value is good for differentiation (i.e. between-cluster heterogeneity) and a lower value is good for validating cluster membership (i.e. within-cluster homogeneity). 
}
  \item{ranks}{
(OPTINAL) Boolean argument of whether or not to include ranks in the heat map output. Diagonal is ranked by within-cluster instance similarity. Upper and lower triangle are ranked by between-cluster instance differentiation.
}
  \item{dist_metric}{
(OPTIONAL) Character argument of what method to use for measuring distance between instances. Arguments are limited to those provided in the dist base-function, which include "euclidean", "maximum", "Manhattan", "Canberra", "binary" and "Malinowski".
}
  \item{axislabs}{
(OPTIONAL) Character argument of the plot axis labels (note: one argument)
}
  \item{title}{
(OPTIONAL) Character argument of the plot title.
}
\item{interactive}{
(OPTIONAL) Logical argument to enable plotly's ggplotly function. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A ggplot cluster-level distance matrix heat map visualization.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Derek Lukacsko & Jonathan Bourne
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
bigextract, bigheat
}
\examples{
# Data frame of features
df <- iris[,c(1:3)]

# Create clusters
k = 3
fit <- kmeans(df, k)

# Append cluster memebership to instance vectors
df$Cluster = fit$cluster

# Visualize using summaryheat
sumnmaryheat(df, compare_metric = "Median")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
