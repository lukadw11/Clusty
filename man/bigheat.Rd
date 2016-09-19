\name{bigheat}
\alias{bigheat}
\title{
High-dimensional cluster distance heat matrix
}
\description{
This function takes a data frame of instances characterized by cluster IDs, condenses the instance vectors, creates a distance matrix between each instance grouped by cluster, and visualizes the clustering using a heat map color gradient from very similar (i.e. blue; low distance) to very dissimilar (i.e. red; high distance).
}
\usage{
bigheat(df, order_diag = FALSE, merge = 10, dist_metric = "euclidean", legend = "Distance", axislabs = "Condensed Distance Vectors", title = "Cluster Differentiation")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
(REQUIRED) Data frame containing numeric features and a cluster ID column. Cluster ID column must be labelled "Cluster" and contain exclusive cluster IDs of type numeric, integer, or factor.
}
  \item{order_diag}{
(OPTIONAL) Boolean argument of whether or not to order the diagonal of the final heat matrix. A ordered diagonal will order the gradient of instances within each cluster, thus highlighting how much of each cluster is very similar or very different.
}
  \item{merge}{
(OPTIONAL) Numeric argument of how many instance vectors to condense for the heat map visualization (e.g. 10,000 rows with merge = 10 corresponds to 1,000 instances in the heat map).
}
  \item{dist_metric}{
(OPTIONAL) Character argument of what method to use for measuring distance between instances. Arguments are limited to those provided in the dist base-function, which include "euclidean", "maximum", "Manhattan", "Canberra", "binary" and "Malinowski".
}
  \item{legend}{
(OPTIONAL) Character argument of the plot legend title.
}
  \item{axislabs}{
(OPTIONAL) Character argument of the plot axis labels (note: one argument)
}\item{title}{
(OPTIONAL) Character argument of the plot title.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A ggplot instance-level distance matrix heat map visualization.
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
bigextract, summaryheat
}
\examples{
# Load necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Data frame of features
df <- iris[,c(1:3)]

# Create clusters
k = 3
fit <- kmeans(df, k)

# Append cluster memebership to instance vectors
df$Cluster = fit$cluster

# Visualize using bigheat
bigheat(df, order_diag = TRUE, merge = 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
