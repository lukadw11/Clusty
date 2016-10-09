\name{bigextract}
\alias{bigextract}
\title{
Within-cluster homogeneity and between-cluster heterogeneity data
}
\description{
The function produces the data used to create summaryheat plot. Each item in the produced data frame is a square of the heat matrix starting from bottom left (1) to top right (clusts^2). The data frame metrics relveal differentiation and similarity between and within clusters using mean, median, standard deviation, and range.
}
\usage{
bigextract(df, output = "full", dist_metric = "euclidean")
}
\arguments{
  \item{df}{
(REQUIRED) Data frame containing numeric features and a cluster ID column. Cluster ID column must be labelled "Cluster" and contain exclusive cluster IDs of type numeric, integer, or factor.
}
  \item{output}{
(OPTIONAL) Character argument corresponding to which comparisons from the matrix will be pulled. "full" pulls the entire matrix; "diagonal" pulls only the within-cluster comparisons; and "triangle" pulls only the between-cluster comparisons.
}
  \item{dist_metric}{
(OPTIONAL) Character argument of what method to use for measuring distance between instances. Arguments are limited to those provided in the dist base-function, which include "euclidean", "maximum", "Manhattan", "Canberra", "binary" and "Malinowski".
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Dataframe where each instance corresponds to a square of the distance matrix. Columns contain the mean, standard deviation, range, and median distance between (triangular matrices) and within clusters (diaganol), as well as the number of instances begin compared (Size), and where the square lies on the matrix (diaganol or triangle). The size of diaganol comparisons correponds to the number of instances in that cluster. The size of the triangle comparisons corresponds to the sum of the number of instances being compared to each other. Intuitively, the size/instances being compared in the diaganols will be larger because two clusters are being compared to each other. The "pair" column corresponds to which two clusters are being compared where the first cluster corresponds to the row (y) of the summaryheat output and the second cluster corresponds to the column (x) of the summaryheat output.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Data frame of features
df <- iris[,c(1:3)]

# Create clusters
k = 3
fit <- kmeans(df, k)

# Append cluster memebership to instance vectors
df$Cluster = fit$cluster

# Pull data frame of cluster comparisons using bigextract
bigextract(df)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
