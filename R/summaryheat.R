library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
# Takes same dataframe input as bigheat and bigextract
# metric = what to rank the squares by; mean = "Mean", standard deviation = "Sd", range = "Range", median = "Median"
summaryheat<-function(df, 
                      compare_metric = "Mean",
                      ranks = TRUE,
                      dist_metric = "euclidean", 
                      axislabs = "Aggregated Cluster Distance",
                      title = "Differentiation Rank",
                      interactive = FALSE){
  
  #Sample data frame structure
  # df <- iris[,-5]
  # cluster_obj <- kmeans(df, centers=4)
  # df$Cluster <- cluster_obj$cluster
  # df <- df %>% arrange(as.numeric(Cluster))
  
  #Create cluster dilemetters
  clusts = length(unique(df$Cluster))
  clustvect = rep(clusts)
  for (i in 1:clusts){
    clustvect[i] = NROW(df[df$Cluster == i,])
  }
  
  #Create cluster ticks vector
  clustvect <-c(0,cumsum(clustvect))
  labelvect = rep(clusts)
  for (i in 1:(clusts+1)){
    if (i!=1){
      labelvect[i] = (clustvect[i] - clustvect[i-1])/2 + clustvect[i-1]
    }
    else{
      labelvect[i] = clustvect[i]/2
    }
  }
  
  #Create distance matrix
  feat_cols = (length(df) - 1)
  g <- as.matrix(dist(df[1:feat_cols],method = dist_metric))
  print("Distance matrix created")
  
  #Isolate the clustering (rectangles)
  rectangles = list()
  for(i in 1:clusts){
    rectangles[[i]] = subset.matrix(g, select = c(clustvect[i]:clustvect[i+1]))
  }
  
  #Extract the cluster relationships (squares)
  blocks = list()
  k = 1
  for(i in 1:clusts){
    block = t(rectangles[[i]])
    for(j in 1:clusts){
      blocks[[k]] = subset.matrix(block, select = c(clustvect[j]:clustvect[j+1]))
      k = k + 1
    }
  }
  print("Cluster relationships determined")
  length(as.data.frame(lapply(blocks, dim))) #DELETE
  
  #Number of squares/comparisons
  squares = clusts^2
  
  #Empty data frame containing metrics for all squares
  All_clustering = data.frame(Mean = numeric(squares), 
                              Sd = numeric(squares), 
                              Range = numeric(squares), 
                              Median = numeric(squares),
                              Size = numeric(squares))
  
  for(i in 1:squares){
    # Access only the diagnol block corresponding to the given cluster
    inter_df = blocks[[i]]
    
    # Record metrics
    All_clustering[i,1] = mean(inter_df)
    All_clustering[i,2] = sd(inter_df)
    All_clustering[i,3] = max(inter_df) - min(inter_df)
    All_clustering[i,4] = median(inter_df)
    All_clustering[i,5] = NROW(inter_df)
  }
  
  #Locate the diagnol (intra) and non-diagnol squares (inter)
  Diag = numeric(rep(clusts))
  z = 1
  for(i in 1:clusts){
    Diag[i] = z
    z = z + (clusts + 1)
  }
  Non_diag = setdiff(c(1:squares),Diag)
  
  #Labels in plot
  clus_labs = vector("character",max(clusts))
  for(i in 1:clusts){
    clus_labs[i]<- paste("C",i,sep="")
  }
  
  #Dataframe with aggregated metrics to create heat map ranks
  plot_df <- All_clustering %>% 
    mutate(value = rep(1:clusts,times=clusts), 
           key = rep(1:clusts,each=clusts),
           rank = numeric(1))

  #Paul Hiemstra: May 10, 2013 #IMPLEMENT LESS COSTLY SORTING ALGORITHM
  larger = function(pair) {
    if(pair[1] > pair[2]) return(TRUE) else return(FALSE)
  }
  swap_if_larger = function(pair) {
    if(larger(pair)) {
      return(rev(pair)) 
    } else {
      return(pair)
    }
  }
  swap_pass = function(vec) { 
    for(i in seq(1, length(vec)-1)) {
      vec[i:(i+1)] = swap_if_larger(vec[i:(i+1)])
    }
    return(vec)
  }
  bubble_sort = function(vec) {
    new_vec = swap_pass(vec)
    if(isTRUE(all.equal(vec, new_vec))) { 
      return(new_vec) 
    } else {
      return(bubble_sort(new_vec))
    }
  }
  
  #Find ranks of diaganol; i.e. the lowest value (least distance) has the best (lowest rank)
  ranked_diag <- bubble_sort(plot_df[Diag,compare_metric])
  for(i in Diag){
    for(j in 1:length(ranked_diag)){
      if(ranked_diag[j] == plot_df[i,compare_metric]){
        plot_df$rank[i] = j
      }
    }
  }
  
  #Find ranks of non-diaganol; i.e. the highest value (greatest distance) is the highest rank
  ranked_nondiag <- bubble_sort(plot_df[Non_diag,compare_metric])
  ranked_nondiag <- sort(ranked_nondiag,decreasing=TRUE)
  ranked_nondiag <- ranked_nondiag[seq(1,length(ranked_nondiag),2)]
  
  #Find ranks of triangular matrices; i.e. the highest value (greatest distance) has the best (lowest) rank
  for(j in 1:(length(ranked_nondiag))){ 
    for(i in Non_diag){                   
      if(all.equal(ranked_nondiag[j],plot_df[i,compare_metric]) == TRUE){
        plot_df$rank[i] = j
      }
    }
  }
  
  #Logical argument, whether to annotate squares by ranks
  if(ranks == TRUE){
    square_labs <- plot_df$rank
  }else{
    square_labs <- ""
  }
  
  #Set plot parameters
  midpoint_set <- if(compare_metric == "Mean"){
    mean(g)
  }else if(compare_metric == "Median"){
    median(g)
  }else if(compare_metric == "Range"){
    mean(g)
  }else if(compare_metric == "SD"){
    sd(g)
  }
  
  #Save memory  
  remove(g,block,rectangles)

  Diff = rescale(plot_df[compare_metric]$Mean) #normalize 0-1 for coloring
  
  daPlot <- ggplot(plot_df, aes(key,value, fill = Diff )) + 
    geom_raster() +
    scale_fill_gradient2(low="blue",mid = "white", high = "red", space = "rgb", 
                         midpoint = mean(Diff), label=NULL) +
    theme_minimal() + 
    geom_hline(yintercept = 0.5+(0:(clusts))) +
    geom_vline(xintercept = 0.5+(0:(clusts))) +
    labs(title = title, x = axislabs, y = axislabs) +
    scale_x_continuous(breaks = 1:clusts,labels = clus_labs) +
    scale_y_continuous(breaks = 1:clusts,labels = clus_labs) + 
    annotate("text", x = rep(1:clusts,times = clusts), 
             y = rep(1:clusts, each = clusts), 
             label = as.character(square_labs))
  
  return(daPlot)
  
}
