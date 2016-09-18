library(tidyr)
library(dplyr)
library(ggplot2)
# Takes same dataframe as bigheat
# output = "triangle" extracts only the comparison squares (i.e. the lower and upper triangular matrices), "diagonal" extracts only the diagonal
# dist_metric = measures of distance for dist
bigextract<-function(df, output="full", dist_metric = "euclidean"){
  #Working data frame
  df <- df %>% arrange(as.numeric(Cluster))
  
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
  
  #Number of squares in heatmap
  squares = clusts^2
  
  # Locate the diagnol (intra) and non-diagonal squares (inter)
  Diag = numeric(rep(clusts))
  z = 1
  for(i in 1:clusts){
    Diag[i] = z
    z = z + (clusts + 1)
  }
  Non_diag = setdiff(c(1:squares),Diag)
  
  #Data frame that contains metrics for all observations (all squares)
  All_clustering = data.frame(Mean = numeric(squares), 
                              Sd = numeric(squares), 
                              Range = numeric(squares), 
                              Median = numeric(squares),
                              Size = numeric(squares))
  
  for(i in 1:squares){
    # Access only the diagonal block corresponding to the given cluster
    inter_df = blocks[[i]]
    
    # Record metrics
    All_clustering[i,1] = mean(inter_df)
    All_clustering[i,2] = sd(inter_df)
    All_clustering[i,3] = max(inter_df) - min(inter_df)
    All_clustering[i,4] = median(inter_df)
    All_clustering[i,5] = NROW(inter_df)
  }
  
  All_clustering <- All_clustering %>% mutate(Position = character(squares))
  for(i in 1:squares){
    All_clustering[i,6] <- if(is.element(i,Diag) == TRUE){
      "diaganol"
    }else{
      "triangle"
    }
  }
  
  #Metrics of cluster observations
  Intra_clustering = All_clustering[Diag,] %>% 
    mutate(Mean = Mean/mean(g), 
           Sd = Sd/sd(g), 
           Range = Range/(max(g)-min(g)), 
           Median = Median/median(g), 
           Size = Size)
  
  # Alter triangle populations to include customers in both clusters (triangle + diangonal)
  z = rep(1:(clusts), times = clusts) #positions in grid/triangles where population needs to be appended
  for(i in 1:squares){
    if(All_clustering[i,"Position"] == "triangle"){
      j = j + 1
      All_clustering[i,"Size"] <- All_clustering[i,"Size"] + Intra_clustering[z[i],"Size"]
    }
  }
  
  #Metrics of clsuter obervations to other cluster observations
  Inter_clustering = All_clustering[Non_diag,]  %>% 
    mutate(Mean = Mean/mean(g), 
           Sd = Sd/sd(g), 
           Range = Range/(max(g)-min(g)), 
           Median = Median/median(g),
           Size = Size)
  
  
  #Save memory  
  remove(g,block,rectangles) 
  
  #Print appropriate extract
  if(output == "diagonal"){
    Intra_clustering
  }else if(output == "triangle"){
    Inter_clustering
  }else{
    All_clustering
  }
  
}