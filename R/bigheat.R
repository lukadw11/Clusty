library(tidyr)
library(dplyr)
library(ggplot2)
#df = data frame with features and clusters; Cluster column must be named "Cluster" and contain integers correpsonding to cluster IDs, must be the last column. The feature vectors must all be numeric so a measure of distance can be used to create matrices
#order_diag = logical argument; order the gradient of observations within each cluster
#merge = the number of cell in the x and y direction to merge, merging on a high number of cells will reduce memory requirements at the cost of reduced granularity in the visualization
#dist_metric = What method/metric to calculate distance between observations
#legend = plot legend label
#axislabs = plot axis labels
#title = plot title label
bigheat <-function(df, 
                   order_diag = FALSE, 
                   merge = 10,
                   dist_metric = "euclidean", 
                   legend = "Distance", 
                   axislabs = "Condensed Distance Vectors", 
                   title = "Cluster Differentiation"){
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
  print("Creating distance matrices")
  feat_cols = (length(df) - 1)
  g <- as.matrix(dist(df[1:feat_cols],method = dist_metric))
  
  #Order the clusters (diaganol of the heat map)
  indx_list = numeric(0)
  for (i in 1:(NROW(clustvect)-1)){
    c1 = subset.matrix(g, select = c((clustvect[i]+1):clustvect[i+1])) #Extract the cluster
    c2 = subset.matrix(t(c1), select = c((clustvect[i]+1):clustvect[i+1])) #Transpose and isolate the diagnol
    c3 =  c2[,order(colSums(c2))] # Order by column sum
    c4 =  c3[order(rowSums(c2)),] # Order by row sum (creating an ordered square matrix)
    order_vect = as.numeric(row.names(c4)) # Extract the order indices to use in ordering the initial data frame
    indx_list = append(indx_list, order_vect) # Append indices, should equal length of original data frame
  }
  remove(c1,c2,c3,c4) #free memory
  
  #Arrange original data frame by new indices
  test2 = df[indx_list,]
  
  #Create distance matrix ordered diaganol
  g1 <- as.matrix(dist(test2[1:feat_cols], method = dist_metric))
  
  print("Distance matrices created")
  
  ##COMPRESSION##
  #Diag argument implementation
  g_plot <- if(order_diag == TRUE){
    g1
  }else{
    g
  }
  remove(g,g1) #free memory
  
  #Labels in plot
  clus_labs = vector("character",max(clusts))
  for(i in 1:clusts){
    clus_labs[i]<- paste("C",i,sep="")
  }
  
  #Dilemeters
  dil = max(clusts)-1
  
  print("Compressing data along rows")
  totrows <-nrow(g_plot)
  remainder <- merge-(totrows %%merge)
  numgroups <-(totrows +remainder)/merge
  
  #rearrange dataframe columns and aggreagte into the predefined groups of size merge
  #rows and columns are first ordered by similarity
  test <- g_plot%>% as.data.frame %>%
    mutate(rowID =rep(1:numgroups,each=merge)[1:totrows]) %>%
    group_by(rowID) %>% summarise_each(funs(mean)) %>%
    select(-rowID) #add in -rows again is the rows data is commented back in
  
  print("Compressing data along columns")
  #allows the grouping to be flexible in the case that the dataframe changes size
  totcols <-ncol(test)
  remainder <- merge-(totcols %%merge)
  numgroups <-(totcols +remainder)/merge
  
  
  #aggregate again transposing the data frame and aggregating by the smart meters
  test <- t(test) %>% data.frame %>% 
    mutate(rowID =rep(1:numgroups,each=merge)[1:totcols]) %>%
    group_by(rowID) %>% summarise_each(funs(mean)) %>%select(-rowID) %>%
    t %>% data.frame %>% mutate( rowID = 1:nrow(.)) 
  
  #gather the data for the plot
  print("Reformatting into long form for ggplot")
  #it's importnt to remove the X and convert to integer otherwise the order is messed up
  test%<>% gather(.,key = "columnID", value =Percentvalid, -rowID )%>%
    mutate(columnID = sub("X","", columnID) %>% as.integer)
  
  print("Creating Plot")
  
  ##PLOTTING##
  ggplot(test, aes(x=columnID, y=rowID, fill = Percentvalid )) +
    geom_raster() + 
    scale_fill_gradient2(low="blue",mid = "white" ,high = "red", midpoint = mean(g_plot),name=legend) +
    theme_minimal() +
    geom_hline(yintercept = (clustvect/merge)) +
    geom_vline(xintercept = (clustvect/merge)) +
    labs(title = title, x = axislabs, y = axislabs) +
    scale_x_continuous(breaks = (labelvect/merge)[-1],labels = clus_labs) +
    scale_y_continuous(breaks = (labelvect/merge)[-1],labels = clus_labs)
}