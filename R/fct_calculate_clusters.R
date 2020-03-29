#' Function to Cluster data
#' 
#' 


calculate_clusters <- function(input, output, session, data, vars, cluster_type) {
  
  clusters <- NULL
  
  if (any(grepl('Rphenograph', cluster_type)) == TRUE) {
    
    phenograph <- cytofkit::Rphenograph(data[,vars], k = 10)
    clusters['Phenograph_Clusters'] <- as.character(phenograph$membership)
    
  } 
  
  if (any(grepl('ClusterX', cluster_type)) == TRUE) {
    
    clusterx <- cytofkit::ClusterX(data[,vars])
    
    clusters['ClusterX_Clusters'] <- as.character(clusterx$cluster)
    
  } else {
    
    NULL
    
  }
  
  if (length(clusters) != 0) {
    
    return(clusters[,-1])
    
  } else {
    NULL
  }
  
  
}