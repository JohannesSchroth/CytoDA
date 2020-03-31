#' Function to cluster data
#' @param input/output/session standard shiny parameters to allow interactivity
#' @param data data frame which to use for clustering
#' @param vars variables to use for clustering
#' @param cluster_type which clustering algorithm to use, multiple allowed
#' @return data frame with each column being a different clustering method

calculate_clusters <- function(input, output, session, data, vars, cluster_type) {
  
  # require cluster_type input
  req(cluster_type)
  
  # create empty list
  clusters <- list()
  
  # Phenograph clustering method
  if (any(grepl('Rphenograph', cluster_type)) == TRUE) {
    
    phenograph <- cytofkit::Rphenograph(data[,vars], k = 10)
    clusters[['Rphenograph']] <- as.character(phenograph$membership)
    
  } 
  
  # ClusterX clustering method
  if (any(grepl('ClusterX', cluster_type)) == TRUE) {
    
    clusterx <- cytofkit::ClusterX(data[,vars])
    
    clusters[['ClusterX']] <- as.character(clusterx$cluster)
    
  }
  
  # Consensuis clustering method
  if (any(grepl('ConsensusClusterPlus', cluster_type)) == TRUE) {
    
    fFrame <- data[,vars] %>%
      as.matrix() %>%
      flowCore::flowFrame()
    print('fFrame')
    som <- fFrame %>%
      FlowSOM::ReadInput() %>%
      FlowSOM::BuildSOM()
    print('som')
    mc <- ConsensusClusterPlus::ConsensusClusterPlus(t(som$map$codes), 
                                                     maxK = 30, 
                                                     reps = 100, 
                                                     pItem = 1, 
                                                     pFeature = 1,
                                                     plot = 'png',
                                                     clusterAlg = "hc", 
                                                     innerLinkage = "average", 
                                                     finalLinkage = "average", 
                                                     distance = "euclidean", 
                                                     seed = 1234)
    
    code_clustering <- mc[[10]]$consensusClass
    
    clusters[['ConsensusClusterPlus']] <- as.character(code_clustering[som$map$mapping[,1]])
  }
  
  # Return clusters data frame
  if (length(clusters) != 0) {
    
    df <- clusters %>%
      unlist() %>%
      matrix(nrow = length(clusters[[1]])) %>%
      as.data.frame()
    
    colnames(df) <- names(clusters)
    
    return(df)
    
  }
  
}