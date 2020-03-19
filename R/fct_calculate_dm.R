#' Function to calculate Diffusion Map
#' 


calculate_dm <- function(input, output, session, data, vars, dims, cluster_df) {
  
  dm <- destiny::DiffusionMap(data[,vars])
  dpt <- destiny::DPT(dm)
  
  d <- cbind(data, 'Clusters' = as.factor(cluster_df[,'Phenograph_Clusters']), 
             'Pseudotime' = rank(dpt$dpt), 
             'DC1' = eigenvectors(dm)[,1], 
             'DC2' = eigenvectors(dm)[,2])
  
  d$Clusters <- with(d, reorder(Clusters, Pseudotime, median))
  
  output$dm_plot <- renderPlotly({ 
    
    plot_ly(d, x = d$DC1, y = d$DC2, marker = list(color = d$Pseudotime)) 
  })
  
  output$pseudotime_plot <- renderPlotly({ 
    
    plot_ly(d, x = ~d$Pseudotime, y = ~d$Clusters, 
                                    type = 'box', boxpoints = "all", jitter = 0.3, 
                                    color = ~d$Clusters)
  })
  
  return(d)
  
  
  
  
}