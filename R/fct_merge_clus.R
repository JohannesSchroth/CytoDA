merge_clus <- function(input, output, session, data, clus_col, vars) {
  
  clusters_reactive <- reactiveVal(list())
  names_reactive <- reactiveVal(list())
  count <- reactiveVal(0)
  
  observeEvent(input$add_clus, {
    
    count(count()+1)
    
    insertUI(
      selector = '#cluster_placeholder',
      where = 'afterEnd',
      ui = mod_merge_clus_ui(id = paste0('mod',count()), clus_col_mod = unique(clus_col))
    )
    
    current <- isolate(clusters_reactive())
    new_cluster <- callModule(clusters_server, paste0('mod',count()))
    current <- c(current, new_cluster)
    clusters_reactive(current)
    
    names_current <- isolate(names_reactive())
    new_name <- callModule(names_server, paste0('mod',count()))
    names_current <- c(names_current, new_name)
    names_reactive(names_current)
    
  })
  
  
  output$clustering_heatmap <- renderPlotly({
    
    heat_data <- data %>%
      group_by_(input$merge_which) %>%
      select(vars) %>%
      summarise_if(is.numeric, median)
    
    heatmaply::heatmaply(heatmaply::normalize(heat_data), 
                         scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                           low="#4575B4", mid= 'white' , high='#D73027', midpoint=0.5, limits=c(0,1)
                         )
    )
  })
  
  
  output$which <- renderPrint({
    if (is.null(names_reactive()) != TRUE) {
      df <- lapply(clusters_reactive(), function(x) x() %>% unlist())
      names(df) <-lapply(names_reactive(), function(x) x() %>% unlist())
      stack(df)
    } else {
      print('Merge Clusters')
    }
  })
  
  
  observeEvent(input$delete_clus,{
    
    removeUI(selector = paste0('#mod',count()))
    
    if (count() > 0) {
      count(count()-1)
    }
    
    clusters_reactive(clusters_reactive()[0:count()])
    names_reactive(names_reactive()[0:count()])
    
  })
  
  observeEvent(input$submit_merge_clus,{
    
    df <- lapply(clusters_reactive(), function(x) x() %>% unlist())
    names(df) <-lapply(names_reactive(), function(x) x() %>% unlist())
    df <- stack(df)
    
    data['Merged_Clusters'] <- df[match(data[,clus_col], df[,1]), 2]
    
    heat_data <- data %>%
      group_by(Merged_Clusters) %>%
      select(vars) %>%
      summarise_if(is.numeric, median)
    
    output$clustering_heatmap <- renderPlotly(
      
      heatmaply::heatmaply(heatmaply::normalize(heat_data), 
                           scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                             low="#4575B4", mid= 'white' , high='#D73027', midpoint=0.5, limits=c(0,1)
                           )
      )
      
    )
  })
  
  
}