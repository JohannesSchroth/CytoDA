#' Application Server

packages_1 <- c('gridExtra','grid', 'ggplot2', 'cytofkit2', 'umap', 'dplyr', 'reshape', 'RColorBrewer',
                'FlowSOM', 'Rtsne', 'ggdendro', 'plotly', 'shiny', 'plotly', 'shinycssloaders', 'monocle',
                'shinydashboard', 'DT', 'flowCore', 'openCyto', 'ggsci', 'rhandsontable', 'shinyBS', 'sortable')

lapply(packages_1, require, character.only=TRUE)
library(flowCore)

app_server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=100000*1024^2)
  
  rv <- reactiveValues()
  
  # onBookmark(function(state) {
  #   state$values$currentSum <- rv$raw_data
  # })
  # 
  # # Read values from state$values when we restore
  # onRestore(function(state) {
  #   vals$sum <- rv$raw_data
  # })
  
  
  observeEvent(input$upload_data, {
    
    rv$flowset <- flowCore::read.flowSet(files = input$file$datapath)
    rv$flowframe <- fs_to_ff(fs = rv$flowset)
    rv$flowframe_transformed <- transform_data(ff = rv$flowframe, method = input$transformation_method)
    rv$raw_data <- as.data.frame(flowCore::exprs(rv$flowframe_transformed))
    # s <- edit_samplenames(data = rv$raw_data, input, output, session)
    # print('4')  
    # rv$raw_data['SampleID'] <- s[[1]]
    # print('5')
    # showModal(s[[2]])
    showModal(edit_colnames(data = rv$raw_data, fs = rv$flowset, input, output, session))
  })
  
  # observeEvent(input$submit_samplenames, {
  #    
  #   # removeModal()
  #   
  #   rv$new_sample_names <- as.data.frame(hot_to_r(input$sample_names))
  #   
  #   rv$raw_data$SampleID <- as.character(rv$new_sample_names$Name[match(rv$raw_data$SampleID, rv$new_sample_names$Sample)])
  #   
  #   
  # })
  
  observeEvent(input$submit_colnames, {
    
    removeModal()
    
    new_colnames <- as.data.frame(hot_to_r(input$edit_colnames))
    
    colnames(rv$raw_data) <- c(new_colnames[,2])
    rv$raw_data <- rv$raw_data[,as.logical(as.vector(new_colnames[,3]))]
    
    updateSelectInput(session = session, inputId = 'variables_pca', label = paste("Variables"), choices = colnames(rv$raw_data))
    updateSelectInput(session = session, inputId = 'variables_tsne', label = paste("Variables"), choices = colnames(rv$raw_data))
    updateSelectInput(session = session, inputId = 'variables_umap', label = paste("Variables"), choices = colnames(rv$raw_data))
    updateSelectInput(session = session, inputId = 'variables_dm', label = paste("Variables"), choices = colnames(rv$raw_data))
    updateSelectInput(session = session, inputId = 'variables_clustering', label = paste("Variables"), choices = colnames(rv$raw_data))
    
    updateSelectInput(session = session,inputId = 'colour_co1', label = 'Colour by:', choices = colnames(rv$raw_data))
    
    updateSelectInput(session = session,inputId = 'colour_col2', label = 'Colour by:', choices = colnames(rv$raw_data))
    updateSelectInput(session = session,inputId = 'colour_col3', label = 'Colour by:', choices = colnames(rv$raw_data))
    
    
    showModal(
      modalDialog(
        title = 'Downsampling Data',
        tagList(
          
          sliderInput('n_downsample', 'N', 0, nrow(rv$raw_data), nrow(rv$raw_data))
          
        ),
        
        footer = tagList(actionButton('submit_downsample', 'Submit'),
                         modalButton("Cancel"))
      )
    )
    
    
    
    observeEvent(input$submit_downsample, {
      
      rv$raw_data <- rv$raw_data[sample(nrow(rv$raw_data), input$n_downsample),]
      
      removeModal()
      
      output$summary_table <- DT::renderDataTable(DT::datatable(rv$raw_data, colnames = colnames(rv$raw_data)) %>% 
                                                    formatRound(columns = colnames(rv$raw_data)))
      
      output$marker_expression <- renderPlot({
        
        rv$raw_data %>%
          melt(variable_name = 'Marker') %>%
          ggplot(aes(x = value, y = Marker, fill = Marker)) +
          ggridges::geom_density_ridges(scale = 1) +
          theme_classic() +
          xlab('MFI') +
          ylab('Density')
        
      })
      
      output$pairwise_expression <- renderPlot({ 
        
        GGally::ggpairs(data = rv$raw_data, upper = list(continuous = pairwise_plot), diag = NULL, lower = list(continuous = pairwise_plot))
        
      })
    })
  })
  
  
  observeEvent(input$run_clustering, {
    
    rv$clusters <- calculate_clusters(input, output, session, data = rv$raw_data, vars = input$variables_clustering, cluster_type = input$clustering_type)
    
    updateSelectInput(session = session, inputId = 'merge_which', choices = input$clustering_type, selected = input$clustering_type[1])
    
    output$clustering_table <- DT::renderDataTable(DT::datatable(data.frame(table(rv$clusters))))
    
    rv$clus_dat <- cbind(rv$raw_data, rv$clusters) 
    
    merge_clus(input, output, session, data = rv$clus_dat, clus_col = rv$clus_dat[,input$merge_which], vars = input$variables_clustering)
    
  })
  
  

  ##Dim Reduction Methods----
  
  observeEvent(input$submit_variables_pca, {
    
    updateSelectInput(session = session, inputId = 'show_clus_pca_heatmap', choices = colnames(rv$clusters), selected = colnames(rv$clusters)[1])
    
    rv$pca <- calculate_pca(input, output, session, data = rv$clus_dat, dims = input$dimension_num_pca, vars = input$variables_pca, cluster = rv$clusters)
    
    output$pca_heatmap <- calculate_heatmap(input, output, session, data = rv$pca, vars = input$variables_pca, show_clus = input$show_clus_pca_heatmap)
    
  })
  
  observeEvent(input$submit_variables_dm, {
    
    rv$dm <- calculate_dm(input, output, session, data = rv$raw_data, dims = input$dimension_num_dm, vars = input$variables_dm, cluster_df = rv$clusters)
  })
  
  
  
  observeEvent(input$submit_variables_tsne, {
    
    rv$tsne <- calculate_tsne(input, output, session, data = rv$raw_data, dims = input$dimension_num_tsne, vars = input$variables_tsne, cluster_df = rv$clusters)
    
    output$tsne_heatmap <- calculate_heatmap(input, output, session, data = rv$tsne, vars = input$variables_tsne)
    
  })
  
  
  observeEvent(input$submit_variables_umap, {
    
    rv$umap <- calculate_umap(input, output, session, data = rv$raw_data, dims = input$dimension_num_umap, vars = input$variables_umap, cluster_df = rv$clusters)
    
    output$umap_heatmap <- calculate_heatmap(input, output, session, data = rv$umap, vars = input$variables_umap)
    
  })
  
  
  
  #Clustering Methods----
  
  
  #Pseudotime Methods---
  
  # if (any(grepl('Monocle3', input$psudotime_type)) == TRUE) {
  #   
  #   cell_md <- cbind(seq(1,length(df_all_t[1,]), 1),'1')
  #   rownames(cell_md) <- cell_md[,1]
  #   
  #   cds_all <- new_cell_data_set(as.matrix(t(data)), cell_metadata = cell_md)
  #   
  #   cds_all <- preprocess_cds(cds_all, norm_method = 'size_only', num_dim = 2)
  #   cds_all <- reduce_dimension(cds_all, max_components = 2)
  #   cds_all <- cluster_cells(cds_all)
  #   cds_all <- learn_graph(cds_all)
  #   cds_all <- order_cells(cds_all, root_cells = input$root_cell_cluster)
  #   
  #   
  #   
  # }
  
  
  
  
}
