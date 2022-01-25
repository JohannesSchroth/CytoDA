#' Application Server

packages_1 <- c('gridExtra','grid', 'ggplot2', 'cytofkit2', 'umap', 'dplyr', 'reshape', 'RColorBrewer',
                'FlowSOM', 'Rtsne', 'ggdendro', 'plotly', 'shiny', 'plotly', 'shinycssloaders', 'monocle',
                'shinydashboard', 'DT', 'flowCore', 'openCyto', 'ggsci', 'rhandsontable', 'shinyBS', 'sortable')

lapply(packages_1, require, character.only=TRUE)
library(flowCore)

app_server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=100000*1024^2)
  
  rv <- reactiveValues()
  
  observeEvent(input$upload_data, {
    
    if (length(input$file$datapath) > 0) {
      
      rv$flowset <- flowCore::read.flowSet(files = input$file$datapath)
      rv$flowframe <- fs_to_ff(fs = rv$flowset)
      rv$flowframe_transformed <- transform_data(ff = rv$flowframe, method = input$transformation_method)
      rv$raw_data <- as.data.frame(flowCore::exprs(rv$flowframe_transformed))
      
    } else if (input$demo_data == TRUE){
      
      rv$raw_data <- HDCytoData::Levine_13dim_SE() %>%
        SummarizedExperiment::assay() %>% # extract the data
        as.data.frame() %>% # convert to data frame
        select(colnames(.)[1:13]) %>% # select markers of interest
        mutate_all(function(x, cofactor = 5) asinh(x / cofactor)) %>% # transform data with arcsinh (cofactor of 5)
        mutate_all(function(x)(x - min(x)) / (max(x) - min(x)))
      
    } else {
      
      showModal(
        modalDialog(
          title = 'No Input Data Selected',
          'Please select your input file or run using demo data.',
        )
      )
      
    }
    
    if (!is.null(rv$raw_data)) {
      
      updateSelectInput(session = session, inputId = 'variables_pca', label = paste("Variables"), choices = colnames(rv$raw_data), selected = colnames(rv$raw_data))
      updateSelectInput(session = session, inputId = 'variables_tsne', label = paste("Variables"), choices = colnames(rv$raw_data), selected = colnames(rv$raw_data))
      updateSelectInput(session = session, inputId = 'variables_umap', label = paste("Variables"), choices = colnames(rv$raw_data), selected = colnames(rv$raw_data))
      updateSelectInput(session = session, inputId = 'variables_clustering', label = paste("Variables"), choices = colnames(rv$raw_data), selected = colnames(rv$raw_data))
      updateSelectInput(session = session,inputId = 'colour_co1', label = 'Colour by:', choices = colnames(rv$raw_data))
      updateSelectInput(session = session,inputId = 'colour_col2', label = 'Colour by:', choices = colnames(rv$raw_data))
      updateSelectInput(session = session,inputId = 'colour_col3', label = 'Colour by:', choices = colnames(rv$raw_data))
      
      
      showModal(
        modalDialog(
          title = 'Downsampling Data',
          tagList(
            
            sliderInput('n_downsample', 'N', 0, nrow(rv$raw_data), 10000, round = 1000, width = '100%')
            
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
        
        output$marker_expression <- renderPlotly({
          
          rv$raw_data %>%
            dplyr::select(-grep('FSC|SSC', colnames(.))) %>%
            melt(variable_name = 'Marker') %>%
            plot_ly(x = ~Marker, y=~value, split = ~Marker, type = 'violin', 
                    box = list(visible = T),
                    meanline = list(visible = T)) %>%
            layout(xaxis = list(title = "Marker"), yaxis = list(title = "Expression", zeroline = F)) %>%
            layout(font = list(size = 25))
          
        })
        
        output$pairwise_expression <- renderPlot({ 
          
          GGally::ggpairs(data = rv$raw_data, upper = list(continuous = pairwise_plot), diag = NULL, lower = list(continuous = pairwise_plot))
          
        })
      })
    }
  })
  
  
  observeEvent(input$run_clustering, {
    
    rv$clusters <- calculate_clusters(input, output, session, data = rv$raw_data, vars = input$variables_clustering, cluster_type = input$clustering_type)
    
    updateSelectInput(session = session, inputId = 'merge_which', choices = paste0(input$clustering_type,'_Clusters'), selected = input$clustering_type[1])
    
    output$cluster_pie <- renderPlotly({
      
      rv$clusters %>%
        table() %>%
        data.frame() %>%
        `colnames<-`(c('Cluster', 'Count')) %>%
        mutate(Frequency = Count/sum(Count)*100) %>%
        plot_ly(x = ~Cluster, y = ~Frequency, type = 'bar') %>%
        layout(font = list(size = 25))
      
    })
    
    rv$clus_dat <- cbind(rv$raw_data, rv$clusters) 
    
    merge_clus(input, output, session, data = rv$clus_dat, clus_col = rv$clus_dat[,input$merge_which], vars = input$variables_clustering)
    
    output$cluster_heatmap <- renderPlotly({
      
      heat <- data.frame(rv$clus_dat[,-grep('Rphenograph', colnames(rv$clus_dat))], 'Clusters' = rv$clus_dat[,'Rphenograph_Clusters'], check.names = FALSE)
      
      heat <- heat %>%
        group_by(Clusters) %>%
        summarise_all(median) %>% 
        as.data.frame() 
      
      plot_ly(heat, x = colnames(heat)[-1], y = as.character(heat$Clusters),z = as.matrix(heat[,-1]), type = 'heatmap',
              colors = colorRamp(c("#4575B4",'#FFFFFF','#D73027'))) %>%
        layout(xaxis = list(type = "category"), 
               yaxis = list(type = "category"), font = list(size = 25))
    })
    
    updateSelectInput(session = session,inputId = 'colour_co1', label = 'Colour by:', choices = colnames(rv$raw_data), selected = 'Rphenograph_Clusters')
    updateSelectInput(session = session,inputId = 'colour_col2', label = 'Colour by:', choices = colnames(rv$raw_data), selected = 'Rphenograph_Clusters')
    updateSelectInput(session = session,inputId = 'colour_col3', label = 'Colour by:', choices = colnames(rv$raw_data), selected = 'Rphenograph_Clusters')
    
  })
  
  
  
  ##Dim Reduction Methods----
  
  observeEvent(input$submit_variables_pca, {
    
    if (is.null(rv$clus_dat)) {
      showModal(
        modalDialog(
          title = 'Error',
          'Please run a clustering algorithm prior to dimensionality reduction.',
        )
      )
    } else {
      
      updateSelectInput(session = session, inputId = 'show_clus_pca_heatmap', choices = colnames(rv$clusters), selected = colnames(rv$clusters)[1])
      
      rv$pca <- calculate_pca(input, output, session, data = rv$clus_dat, dims = input$dimension_num_pca, vars = input$variables_pca)
      
      output$pca_heatmap <- calculate_heatmap(input, output, session, data = rv$pca, vars = input$variables_pca, show_clus = input$show_clus_pca_heatmap)
    }
  })
  
  observeEvent(input$submit_variables_tsne, {
    
    if (is.null(rv$clus_dat)) {
      showModal(
        modalDialog(
          title = 'Error',
          'Please run a clustering algorithm prior to dimensionality reduction.',
        )
      )
    } else {
      rv$tsne <- calculate_tsne(input, output, session, data = rv$raw_data, dims = input$dimension_num_tsne, vars = input$variables_tsne, cluster_df = rv$clusters)
      
      output$tsne_heatmap <- calculate_heatmap(input, output, session, data = rv$tsne, vars = input$variables_tsne)
    }
  })
  
  
  observeEvent(input$submit_variables_umap, {
    
    if (is.null(rv$clus_dat)) {
      showModal(
        modalDialog(
          title = 'Error',
          'Please run a clustering algorithm prior to dimensionality reduction.',
        )
      )
    } else {
      
      rv$umap <- calculate_umap(input, output, session, data = rv$raw_data, dims = input$dimension_num_umap, vars = input$variables_umap, cluster_df = rv$clusters)
      
      output$umap_heatmap <- calculate_heatmap(input, output, session, data = rv$umap, vars = input$variables_umap)
    }
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
