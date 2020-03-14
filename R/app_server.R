#' Application Server

packages_1 <- c('gridExtra','grid', 'ggplot2', 'umap', 'dplyr', 'reshape', 'RColorBrewer', 'cytofkit',
                'FlowSOM', 'Rtsne', 'ggdendro', 'plotly', 'shiny', 'plotly', 'shinycssloaders', 'monocle',
                'shinydashboard', 'DT', 'flowCore', 'openCyto', 'ggsci', 'rhandsontable', 'shinyBS')
lapply(packages_1,library, character.only=TRUE)
library(flowCore)

app_server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  observeEvent(input$upload_data, {
    
    rv$flowset <- flowCore::read.flowSet(files = input$file$datapath)
    
    rv$flowframe <- fs_to_ff(fs = rv$flowset)
    
    rv$flowframe_transformed <- transform_data(ff = rv$flowframe, method = input$transformation_method)
    
    rv$raw_data <- as.data.frame(flowCore::exprs(rv$flowframe_transformed))
    
    s <- edit_samplenames(data = rv$raw_data, input, output, session)
    
    rv$raw_data['SampleID'] <- s[[1]]
    
    showModal(s[[2]])
    
    observeEvent(input$submit_samplenames, {
      
      removeModal()
      
      rv$new_sample_names <- as.data.frame(hot_to_r(input$sample_names))
      
      rv$raw_data$SampleID <- as.character(rv$new_sample_names$Name[match(rv$raw_data$SampleID, rv$new_sample_names$Sample)])
      
      showModal(edit_colnames(data = rv$raw_data, fs = rv$flowset, input, output, session))
      
    })
    
  })
  
  observeEvent(input$submit_colnames, {
    
    removeModal()
    
    new_colnames <- as.data.frame(hot_to_r(input$edit_colnames))
    
    colnames(rv$raw_data) <- c(new_colnames[,2])
    rv$raw_data <- rv$raw_data[,as.logical(as.vector(new_colnames[,3]))]
    
    updateSelectInput(session = session, inputId = 'variables_pca', label = paste("Variables"), choices = colnames(rv$raw_data))
    updateSelectInput(session = session, inputId = 'variables_tsne', label = paste("Variables"), choices = colnames(rv$raw_data))
    updateSelectInput(session = session, inputId = 'variables_umap', label = paste("Variables"), choices = colnames(rv$raw_data))
    updateSelectInput(session = session, inputId = 'variables_dm', label = paste("Variables"), choices = colnames(rv$raw_data))
    
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
        
        ggplot(melt(rv$raw_data, variable_name = 'Marker'), aes(x = value, fill = Marker)) +
          facet_wrap(~Marker, ncol = ceiling(length(rv$raw_data)/4), scales = 'free') +
          geom_density(alpha = 0.4) +
          theme_classic() +
          xlab('MFI') +
          ylab('Density')
        
        
        
      })
      
      output$pairwise_expression <- renderPlot({ 
        
        GGally::ggpairs(data = rv$raw_data, upper = list(continuous = pairwise_plot), diag = NULL, lower = list(continuous = pairwise_plot))
        
      })
    })
  })
  
  
  ##Dim Reduction Methods----
  
  observeEvent(input$submit_variables_pca, {
    
    rv$clusters <- calculate_clusters(input, output, session, data = rv$raw_data, vars = input$variables_pca, cluster_type = input$clustering_type_pca)
    
    updateSelectInput(session = session, inputId = 'show_clus_pca_heatmap', choices = input$clustering_type_pca, selected = input$clustering_type_pca[1])
    
    rv$pca <- calculate_pca(input, output, session, data = rv$raw_data, dims = input$dimension_num_pca, vars = input$variables_pca, cluster = rv$clusters)
    
  })
  
  observeEvent(input$submit_variables_dm, {
    
    calculate_dm(input, output, session, data = rv$raw_data, dims = input$dimension_num_dm, vars = input$variables_dm)
  })
  
  
  
  observeEvent(input$submit_variables_tsne, {
    
    set.seed(123)
    
    if (input$dimension_num_tsne == '2D') {
      
      tsne <- Rtsne::Rtsne(X = rv$raw_data[,input$variables_tsne, drop = FALSE], dims = 2, verbose = T,
                           perplexity = input$perplexity_tsne, theta = input$theta_tsne, max_iter = input$max_iter_tsne)
      colnames(tsne$Y) <- c('tSNE1', 'tSNE2')
      rv$tsne <- as.data.frame(cbind(rv$raw_data, tsne$Y))
      
      output$tSNE_plot <- renderPlotly({
        
        plot_ly(data = rv$tsne, x = rv$tsne$tSNE1, y = rv$tsne$tSNE2) %>%
          add_markers(color = ~rv$raw_data[,input$colour_col2]) %>%
          layout(dragmode = 'lasso') %>%
          layout(legend = list(title=list(text=''), orientation = 'h'))
        
        
      })
      
    } else if (input$dimension_num_tsne == '3D') {
      
      tsne <- Rtsne::Rtsne(X = rv$raw_data[,input$variables_tsne, drop = FALSE], dims = 3, verbose = T,
                           perplexity = input$perplexity_tsne, theta = input$theta_tsne, max_iter = input$max_iter_tsne)
      colnames(tsne$Y) <- c('tSNE1', 'tSNE2', 'tSNE3')
      rv$tsne <- as.data.frame(cbind(rv$raw_data[,input$variables_tsne, drop = FALSE], tsne$Y))
      
      output$tSNE_plot <- renderPlotly({
        
        plot_ly(data = rv$tsne, x = rv$tsne$tSNE1, y = rv$tsne$tSNE2, z = rv$tsne$tSNE3) %>%
          add_markers(color = ~rv$raw_data[,input$colour_col2]) %>%
          layout(dragmode = 'lasso', legend = F)
        
      })
      
    }
    
    
    if (any(grepl('Rphenograph', input$clustering_type_tsne)) == TRUE) {
      
      rv$phenograph <- Rphenograph(rv$raw_data[,input$variables_tsne], k = 10)
      rv$raw_data['Clusters'] <- as.character(rv$phenograph$membership)
      
    } else if (any(grepl('ClusterX', input$clustering_type_tsne)) == TRUE) {
      
      rv$clusterx <- ClusterX(rv$raw_data[,input$variables, drop = FALSE])
      rv$raw_data['ClusterX_Clusters'] <- as.character(rv$clusterx$cluster)
      
    } else {
      NULL
    }
    
    updateSelectInput(session = session, inputId = 'colour_col2', label = 'Colour by:', choices = colnames(rv$raw_data))
    
    output$tsne_heatmap <- renderPlotly({
      
      heat <- as.data.frame(sapply(rv$raw_data[,input$variables_tsne],normalise))
      heat['Clusters'] <- rv$phenograph$membership
      
      heat <- heat %>%
        group_by(Clusters) %>%
        summarise_all(median) %>% 
        as.data.frame() 
      
      selected <- sapply(rv$raw_data[,input$variables_tsne],normalise)
      
      selected <- selected[which_selected(),] %>%
        as.data.frame() %>%
        summarise_all(median) %>%
        tibble::add_column(Clusters = 'Selected', .before = 1)
      
      heat <- rbind(heat, selected)
      
      plot_ly(heat, x = colnames(heat)[-1], y = as.character(heat$Clusters),z = as.matrix(heat[,-1]), type = 'heatmap',
              colors = colorRamp(c("#4575B4",'#FFFFFF','#D73027'))) %>%
        layout(xaxis = list(type = "category"), 
               yaxis = list(type = "category"))
    })
    
    
    output$download_tsne_data <- downloadHandler(
      
      filename = function() {
        paste('tsne data', ".csv", sep = "")
      },
      content = function(file) {
        write.csv(rv$tsne, file, row.names = FALSE)
      }
      
    )
    
    
  })
  
  
  observeEvent(input$submit_variables_umap, {
    
    set.seed(123)
    
    custom <- umap.defaults
    custom$verbose <- T
    custom$n_neighbors <- input$n_neighbours_umap
    custom$min_dist <- input$min_dist_umap
    
    
    if (input$dimension_num_umap == '2D') {
      
      custom$n_components <- 2
      umap2 <- umap(rv$raw_data[,input$variables_umap, drop = FALSE], config = custom)
      colnames(umap2$layout) <- c('UMAP1', 'UMAP2')
      rv$umap2 <- as.data.frame(cbind(umap2$data, umap2$layout))
      
      output$umap_plot <- renderPlotly({
        plot_ly(data = rv$umap2, x = rv$umap2$UMAP1, y = rv$umap2$UMAP2,
                marker = list(color = ~rv$raw_data[,input$colour_col3], colorscale = c('#FFE1A1', '#683531'))) %>%
          add_markers()
      })
      
    } else if (input$dimension_num_umap == '3D') {
      
      custom$n_components <- 3
      umap3 <- umap(rv$raw_data[,input$variables_umap, drop = FALSE], config = custom)
      colnames(umap3$layout) <- c('UMAP1', 'UMAP2', 'UMAP3')
      rv$umap3 <- as.data.frame(cbind(umap3$data, umap3$layout))
      output$umap_plot <- renderPlotly({
        
        plot_ly(data = rv$umap3, x = rv$umap3$UMAP1, y = rv$umap3$UMAP2, z = rv$umap3$UMAP3,
                marker = list(color = ~rv$raw_data[,input$colour_col3], colorscale = c('#FFE1A1', '#683531'))) %>%
          add_markers()
      })
    }
    
    # if(any(input$clustering_type == 'Rphenograph') == TRUE) {
    #   phenograph(variables = input$variables_umap, K = 30)
    # } else {
    #   NULL
    # }
    
    
  })
  
  
  
  # 
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
