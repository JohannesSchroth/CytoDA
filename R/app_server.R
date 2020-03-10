#' Application Server

packages_1 <- c('gridExtra','grid', 'ggplot2', 'umap', 'dplyr', 'reshape', 'RColorBrewer', 'cytofkit',
                'FlowSOM', 'Rtsne', 'ggdendro', 'plotly', 'shiny', 'plotly', 'shinycssloaders', 'monocle',
                'shinydashboard', 'DT', 'flowCore', 'openCyto', 'ggsci', 'rhandsontable', 'shinyBS')
lapply(packages_1,library, character.only=TRUE)
library(flowCore)

app_server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  observeEvent(input$upload_data, {
    
    rv$raw_data <- flowCore::read.flowSet(files = input$file$datapath)
    
    channels <- pData(parameters(raw_data[[1]]))$name
    antibodies <- pData(parameters(raw_data[[1]]))$desc
    
    if (length(rv$raw_data > 1)) {
      
      set
      
      
      
        
      
    }
    
    
    
    if(input$logicletransform) {
      vars <- flowCore::colnames(rv$raw_data[[1]])[-grep('SSC|FSC|Time|Sample|Group', flowCore::colnames(rv$raw_data[[1]]))]
      rv$raw_data[[i]] <- flowCore::transform(rv$raw_data[[i]], flowCore::estimateLogicle(rv$raw_data[[i]], channels = vars))
    }
    
    
    rv$raw_data <- as.data.frame(flowCore::exprs(rv$raw_data))
    
    if (any(flowCore::colnames(rv$raw_data) == 'SampleID') == TRUE) {
      
      dens <- density(rv$raw_data$SampleID, n = length(rv$raw_data$SampleID))
      samples <- pastecs::turnpoints(ts(dens$y))$pits
      
      for (i in 0:length(dens$x[samples]-1)) {
        rv$raw_data[rv$raw_data$SampleID > 
                      if (i == 0) {1} else {
                        dens$x[samples][i]},'Sample'] <- i+1
      }
      
      
      edit_sample_names_table <- data.frame(Sample = as.character(unique(rv$raw_data$Sample)),
                                            Name = unique(rv$raw_data$Sample))
      
      output$sample_names <- renderRHandsontable(
        rhandsontable(edit_sample_names_table) %>%
          hot_col('Name', strict = F)
      )
      
      
    }
    
    edit_colnames_table <- data.frame(
      Current = colnames(rv$raw_data),
      New = colnames(rv$raw_data),
      Include = TRUE,
      stringsAsFactors = FALSE
    )
    
    output$edit_colnames <- renderRHandsontable(
      rhandsontable(edit_colnames_table) %>%
        hot_col('Include', 'checkbox')
    )
    
    
  })
  
  
  observeEvent(input$upload_data, {
    
    removeModal()
    showModal(modalDialog(
      tagList(
        rHandsontableOutput('edit_colnames',width = '600px')
      ), 
      title='Specify Column Names:',
      footer = tagList(actionButton('submit_colnames', 'Submit'),
                       modalButton("Cancel")
      )
    )
    )
  })
  
  
  observeEvent(input$submit_colnames, {
    
    
    removeModal()
    showModal(modalDialog(
      tagList(
        rHandsontableOutput('sample_names',width = '600px')
      ), 
      title='Specify Sample Names:',
      footer = tagList(actionButton('submit_samplenames', 'Submit'),
                       modalButton("Cancel")
      )
    )
    )
    removeModal()
    
    new_colnames <- as.data.frame(hot_to_r(input$edit_colnames))
    
    colnames(rv$raw_data) <- new_colnames[,2]
    rv$raw_data <- rv$raw_data[,as.logical(as.vector(new_colnames[,3]))]
    
    updateSelectInput(session = session, inputId = 'variables_pca', label = paste("Variables"), choices = colnames(rv$raw_data))
    updateSelectInput(session = session, inputId = 'variables_tsne', label = paste("Variables"), choices = colnames(rv$raw_data))
    updateSelectInput(session = session, inputId = 'variables_umap', label = paste("Variables"), choices = colnames(rv$raw_data))
    
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
    
    updateSelectInput(session = session,inputId = 'colour_col1', label = 'Colour by:', choices = colnames(rv$raw_data))
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
  })
  
  
  observeEvent(input$submit_samplenames, {
    
    removeModal()
    
    new_sample_names <- as.data.frame(hot_to_r(input$sample_names))
    
    for (i in 1:length(new_sample_names[,1])) {
      
      rv$raw_data[as.character(rv$raw_data$Sample) == new_sample_names[i,1], 'Group'] <- as.character(new_sample_names[i,2])
      
    }
    
    
    
  })
  
  
  observeEvent(input$submit_downsample, {
    
    rv$raw_data <- rv$raw_data[sample(nrow(rv$raw_data), input$n_downsample),]
    
    removeModal()
    
  })
  
  
  normalise <- function(data) {
    
    dat <- c()
    
    q <- quantile(data, c(0.01, 0.99))
    
    for (j in 1:length(data)) {
      dat[j] <- (data[j] - q[1])/(q[2]-q[1])
    }
    dat[dat < 0] <- 0
    dat[dat > 1] <- 1
    
    return(as.numeric(dat))
    
  }
  
  
  
  
  ##Dim Reduction Methods----
  
  observeEvent(input$submit_variables_pca, {
    
    pca <- prcomp(as.matrix(rv$raw_data[ ,input$variables_pca]), center = TRUE, scale. = TRUE)
    rv$pca <- cbind(rv$raw_data, as.data.frame(pca$x[,1:3]))
    
    if (input$dimension_num_pca == '2D') { 
      
      output$pca_plot <- renderPlotly({
        
        plot_ly(data = rv$pca, x = rv$pca$PC1, y = rv$pca$PC2,
                marker = list(color = ~rv$raw_data[,input$colour_col1], colorscale = c('#FFE1A1', '#683531'))) %>%
          add_markers()
      })
      
    } else if (input$dimension_num_pca == '3D') {
      output$pca_plot <- renderPlotly({
        
        plot_ly(data = rv$pca, x = rv$pca$PC1, y = rv$pca$PC2, z = rv$pca$PC3,
                marker = list(color = ~rv$raw_data[,input$colour_col1], colorscale = c('#FFE1A1', '#683531'))) %>%
          add_markers()
      })
      
    }
    
    if(any(input$clustering_type == 'Rphenograph') == TRUE) {
      
      phenograph(variables = input$variables_pca, K = 30)
      
    } else {
      NULL
    }
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
      
      selected <- selected[which(),] %>%
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
  
  
  
  
  #   }
  #   
  #   
  #   if (any(grepl('ClusterX', input$clustering_type)) == TRUE) {
  #     
  #     clusterx <- ClusterX(rv$raw_data[,input$variables, drop = FALSE])
  #     rv$clusterx <- as.data.frame(cbind(rv$raw_data, 'ClusterX_Clusters' = clusterx$cluster))
  #     
  #   }
  #   
  #   if (any(grepl('DensVM', input$clustering_type)) == TRUE) {
  #     
  #     
  #     #dens_vm <- DensVM(ydata = as.matrix(data$tSNE1_2D, data$tSNE2_2D), xdata = data[,input$variables])
  #     #data[,'DensVM_Clusters'] <- dens_vm
  #     
  #   }
  #   
  #   
  # })
  
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
  
  
  #if (any(grepl('ClusterX', input$clustering_type)) == TRUE) {
  #  
  #  clusterx <- ClusterX(data[,grep('tSNE', colnames(data)), drop = FALSE])
  #  data <<- as.data.frame(cbind(data, 'ClusterX_Clusters' = clusterx$cluster))
  #  
  #}
  
  # if (any(grepl('DensVM', input$clustering_type)) == TRUE) {
  #
  #
  #   #dens_vm <- DensVM(ydata = as.matrix(data$tSNE1_2D, data$tSNE2_2D), xdata = data[,input$variables])
  #   #data[,'DensVM_Clusters'] <- dens_vm
  #
  # }
  
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
