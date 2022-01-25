#' Application User Interface
#' 

packages_1 <- c('gridExtra','grid', 'ggplot2', 'cytofkit2', 'umap', 'dplyr', 'reshape', 'RColorBrewer',
                'FlowSOM', 'Rtsne', 'ggdendro', 'plotly', 'shiny', 'plotly', 'shinycssloaders', 'monocle',
                'shinydashboard', 'DT', 'flowCore', 'openCyto', 'ggsci', 'rhandsontable', 'shinyBS', 'sortable')

lapply(packages_1, require, character.only=TRUE)

app_ui <- function(request) {
  
  tagList(
    
    golem_add_external_resources(),
    
    navbarPage(inverse= TRUE, 'CytoDA', 
               
               
               ## ---------------------------- Data Formatting Panel ----------------------------
               tabPanel('Data Import & Formatting',
                        
                        sidebarLayout(
                          
                          sidebarPanel = sidebarPanel(
                            
                            h3('Run Using Demo Data'),
                            checkboxInput('demo_data', 'Human Bone Marrow Cells - Levine et al. 2015 (asinh tranformed)', FALSE),
                            
                            #File input
                            fileInput("file", label = h3("File input"), multiple = T, accept = '.fcs'),
                            
                            fluidRow(column(5, verbatimTextOutput("file"))),
                            
                            selectInput('transformation_method', 'Select Transformation Method', c('Logicle', 'Biexponential'), multiple = F),
                            
                            actionButton('upload_data', 'Run')
                          ),
                          
                          mainPanel = mainPanel(
                            tabsetPanel(
                              tabPanel('Marker Expression', plotlyOutput('marker_expression', height = '800px', width = '1200px') %>%
                                         shinycssloaders::withSpinner()),
                              tabPanel('Pairwise Expression', plotOutput('pairwise_expression', height = '800px') %>%
                                         shinycssloaders::withSpinner()),
                              tabPanel('Summary Table', DT::dataTableOutput('summary_table', width = '80%'))
                            )
                          )
                          
                        )
               ),
               
               
               ## ---------------------------- Clustering Panel ----------------------------
               tabPanel('Clustering',
                        
                        sidebarLayout(
                          sidebarPanel = sidebarPanel(
                            
                            selectInput(inputId = 'variables_clustering', label = 'Variables', choices = c(), multiple = T),
                            selectInput('clustering_type', 'Select Clustering Algorithm', c('Rphenograph'), selected = 'Rphenograph', multiple = F),
                            actionButton('run_clustering', 'Run')
                          ),
                          
                          mainPanel = mainPanel(
                            
                            tabsetPanel(
                              
                              tabPanel('Heatmap',
                                       column(plotlyOutput('cluster_heatmap',height = '800px', width = '1000px'), width = 12)
                              ),
                              
                              tabPanel('Cluster Frequencies',
                                       column(plotlyOutput('cluster_pie', height = '640px', width = '1000px'), width = 12),
                                       hr())
                              
                            )
                            
                          )
                        )
               ),
               
               ## ---------------------------- Dimensionality Reduction Panel ----------------------------
               tabPanel('Dimensionality Reduction',
                        
                        tabsetPanel(
                          
                          
                          tabPanel('PCA',
                                   
                                   sidebarLayout(
                                     
                                     sidebarPanel = sidebarPanel(
                                       
                                       selectInput(inputId = 'variables_pca', label = 'Variables', choices = c(), multiple = T),
                                       
                                       selectInput('dimension_num_pca', 'Number of Dimensions', c('2D', '3D')),
                                       
                                       actionButton('submit_variables_pca', 'Run'),
                                       
                                       width = 3),
                                     
                                     mainPanel = mainPanel(
                                       
                                       column(6,
                                              plotlyOutput('pca_plot', height = '640px', width = '640px')),
                                       column(6,
                                              plotlyOutput('pca_heatmap', height = '740px', width = '740px')),
                                       hr(),
                                       fluidRow(
                                         selectInput('colour_col1', label = 'Colour by:', c()),
                                         downloadButton('download_pca_data', 'Download PCA Data')
                                       )
                                     )
                                   )
                                   
                                   
                                   
                          ),
                          
                          
                          tabPanel('tSNE', 
                                   
                                   sidebarLayout(
                                     
                                     sidebarPanel = sidebarPanel(
                                       
                                       selectInput(inputId = 'variables_tsne', label = 'Variables', choices = c(), multiple = T),
                                       
                                       selectInput('dimension_num_tsne', 'Number of Dimensions', c('2D', '3D')),
                                       
                                       sliderInput('perplexity_tsne', 'Perplexity', 0,100,30),
                                       
                                       sliderInput('theta_tsne', 'Theta', 0,1,0.5),
                                       
                                       sliderInput('max_iter_tsne', 'Number of Iterations', 100,10000,1000),
                                       
                                       actionButton('submit_variables_tsne', 'Run'),
                                       
                                       width = 3),
                                     
                                     mainPanel = mainPanel(
                                       
                                       fluidRow(
                                         column(6,
                                                plotlyOutput('tsne_plot', height = '640px', width = '640px')),
                                         column(6,
                                                plotlyOutput('tsne_heatmap',height = '740px', width = '740px')),
                                         
                                         hr(),
                                         fluidRow(
                                           selectInput('colour_col2', label = 'Colour by:', c()), 
                                           downloadButton('download_tsne_data', 'Download tSNE Data')
                                         )
                                       )
                                     )
                                   )),
                          
                          tabPanel('UMAP',
                                   
                                   sidebarLayout(
                                     
                                     sidebarPanel = sidebarPanel(
                                       
                                       selectInput(inputId = 'variables_umap', label = 'Variables', choices = c(), multiple = T),
                                       
                                       selectInput('dimension_num_umap', 'Number of Dimensions', c('2D', '3D')),
                                       
                                       sliderInput('n_neighbours_umap', 'Number of neighbours', min = 2, max = 200, value = 50),
                                       
                                       sliderInput('min_dist_umap', 'Minimum Distance', 0.01,0.99,0.5),
                                       
                                       actionButton('submit_variables_umap', 'Run'),
                                       
                                       width = 3),
                                     
                                     mainPanel = mainPanel(
                                       fluidRow(
                                         column(6,
                                                plotlyOutput('umap_plot', height = '640px', width = '640px')),
                                         column(6,
                                                plotlyOutput('umap_heatmap',height = '740px', width = '740px')),
                                         
                                         hr(),
                                         fluidRow(
                                           selectInput('colour_col3', label = 'Colour by:', c()), 
                                           downloadButton('download_umap_data', 'Download UMAP Data')
                                         )
                                       )
                                     )
                                   )
                          )
                        )
               )
    )
  )
}


