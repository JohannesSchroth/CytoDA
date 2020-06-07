#' Application User Interface
#' 
#' 
#' 
app_ui <- function(request) {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPage(inverse= TRUE, 'CytoDA', 
               
               
               ##Data Formatting Panel----
               tabPanel('Data Import & Formatting',
                        
                        sidebarLayout(
                          
                          sidebarPanel = sidebarPanel(
                            
                            #File input
                            fileInput("file", label = h3("File input"), multiple = T, accept = '.fcs'),
                            
                            fluidRow(column(5, verbatimTextOutput("file"))),
                            
                            selectInput('transformation_method', 'Select Transformation Method', c('Logicle', 'Biexponential'), multiple = F),
                            
                            actionButton('upload_data', 'Upload')),
                          
                          mainPanel = mainPanel(
                            tabsetPanel(
                              tabPanel('Summary Table', column(DT::dataTableOutput('summary_table'), width = 6)),
                              tabPanel('Marker Expression', plotOutput('marker_expression') %>%
                                         shinycssloaders::withSpinner()),
                              tabPanel('Pairwise Expression', plotOutput('pairwise_expression', height = '800px')%>%
                                         shinycssloaders::withSpinner())
                            )
                          )
                          
                        )
               ),
               
               tabPanel('Clustering',
                        
                        tabsetPanel(
                          
                          tabPanel('Clustering',
                                   
                                   sidebarLayout(
                                     sidebarPanel = sidebarPanel(
                                       
                                       selectInput(inputId = 'variables_clustering', label = 'Variables', choices = c(), multiple = T),
                                       selectInput('clustering_type', 'Select Clustering Algorithm', c('Rphenograph', 'ClusterX', 'ConsensusClusterPlus'), multiple = T),
                                       actionButton('run_clustering', 'Run')
                                     ),
                                     
                                     mainPanel = mainPanel(
                                       tabPanel('Initial Clustering', column(DT::dataTableOutput('clustering_table'), width = 6))
                                     ))
                          ),
                          tabPanel('Merge Clusters',
                                   
                                   sidebarLayout(
                                     
                                     sidebarPanel = sidebarPanel(
                                       
                                       selectInput('merge_which', label = 'Choose which clusters to merge:', choices = c(), multiple = F),
                                       
                                       actionButton('add_clus',label = 'Add Group'),
                                       
                                       actionButton('delete_clus', 'Delete'),
                                       
                                       div(id='cluster_placeholder'),
                                       
                                       actionButton('submit_merge_clus', 'Submit'),
                                       
                                       verbatimTextOutput('which'),
                                       
                                     ),
                                     mainPanel = mainPanel(
                                       plotlyOutput('clustering_heatmap',height = '800px', width = '1000px')
                                     )
                                   )
                                   
                          )
                          
                        )
               ),
               
               ##Dimensionality Reduction Panel---
               tabPanel('Dimensionality Reduction',
                        
                        tabsetPanel(
                          
                          
                          tabPanel('PCA',
                                   
                                   sidebarLayout(
                                     
                                     sidebarPanel = sidebarPanel(
                                       
                                       selectInput(inputId = 'variables_pca', label = 'Variables', choices = c(), multiple = T),
                                       
                                       selectInput('dimension_num_pca', 'Number of Dimensions', c('2D', '3D')),
                                       
                                       selectInput('clustering_type_pca', 'Select Clustering Algorithm', c('Rphenograph', 'ClusterX', 'DensVM'), multiple = T),
                                       
                                       actionButton('submit_variables_pca', 'Run'),
                                       
                                       width = 3),
                                     
                                     mainPanel = mainPanel(
                                       
                                       column(6,
                                              plotlyOutput('pca_plot', height = '800px', width = '640px') %>% 
                                                shinycssloaders::withSpinner()),
                                       column(6,
                                              plotlyOutput('pca_heatmap', height = '800px', width = '640px')),
                                       hr(),
                                       fluidRow(
                                         selectInput('colour_col1', label = 'Colour by:', c()),
                                         downloadButton('download_tsne_data', 'Download tSNE Data'),
                                         selectInput(inputId = 'show_clus_pca_heatmap', 'Clustering to Display', c())
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
                                                plotlyOutput('tsne_plot', height = '800px', width = '640px') %>% 
                                                  shinycssloaders::withSpinner()),
                                         column(6,
                                                plotlyOutput('tsne_heatmap',height = '800px', width = '640px')),
                                         
                                         hr(),
                                         fluidRow(
                                           selectInput('colour_col2', label = 'Colour by:', c()), 
                                           downloadButton('download_pca_data', 'Download tSNE Data')
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
                                       column(6,
                                              plotlyOutput('umap_plot', height = '800px', width = '640px') %>% 
                                                shinycssloaders::withSpinner()),
                                       column(6,
                                              plotlyOutput('umap_heatmap', height = '800px', width = '640px')),
                                       hr(),
                                       fluidRow(
                                         selectInput('colour_col3', label = 'Colour by:', c()))
                                     )
                                   )
                          ),
                          tabPanel('Diffusion Map',
                                   
                                   sidebarLayout(
                                     
                                     sidebarPanel = sidebarPanel(
                                       
                                       selectInput(inputId = 'variables_dm', label = 'Variables', choices = c(), multiple = T),
                                       
                                       selectInput('dimension_num_dm', 'Number of Dimensions', c('2D', '3D')),
                                       
                                       selectInput('clustering_type_dm', 'Select Clustering Algorithm', c('Rphenograph', 'ClusterX', 'DensVM'), multiple = T),
                                       
                                       actionButton('submit_variables_dm', 'Run'),
                                       
                                       width = 3),
                                     
                                     mainPanel = mainPanel(
                                       column(6,
                                              plotlyOutput('dm_plot', height = '500px', width = '500px') %>% 
                                                shinycssloaders::withSpinner()),
                                       column(6,
                                              plotlyOutput('pseudotime_plot', height = '500px', width = '500px') %>% 
                                                shinycssloaders::withSpinner()),
                                       # column(6,
                                       #        plotlyOutput('heatmap',height = '500px', width = '500px')),
                                       hr(),
                                       fluidRow(
                                         selectInput('colour_col_dm', label = 'Colour by:', c()))
                                     )
                                   )
                          )
                        )
               ),
               
               tabPanel('Pseudotime',
                        fluidPage(
                          fluidRow(
                            plotlyOutput('piuhiussseudotime_plot', height = '500px', width = '500px') %>%
                              shinycssloaders::withSpinner()),
                          hr(),
                          fluidRow(
                            
                          )
                        )
               ),
               
               tabPanel('Download', 
                        fluidPage(
                          bookmarkButton()
                        )),
               
               ##Summary Panel----
               tabPanel('Summary of Project', 
                        fluidPage(h1('Cytometry Analysis Tools'),
                                  br(),
                                  p(strong('This app was developed to assist in the analysis of single cell data.'))
                        )
               )
    )
  )
}


