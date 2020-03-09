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
               navbarMenu('Data Import & Formatting',
                          
                          tabPanel('Summary Table', 
                                   
                                   sidebarLayout(
                                     
                                     sidebarPanel = sidebarPanel(
                                       
                                       #File input
                                       fileInput("file", label = h3("File input"), multiple = T, accept = '.fcs'),
                                       
                                       fluidRow(column(5, verbatimTextOutput("file"))),
                                       
                                       checkboxInput('logicletransform', 'Transform data', TRUE),
                                       
                                       actionButton('upload_data', 'Upload')),
                                     
                                     mainPanel = mainPanel(
                                       column(DT::dataTableOutput('summary_table'), width = 6)
                                     )
                                   )),
                          
                          tabPanel('Marker Expression', plotOutput('marker_expression')
                          )
               ),
               
               
               ##Dimensionality Reduction Panel---
               navbarMenu('Dimensionality Reduction',
                          
                          tabPanel('PCA',
                                   
                                   sidebarLayout(
                                     
                                     sidebarPanel = sidebarPanel(
                                       
                                       selectInput(inputId = 'variables_pca', label = 'Variables', choices = c(), multiple = T),
                                       
                                       selectInput('dimension_num_pca', 'Number of Dimensions', c('2D', '3D')),
                                       
                                       selectInput('clustering_type', 'Select Clustering Algorithm', c('Rphenograph', 'ClusterX', 'DensVM'), multiple = T),
                                       
                                       actionButton('submit_variables_pca', 'Run'),
                                       
                                       width = 3),
                                     
                                     mainPanel = mainPanel(
                                       
                                       column(6,
                                              plotlyOutput('pca_plot', height = '500px', width = '500px') %>% 
                                                shinycssloaders::withSpinner()),
                                       column(6,
                                              plotlyOutput('heatmap',height = '500px', width = '500px')),
                                       hr(),
                                       fluidRow(
                                         selectInput('colour_col1', label = 'Colour by:', c()))
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
                                       
                                       selectInput('clustering_type_tsne', 'Select Clustering Algorithm', c('Rphenograph', 'ClusterX', 'DensVM'), multiple = T),
                                       
                                       actionButton('submit_variables_tsne', 'Run'),
                                       
                                       width = 3),
                                     
                                     mainPanel = mainPanel(
                                       
                                       fluidRow(
                                         column(6,
                                                plotlyOutput('tSNE_plot', height = '800px', width = '640px') %>% 
                                                  shinycssloaders::withSpinner()),
                                         column(6,
                                                plotlyOutput('tsne_heatmap',height = '800px', width = '640px')),
                                         
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
                                       
                                       selectInput('clustering_type', 'Select Clustering Algorithm', c('Rphenograph', 'ClusterX', 'DensVM'), multiple = T),
                                       
                                       actionButton('submit_variables_umap', 'Run'),
                                       
                                       width = 3),
                                     
                                     mainPanel = mainPanel(
                                       column(6,
                                              plotlyOutput('umap_plot', height = '500px', width = '500px') %>% 
                                                shinycssloaders::withSpinner()),
                                       # column(6,
                                       #        plotlyOutput('heatmap',height = '500px', width = '500px')),
                                       hr(),
                                       fluidRow(
                                         selectInput('colour_col3', label = 'Colour by:', c()))
                                     )
                                   )
                          )
               ),
               
               tabPanel('Pseudotime',
                        fluidPage(
                          fluidRow(
                            plotlyOutput('pseudotime_plot', height = '500px', width = '500px') %>%
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



#' Add external Resources to the Application
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'CytoDA'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

