#' Add external Resources to the Application
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', app_sys('app/www')
  )
  
  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys('app/www'),
      app_title = 'CytoDA'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}
