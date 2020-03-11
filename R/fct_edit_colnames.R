#' Function to edit data frame column names
#' @param data data frame
#' @return a modal dialogue to edit colnames
#' 


edit_colnames <- function(data = NULL, fs = NULL, input, output, session) {
  
  channels <- pData(flowCore::parameters(fs[[1]]))$name
  
  antibodies <- pData(flowCore::parameters(fs[[1]]))$desc
  
  suggested_colnames <- sapply(1:length(antibodies), function(i){
    if(is.na(antibodies[i])|antibodies[i]=="NA"){return(channels[i])}else{antibodies[i]}
  })
  
  edit_colnames_table <- data.frame(
    Current = channels,
    New = suggested_colnames,
    Include = TRUE,
    stringsAsFactors = FALSE
  )
  
  output$edit_colnames <- renderRHandsontable(
    rhandsontable(edit_colnames_table) %>%
      hot_col('Include', 'checkbox')
  )
  
  colname_modal <- modalDialog(
       tagList(
         rHandsontableOutput('edit_colnames',width = '600px')
       ), 
       title='Specify Column Names:',
       footer = tagList(actionButton('submit_colnames', 'Submit'),
                        modalButton("Cancel")
       )
     )
  
  return(colname_modal)
  
}