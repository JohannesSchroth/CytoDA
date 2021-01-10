#' Funtion to display popup modal allowing user to edit sample names
#' @param data data frame
#' @return 
#' 

edit_samplenames <- function(data = NULL, input, output, session, ...) {
  
  edit_sample_names_table <- data.frame(Sample = as.character(sampleNames(data)),
                                        Rename = as.character(sampleNames(data)),
                                        stringsAsFactors = FALSE)
  
  output$sample_names <- renderRHandsontable(
    rhandsontable(edit_sample_names_table)
  )
  
  print('2')
  samplename[[2]] <-
    
    modalDialog(
      
      tagList(
        rHandsontableOutput('sample_names',width = '600px')
      ), 
      title='Specify Sample Names:',
      footer = tagList(actionButton('submit_samplenames', 'Submit'),
                       modalButton("Cancel")
      )
    )
  
  return(samplename)
  
}
