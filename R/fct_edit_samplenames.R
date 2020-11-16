#' Funtion to display popup modal allowing user to edit sample names
#' @param data data frame
#' @return 
#' 

edit_samplenames <- function(data = NULL, input, output, session, ...) {
  
  if (any(flowCore::colnames(data) == 'SampleID') == TRUE) {
    
    dens <- density(data$SampleID, n = length(data$SampleID))
    turn <- pastecs::turnpoints(ts(dens$y))$pits
    
    s <- as.integer(diff(c(0, which(turn), length(data$SampleID))))
    
    samplename <- list()
    
    samples <- unlist(lapply('Sample', paste, 1:length(s)))
    
    samplename[[1]] <- rep(x = samples, times = s)
    
    edit_sample_names_table <- data.frame(Sample = as.character(unique(samplename[[1]])),
                                          Name = as.character(unique(samplename[[1]])),
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
    
  } else {
    NULL
  }
}
