#' Funtion to display popup modal allowing user to edit sample names
#' @param data data frame
#' @return 
#' 

edit_samplenames <- function(data = NULL, input, output, session) {
  
  if (any(flowCore::colnames(data) == 'SampleID') == TRUE) {
    
    dens <- density(data$SampleID, n = length(data$SampleID))
    s <- pastecs::turnpoints(ts(dens$y))$pits
    
    s <- c(0, unique(s), length(s))
    
    sample <- sapply(1:length(s), function(i) rep(paste('Sample',i),s[i])) %>%
      unlist()
    
    data$SampleID <- sample[1:length(data$SampleID)]
    
    edit_sample_names_table <- data.frame(Sample = as.character(unique(data$SampleID)),
                                          Name = unique(data$SampleID))
    
    output$sample_names <- renderRHandsontable(
      rhandsontable(edit_sample_names_table) %>%
        hot_col('Name', strict = F)
    )
    
    
    samplename_modal <-
      
      modalDialog(
        
        tagList(
          rHandsontableOutput('sample_names',width = '600px')
        ), 
        title='Specify Sample Names:',
        footer = tagList(actionButton('submit_samplenames', 'Submit'),
                         modalButton("Cancel")
        )
      )
    
    
    return(samplename_modal)
    
  }
}