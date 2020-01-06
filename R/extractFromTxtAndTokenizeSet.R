################################################################
# extractFromTxtAndTokenizeSet.R
#
# A function to extract text from a pdf and tokenize it by word.
################################################################
#' @export
extractFromTxtAndTokenizeSet <- function(filePath, txtSetName){

  # ##FOR TESTING####
  # filePath <- "PDFs/preChavez/"
  # txtSetName <- "preChavez"
  # ##FOR TESTING####


  # Get the filenames for the two sets of textbooks
  txtNames <- list.files(path = filePath)

  # Extract and tokenize the prechavez PDFs into a list
  txtTokens <- list()
  for(i in 1:length(txtNames)){
    cat("Extracting from", txtNames[i],"\n")
    txtTokens[[i]]  <- extractFromTxtAndTokenize(filePath = filePath, fileName = txtNames[i])
  }

  # Combine the list of tokenized tibbles into one dataframe
  txtTokens <- do.call(rbind.data.frame, txtTokens)

  # Add the pdfSet name to the tibble

  txtTokens$setName <- txtSetName

  txtTokens

}
