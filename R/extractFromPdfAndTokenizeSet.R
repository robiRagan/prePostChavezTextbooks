################################################################
# extractFromPdfAndTokenizeSet.R
#
# A function to extract text from a pdf and tokenize it by word.
################################################################
#' @export
extractFromPdfAndTokenizeSet <- function(filePath, pdfSetName){

  # ##FOR TESTING####
     # filePath <- "PDFs/postChavez/"
     # pdfSetName <- "postChavez"
  # ##FOR TESTING####


  # Get the filenames for the two sets of textbooks
  pdfNames <- list.files(path = filePath)

  # Extract and tokenize the prechavez PDFs into a list
  pdfTokens <- list()
  for(i in 1:length(pdfNames)){
    cat("Extracting from", pdfNames[i],"\n")
    pdfTokens[[i]]  <- extractFromPdfAndTokenize(filePath = filePath, fileName = pdfNames[i])
  }

  # Combine the list of tokenized tibbles into one dataframe
  pdfTokens <- do.call(rbind.data.frame, pdfTokens)

  # Add the pdfSet name to the tibble

  pdfTokens$setName <- pdfSetName

  pdfTokens

}
