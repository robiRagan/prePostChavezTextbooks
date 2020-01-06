################################################################
# extractFromPdfAndTokenize.R
#
#
# A function to extract text from a pdf and tokenize it by word.
################################################################
#' @export
extractFromPdfAndTokenize <- function(filePath, fileName){

  ##FOR TESTING####
    # filePath <- "PDFs/postChavez/"
    # fileName <- "sociales2_2013.pdf"
  ##FOR TESTING####




  # conbine the filepath and filename
  pdfFilePathAndName <- paste(filePath, fileName, sep = "")

  # First read in pdf
  pdfTextRaw <- pdf_text(pdfFilePathAndName)


  # Check to make sure there is machine readble text
  if( all(pdfTextRaw=="")==TRUE ){
    warning("The file ", fileName, " is not machine readable and/or contains no text.\n It is being skipped.")
  } else {

  # Split the raw data into lines with each page as a seperate element in a list
  pdfTextPages <- str_split(string = pdfTextRaw, pattern = "\n")

  # Remove any pages that had no text on them
  pdfTextPages[pdfTextPages==""] <- NULL

  # Make each element of the list a dataframe (its a tidyverse "tibble" but that's just a nicer form of dataframe)
  pdfTextPagesTibblesInAList <- list()
  for(i in 1:length(pdfTextPages)){
    pdfTextPagesTibblesInAList[[i]]  <- tibble(document= fileName, pageGroup=i, line=c(1:length(pdfTextPages[[i]]) ), text=pdfTextPages[[i]] )
  }

  pdfTextPagesTibble <- do.call(rbind.data.frame, pdfTextPagesTibblesInAList)

  # Now tokenify each word into into it's own row. [Use the to_lower = FALSE, if case matters]
  pdfTextTokens <- unnest_tokens(tbl = pdfTextPagesTibble, output = word, input = text)

  pdfTextTokens
  } # Ends the else
}

