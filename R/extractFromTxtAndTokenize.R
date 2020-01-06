################################################################
# extractFromTxtAndTokenize.R
#
#
# A function to extract text from a pdf and tokenize it by word.
################################################################
#' @export
extractFromTxtAndTokenize <- function(filePath, fileName){

  # ##FOR TESTING####
  #   filePath <- "PDFs/preChavez/"
  #   fileName <- "TransBeforeChavez1stGradeUTF8.txt"
  # ##FOR TESTING####

    # ##FOR TESTING####
    # filePath <- "PDFs/preChavez/"
    # fileName <- "TransBeforeChavez2ndGradeUTF8.txt"
    # ##FOR TESTING####


  # ##FOR TESTING####
  # filePath <- "PDFs/preChavez/"
  # fileName <- "TransBeforeChavez5thGradeUTF8.txt"
  # ##FOR TESTING####


  # conbine the filepath and filename
  txtFilePathAndName <- paste(filePath, fileName, sep = "")

  # First read in txt
    txtFile <- read_file(file = txtFilePathAndName, locale = locale("es", decimal_mark = ","))

    txtTextRaw <- tibble(txtFile)


  # Check to make sure there is machine readble text
  if( all(txtTextRaw=="")==TRUE ){
    warning("The file ", fileName, " is not machine readable and/or contains no text.\n It is being skipped.")
  } else {

  # Split the raw data into lines with each page as a seperate element in a list
  txtTextPages <- str_split(string = txtTextRaw, pattern = "PAGE_\\d{1,4}|Page_\\d{1,4}|PAGE\\s\\d{1,4}|Page\\s\\d{1,4}")

  # Remove any pages that had no text on them
  txtTextPages[txtTextPages==""] <- NULL

  # Make each element of the list a dataframe (its a tidyverse "tibble" but that's just a nicer form of dataframe)
  txtTextPagesTibblesInAList <- list()
  for(i in 1:length(txtTextPages)){
    txtTextPagesTibblesInAList[[i]]  <- tibble(document= fileName, pageGroup=i, line=c(1:length(txtTextPages[[i]]) ), text=txtTextPages[[i]] )
  }

  txtTextPagesTibble <- do.call(rbind.data.frame, txtTextPagesTibblesInAList)

  # Now tokenify each word into into it's own row. [Use the to_lower = FALSE, if case matters]
  txtTextTokens <- unnest_tokens(tbl = txtTextPagesTibble, output = word, input = text)

  txtTextTokens
  } # Ends the else
}

