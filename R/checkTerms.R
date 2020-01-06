################################################################
# checkTerms.R
#
# Creates a keyword list using the words in this file.
# Revised based on Antonios email
################################################################


#' @export
checkTerms <- function(tokenTableToCheck, typeToCheck, termsToCheck, printAllOutput=FALSE){

  # # # # # # #### FOR TESTING #########
  # typeToCheck="word"
  # tokenTableToCheck <-  postChavezPdfTokens
  #    termsToCheck <- c("indígena", "indígenas")
  # # # # # # #### FOR TESTING

  # Exract the pdfSetNames from the fielpaths

orList <- glue(' {typeToCheck}=="{termsToCheck}" ')

orList <- glue_collapse(orList, "|", last = "")

expressionToEval <- parse(text = glue(' filter(tokenTableToCheck,{orList}) ') )

termsOut <- eval(expressionToEval)

if(printAllOutput==TRUE){
    print( termsOut, n=Inf )
  }

termsOut


}
