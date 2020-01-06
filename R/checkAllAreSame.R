################################################################
# checkAllAreSame.R
#
# Creates a keyword list using the words in this file.
# Revised based on Antonios email
################################################################


#' @export
checkAllAreSame <- function(tokenTablesToCheck=list(preChavezPdfTokens, postChavezPdfTokens, keywordMasterList), typeToFilter="word", termsToCheck, typeToCheck="stem"){

  # # # # # # #### FOR TESTING #########
  # typeToFilter="word"
  # typeToCheck="stem"
  # tokenTablesToCheck <-  list(preChavezPdfTokens, postChavezPdfTokens, keywordMasterList)
  # termsToCheck <- c("indígena", "indígenas")
  # # # # # # #### FOR TESTING

filteredTokenListList <- list()

for (i in 1:length(tokenTablesToCheck)){
  filteredTokenListList[[i]] <-  checkTerms(tokenTableToCheck = tokenTablesToCheck[[i]], typeToCheck = typeToFilter, termsToCheck = termsToCheck, printAllOutput = TRUE)
}

combinedFiltered <- bind_rows(filteredTokenListList)

termColumnToCheck <-  select(.data = combinedFiltered, typeToCheck)

uiqueTerms <- unique(termColumnToCheck)


     print( uiqueTerms, n=Inf )
}
