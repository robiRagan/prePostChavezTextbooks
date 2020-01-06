################################################################
# updateStems.R
#
#
# Updates the stems in a list with custom stems
################################################################
#' @export
updateStems <- function(tokenTableList, newStemList){

  # #FOR TESTING####
  # tokenTableList <- preChavezPdfTokens
  # newStemList <- newStems
  # #FOR TESTING####



  # #Diagnostic
  # print( filter(postChavezPdfTokens, word=="injusticia" | word=="injusticias"), n=Inf )


   tokenTableList <- left_join(tokenTableList, newStemList, by=c("word"="word"))


  # #Diagnostic
  # print( filter(tokenTableList, word=="injusticia" | word=="injusticias"), n=Inf )


  tokenTableList <- rename(tokenTableList, "stem" = "stem.y")

   tokenTableList <- rename(tokenTableList, "oldStem" = "stem.x")

   # #Diagnostic
   # print( filter(tokenTableList, word=="injusticia" | word=="injusticias"), n=Inf )


   tokenTableList$stem <- if_else( is.na(tokenTableList$stem), tokenTableList$oldStem, tokenTableList$stem)


   # #Diagnostic
   # print( filter(tokenTableList, word=="injusticia" | word=="injusticias"), n=Inf )



  ## Diagnostics ##
    tokenTableListChangedStems <- filter(tokenTableList, oldStem != stem)

     tokenTableListSameStems <- filter(tokenTableList, oldStem == stem)

    length(tokenTableList$stem) == length(tokenTableListChangedStems$stem) + length(tokenTableListSameStems$stem) # Should be TRUE




  tokenTableList <- select(tokenTableList, -oldStem)

  message( paste(length(tokenTableListChangedStems$stem),"stems have been updated") )


  #Diagnostic
  # print( filter(tokenTableList, word=="injusticia" | word=="injusticias"), n=Inf )


  tokenTableList

}


