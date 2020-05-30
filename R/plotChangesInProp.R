################################################################
# plotChangesInProp.R
#
#
# Takes a token table with proportions and does the folowing:
#   Finds the items whose proportions have changed the most.
#   Plots the changes in those proportions.
################################################################
#' @export
plotChangesInProp <- function(preChavezItemProp, postChavezItemProp, WordORStemORLemma="Word", numberOfItems=10, isKeywords=FALSE){

  # #FOR TESTING####
  # preChavezItemProp <- preChavezKeywordLemmaProportion
  # postChavezItemProp <- postChavezKeywordLemmaProportion
  # WordORStemORLemma="Lemma"
  # numberOfItems=10
  # isKeywords=TRUE
  # #FOR TESTING####

  if(isKeywords==TRUE){
    WordORStemORLemma <- paste("Keyword", WordORStemORLemma, sep=" ")
  }

  internalFunctionVarNamesPre <- c("theItem", "preProp")
  internalFunctionVarNamesPost <- c("theItem", "postProp")


  names(preChavezItemProp) <- internalFunctionVarNamesPre
  names(postChavezItemProp) <- internalFunctionVarNamesPost

  combinedProps <- full_join(preChavezItemProp, postChavezItemProp)

  combinedProps <- replace_na(combinedProps, list(preProp = 0,  postProp = 0) )


  combinedProps <- mutate(combinedProps, propChange = postProp - preProp)

  combinedProps <- arrange(combinedProps, desc(propChange) )

  topNIncreases <- slice(combinedProps, 1:numberOfItems)

  topNIncreases <- select(topNIncreases, theItem, propChange)

  combinedProps <- arrange(combinedProps, propChange)

  topNDecreases <- slice(combinedProps, 1:numberOfItems)

  topNDecreases <- select(topNDecreases, theItem, propChange)

  increasesDecreasesCombined <- bind_rows(topNIncreases, topNDecreases)

#  increasesDecreasesCombined <- mutate(increasesDecreasesCombined, theItem = reorder(theItem, propChange))

  increasesDecreasesCombined <- arrange(increasesDecreasesCombined, desc(propChange) )

  customPal <- c("firebrick4", "forestgreen")

  outPlot <- ggplot(increasesDecreasesCombined)
  outPlot <- outPlot + aes(x = reorder(theItem, propChange), y = propChange, fill = propChange > 0)
  outPlot <- outPlot + geom_col(show.legend = FALSE)
  outPlot <- outPlot + coord_flip()
  outPlot <- outPlot + ylab( paste("Top", numberOfItems, "Positive and Top", numberOfItems,"Negative Changes in",WordORStemORLemma,"Proportion", sep=" ") )
  outPlot <- outPlot + xlab("")
  outPlot <- outPlot + theme(axis.text.x = element_text(angle=45, hjust=1))
  outPlot <- outPlot + scale_fill_manual(values = customPal)
  outPlot <- outPlot + labs(x = "", y = "", title = paste("Top", numberOfItems, "Positive and Top", numberOfItems,"Negative Changes in",WordORStemORLemma,"Proportion", sep=" ") )
  outPlot
}


