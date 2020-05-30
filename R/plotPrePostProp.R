################################################################
# plotPrePostProp.R
#
# A function to create a plot to compare the frequency of words in two sets of frequency counts
# See: https://dzone.com/articles/who-wrote-the-anti-trump-new-york-times-op-ed-usin
################################################################
#' @export
plotPrePostProp <- function(preChavezTokenProp, postChavezTokenProp, WordsORStemsORLemmas="Words", isKeywords=FALSE, topNWords = 20){


  # # # #   ##FOR TESTING with Info Generated in Main Script ####
  # # # #
  # preChavezTokenProp <- preChavezWordProportion
  # #
  # postChavezTokenProp <- postChavezWordProportion
  # #
  #  WordsORStemsORLemmas="Words"
  #   topNWords <-  20
  #   isKeywords <- FALSE
  #    # # #   ##FOR TESTING with Info Generated in Main Script ####

# Set the correct identifier for the labeling of the graph
    if(isKeywords==TRUE){
      WordsORStemsORLemmas <- paste("Keyword", WordsORStemsORLemmas, sep=" ")
    }

  combinedProp <- bind_rows(list(preChavez=preChavezTokenProp, postChavez=postChavezTokenProp), .id="id")

  # Rename the varibiles so they have same name within this function regardless of which
  # types of terms (Words/Stems/Lemmas) are being analyzed.

  internalFunctionVarNames <- c("theDocumentSet", "theTerm", "theProp")

  names(combinedProp) <- internalFunctionVarNames

  # Prepare the tf_idf table for plotting
  combinedProp <- group_by(combinedProp, theDocumentSet)
  combinedProp <- arrange(combinedProp, desc(theProp))
  combinedProp <- slice(combinedProp, 1:topNWords)
  # word_tf_idf <- top_n(word_tf_idf, topNWords, tf_idf)
  combinedProp <- ungroup(combinedProp)
  combinedProp <- mutate( combinedProp, theTerm = reorder_within(theTerm, theProp, theDocumentSet) )


  # Change make theDocumentSet a Factor and set the order of the levels of the factor
  #   This will make the order of the two facets in the plot as desired.
  combinedProp <- mutate(combinedProp, theDocumentSet=factor(theDocumentSet, levels = c("preChavez", "postChavez") ) )

outPlot <- NULL
outPlot <- ggplot(combinedProp, aes(theTerm, theProp, fill = theDocumentSet))
outPlot <- outPlot + geom_col(show.legend = FALSE)
outPlot <- outPlot + scale_x_reordered()
outPlot <- outPlot + coord_flip()
outPlot <- outPlot + theme(axis.text.x = element_text(angle=45, hjust=1))
outPlot <- outPlot + facet_wrap("theDocumentSet", scales = "free_y")
outPlot <- outPlot + labs(x = "",
         y = "",
         title = paste("Proportions for the Top",topNWords,WordsORStemsORLemmas, "\nfor Each Set of Textbooks", sep=" ") )

  outPlot
}

