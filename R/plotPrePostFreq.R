################################################################
# plotPrePostFreq.R
#
# A function to create a plot to compare the frequency of words in two sets of frequency counts
# See: https://dzone.com/articles/who-wrote-the-anti-trump-new-york-times-op-ed-usin
################################################################
#' @export
plotPrePostFreq <- function(preChavezTokenFrequency, postChavezTokenFrequency, WordsORStemsORLemmas="Words", isKeywords=FALSE, topNWords = 20){


  # # # #   ##FOR TESTING with Info Generated in Main Script ####
  # # # #
  #  preChavezTokenFrequency <- preChavezWordFrequency
  # #
  #  postChavezTokenFrequency <- postChavezWordFrequency
  # #
  #  WordsORStemsORLemmas="Words"
  #   topNWords <-  20
  #   isKeywords <- FALSE
  #    # # #   ##FOR TESTING with Info Generated in Main Script ####

    # Ungroup tibbles just in case


# Set the correct identifier for the labeling of the graph
    if(isKeywords==TRUE){
      WordsORStemsORLemmas <- paste("Keyword", WordsORStemsORLemmas, sep=" ")
    }

  combinedFreq <- bind_rows(list(preChavez=preChavezTokenFrequency, postChavez=postChavezTokenFrequency), .id="id")

  # Rename the varibiles so they have same name within this function regardless of which
  # types of terms (Words/Stems/Lemmas) are being analyzed.

  internalFunctionVarNames <- c("theDocumentSet", "theTerm", "theFreq")

  names(combinedFreq) <- internalFunctionVarNames

  # Prepare the tf_idf table for plotting
  combinedFreq <- group_by(combinedFreq, theDocumentSet)
  combinedFreq <- arrange(combinedFreq, desc(theFreq))
  combinedFreq <- slice(combinedFreq, 1:topNWords)
  combinedFreq <- ungroup(combinedFreq)
  combinedFreq <- mutate( combinedFreq, theTerm = reorder_within(theTerm, theFreq, theDocumentSet) )


  # Change make theDocumentSet a Factor and set the order of the levels of the factor
  #   This will make the order of the two facets in the plot as desired.
  combinedFreq <- mutate(combinedFreq, theDocumentSet=factor(theDocumentSet, levels = c("preChavez", "postChavez") ) )

outPlot <- NULL
outPlot <- ggplot(combinedFreq, aes(theTerm, theFreq, fill = theDocumentSet))
outPlot <- outPlot + geom_col(show.legend = FALSE)
outPlot <- outPlot + scale_x_reordered()
outPlot <- outPlot + coord_flip()
outPlot <- outPlot + facet_wrap("theDocumentSet", scales = "free_y")
outPlot <- outPlot + labs(x = "",
         y = "",
         title = paste("Frequency for the Top",topNWords,WordsORStemsORLemmas, "for Each Set of Textbooks", sep=" ") )

  outPlot
}

