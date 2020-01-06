################################################################
# plotTFIDF.R
#
# A function to create a plot to compare the frequency of words in two sets of frequency counts
# See: https://dzone.com/articles/who-wrote-the-anti-trump-new-york-times-op-ed-usin
################################################################
#' @export
plotTFIDF <- function(preChavezTokenFrequency, postChavezTokenFrequency, WordsORStemsORLemmas="Words", isKeywords=FALSE, topNWords = 20){


  # # # #   ##FOR TESTING with Info Generated in Main Script ####
  # # # #
  #  preChavezTokenFrequency <- preChavezKeywordLemmaFrequency
  # #
  #  postChavezTokenFrequency <- postChavezKeywordLemmaFrequency
  # #
  #  WordsORStemsORLemmas="Lemmas"
  #   topNWords <-  20
  #   isKeywords <- TRUE
  #    # # #   ##FOR TESTING with Info Generated in Main Script ####

# Set the correct identifier for the labeling of the graph
    if(isKeywords==TRUE){
      WordsORStemsORLemmas <- paste("Keyword", WordsORStemsORLemmas, sep=" ")
    }

  combinedFreq <- bind_rows(list(preChavez=preChavezTokenFrequency, postChavez=postChavezTokenFrequency), .id="id")

  # Rename the varibiles so they have same name within this function regardless of which
  # types of terms (Words/Stems/Lemmas) are being analyzed.

  internalFunctionVarNames <- c("theDocumentSet", "theTerm", "theFreq")

  names(combinedFreq) <- internalFunctionVarNames



  # Create the TF-IDF
  word_tf_idf <- bind_tf_idf(combinedFreq, theTerm, theDocumentSet, theFreq)
  word_tf_idf <- arrange(word_tf_idf, desc(tf_idf))


  # Remove any rows where tf_idf == 0
  word_tf_idf <- filter(word_tf_idf, tf_idf!=0)

# Prepare the tf_idf table for plotting
  word_tf_idf <- group_by(word_tf_idf, theDocumentSet)
  word_tf_idf <- slice(word_tf_idf, 1:topNWords)
 # word_tf_idf <- top_n(word_tf_idf, topNWords, tf_idf)
  word_tf_idf <- ungroup(word_tf_idf)
  word_tf_idf <- mutate( word_tf_idf, theTerm = reorder_within(theTerm, tf_idf, theDocumentSet) )

  # Change make theDocumentSet a Factor and set the order of the levels of the factor
  #   This will make the order of the two facets in the plot as desired.
   word_tf_idf <- mutate(word_tf_idf, theDocumentSet=factor(theDocumentSet, levels = c("preChavez", "postChavez") ) )

outPlot <- NULL
outPlot <- ggplot(word_tf_idf, aes(theTerm, tf_idf, fill = theDocumentSet))
outPlot <- outPlot + geom_col(show.legend = FALSE)
outPlot <- outPlot + scale_x_reordered()
outPlot <- outPlot + coord_flip()
outPlot <- outPlot + facet_wrap("theDocumentSet", scales = "free_y")
outPlot <- outPlot + labs(x = "",
         y = paste("TF-IDF for These", WordsORStemsORLemmas, "for Each Set of Textbooks", sep=" "),
         title = paste("\"Signature\"", WordsORStemsORLemmas, "for the Pre- and Post- Chavez Textbook Sets.") )

  outPlot
}



# outPlot <-  word_tf_idf %>%
#   #  filter(screen_name %in% selected) %>%
#   group_by(theDocumentSet) %>%
#   top_n(topNWords, tf_idf) %>%
#   ungroup() %>%
#   mutate(theTerm = reorder_within(theTerm, tf_idf, theDocumentSet)) %>%
#   ggplot(aes(theTerm, tf_idf, fill = theDocumentSet)) +
#   geom_col(show.legend = FALSE) +
#   scale_x_reordered() +
#   coord_flip() +
#   facet_wrap(~ theDocumentSet, scales = "free_y") +
#   labs(x = "",
#        y = paste("TF-IDF for These", WordsORStemsORLemmas, "for Each Set of Textbooks", sep=" "),
#        title = paste("\"Signature\"", WordsORStemsORLemmas, "for the Pre- and Post- Chavez Textbook Sets.") )
