################################################################
# findCosineSimilarity.R
#
# A function to create a plot to compare the frequency of words in two sets of frequency counts
# See: https://dzone.com/articles/who-wrote-the-anti-trump-new-york-times-op-ed-usin
################################################################
#' @export
findCosineSimilarity <- function(preChavezTokenFrequency, postChavezTokenFrequency){


  # # # #   ##FOR TESTING with Info Generated in Main Script ####
  # # # #
  #  preChavezTokenFrequency <- preChavezWordFrequency
  # #
  #  postChavezTokenFrequency <- postChavezWordFrequency
  # #
  #    # # #   ##FOR TESTING with Info Generated in Main Script ####

# # Set the correct identifier for the labeling of the graph
#     if(isKeywords==TRUE){
#       WordsORStemsORLemmas <- paste("Keyword", WordsORStemsORLemmas, sep=" ")
#     }

  combinedFreq <- bind_rows(list(preChavez=preChavezTokenFrequency, postChavez=postChavezTokenFrequency), .id="id")

  # Rename the varibiles so they have same name within this function regardless of which
  # types of terms (Words/Stems/Lemmas) are being analyzed.

  internalFunctionVarNames <- c("theDocumentSet", "theTerm", "theFreq")

  names(combinedFreq) <- internalFunctionVarNames



  # Create the TF-IDF
  word_tf_idf <- bind_tf_idf(combinedFreq, theTerm, theDocumentSet, theFreq)
  word_tf_idf <- arrange(word_tf_idf, desc(tf_idf))


  # Remove any rows where tf_idf == 0
  # word_tf_idf <- filter(word_tf_idf, tf_idf!=0)


  # Find the pairwise similarity
  outCosineSimalarity <- pairwise_similarity(tbl = word_tf_idf, item = theDocumentSet, feature = theTerm, value = theFreq, upper = FALSE, sort = TRUE)

  outCosineSimalarity
}

