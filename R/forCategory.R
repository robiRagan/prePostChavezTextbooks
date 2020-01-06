################################################################
# forCategory.R
#
# Plots frequencies and comparision plot for categories of keywords
################################################################

#' @export
forCategory <- function(preChavezPdfTokensIn, postChavezPdfTokensIn, theKeywordMasterList, theCategory, findFrequency = TRUE, findCorr = FALSE,  saveGraphs) {


  # ## For Testing
  # preChavezPdfTokensIn  <- preChavezPdfTokens
  # postChavezPdfTokensIn <- postChavezPdfTokens
  # theKeywordMasterList <- keywordMasterList
  # theCategory <- "surplus value"
  # findFrequency <- TRUE
  # findCorr <- FALSE
  # saveGraphs <- FALSE
  # ## For Testing


  # Title Case the Category for use in the plot
  theCategoryInTitleCase <- str_to_title(theCategory)
  theCategoryInTitleCase <- paste("\"",theCategoryInTitleCase,"\"", sep = "")


  # Change Case the Category for use in filenames
  theCategoryForFileNames <- str_remove_all(str_to_title(theCategory), pattern = " ")


  # Subset the Keywords Master list for the category
  theKeywordMasterList <- filter(theKeywordMasterList, category==theCategory)

  # Now remove all words not on the master list of keywords

  preChavezPdfTokensMasterKey <- filter(preChavezPdfTokensIn, word %in% theKeywordMasterList$word)
  postChavezPdfTokensMasterKey <- filter(postChavezPdfTokensIn, word %in% theKeywordMasterList$word)


  # Construct a word count for the two sets of PDFs
  preChavezWordFrequencyMasterKey <- count(x = preChavezPdfTokensMasterKey, word, sort = TRUE)
  postChavezWordFrequencyMasterKey <- count(x = postChavezPdfTokensMasterKey, word, sort = TRUE)


  # We can construct a chart that displays all of the words that appear more than X number of times.
  # Set the threshold for a word to appear in the chart.
  # # Here I am charting all words whose count is in the 99th percentile

  if (findFrequency==TRUE){
  # Set the threshold
  # countThresholdForChart2 <- 1
  topXNumberOfWordsForChart2 <- min(length(preChavezWordFrequencyMasterKey$word), length(postChavezWordFrequencyMasterKey$word) )

  preChavezKeyFreq <- plotFreqOfWordsFromCount(countData = preChavezWordFrequencyMasterKey, topXNumberOfWords = topXNumberOfWordsForChart2) + ggtitle( paste("Frequency of the", topXNumberOfWordsForChart2,theCategoryInTitleCase,"Keywords in Pre-Chavez Textbooks", sep=" ") )
  if(saveGraphs==TRUE){
  ggsave( paste("images/top",topXNumberOfWordsForChart2,theCategoryForFileNames,"KeywordsPreChavez.pdf", sep=""), width = 20, height = 20, units = "cm" )
  ggsave( paste("images/top",topXNumberOfWordsForChart2,theCategoryForFileNames,"KeywordsPreChavez.png", sep=""), width = 20, height = 20, units = "cm" )
  }

  # Set the threshold
  # countThresholdForChart2 <- 1
  # topXNumberOfWordsForChart3 <- length(postChavezWordFrequencyMasterKey$word)

  postChavezKeyFreq <- plotFreqOfWordsFromCount(countData = postChavezWordFrequencyMasterKey, topXNumberOfWords = topXNumberOfWordsForChart2) + ggtitle( paste("Frequency of the", topXNumberOfWordsForChart2,theCategoryInTitleCase,"Keywords in Post-Chavez Textbooks", sep=" ") )
  if(saveGraphs==TRUE){
  ggsave( paste("images/top",topXNumberOfWordsForChart2,theCategoryForFileNames,"KeywordsPostChavez.pdf", sep=""), width = 20, height = 20, units = "cm" )
  ggsave( paste("images/top",topXNumberOfWordsForChart2,theCategoryForFileNames,"KeywordsPostChavez.png", sep=""), width = 20, height = 20, units = "cm" )
  }

} # end if findFrequency==TRUE




  if (findCorr==TRUE){
  # Finding corelation coefficent
  # Find Correlations
  correlationKeywords <- correlationComparingFrequency(tidyWord1 = preChavezPdfTokensMasterKey, tidyWord2 = postChavezPdfTokensMasterKey)

  # Plot Differences
  keyordsComparing <- plotComparingFrequencyKeywords(tidyWord1 = preChavezPdfTokensMasterKey, tidyWord2 = postChavezPdfTokensMasterKey) + ggtitle( paste("Comparing",theCategoryInTitleCase,"Keyword Frequency of Pre- and Post-Chavez Textbooks\n(",correlationKeywords$method,": ", round(correlationKeywords$estimate, digits = 2),")") )
  if(saveGraphs==TRUE){
  ggsave( paste0("images/keyword",theCategoryForFileNames,"FreqComparision.pdf"), width = 20, height = 20, units = "cm" )
  ggsave( paste0("images/keyword",theCategoryForFileNames,"FreqComparision.png"), width = 20, height = 20, units = "cm" )
  }

} # end if findCorr==TRUE


  if (findFrequency==TRUE & findCorr==FALSE){
    outStuff <-  list(preChavezKeyFreq, postChavezKeyFreq)
  }

  if (findFrequency==FALSE & findCorr==TRUE){
    outStuff <-  keyordsComparing
  }


  if (findFrequency==TRUE & findCorr==TRUE){
    outStuff <-  list(preChavezKeyFreq, postChavezKeyFreq, keyordsComparing)
  }

  outStuff
}
