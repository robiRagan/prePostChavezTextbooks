################################################################
# plotPropOfWordsFromProp.R
#
# A function to plot the frequency of words from word frequency data
################################################################
#' @export
plotPropOfWordsFromProp <- function(countData, topXNumberOfWords=NA){

# ##FOR TESTING####
#   word <- c("venezuela", "bolívar", "caracas", "si", "escuela", "así", "cada", "dos", "después", "casa")
#   n <- c(700, 327, 326, 263, 258, 256, 256, 249, 246, 241)
#   countData <- tibble(word, n)
#
#   countThreshold <- quantile(countData$n, c(.50))
#   topXNumberOfWords <- 5
# ##FOR TESTING####

  # ##FOR TESTING####
  #   countData <- preChavezWordProportion
  #   topXNumberOfWords <- topXNumberOfWordsForChart
  # ##FOR TESTING####

  # ##FOR TESTING####
  #   countData <- preChavezStemWordProportion
  #   topXNumberOfWords <- topXNumberOfWordsForChart
  # ##FOR TESTING####


  if(is.na(topXNumberOfWords)==TRUE){stop("You must specify the topXNumberOfWords.")}





  if( is.na(topXNumberOfWords)==FALSE & names(countData)[1]=="word" ){
    # Filter the count tibble by the number of words
    countToPlot <- countData[1:topXNumberOfWords, ]

    # Rearrange the count tibble for use with ggplot2
    countToPlot <- mutate(countToPlot, word = reorder(word, prop))

    # Use ggplot to plot
    outPlot <- ggplot(countToPlot, aes(word, prop)) + geom_col() + xlab(NULL) + coord_flip()
    }


    ## FOR STEM COUNTS ###



    if( is.na(topXNumberOfWords)==FALSE & names(countData)[1]=="theWordStem" ){
      # Filter the count tibble by the number of words
      countToPlot <- countData[1:topXNumberOfWords, ]

      # Rearrange the count tibble for use with ggplot2
      countToPlot <- mutate(countToPlot, theWordStem = reorder(theWordStem, prop))
      # Use ggplot to plot
      outPlot <- ggplot(countToPlot, aes(theWordStem, prop)) + geom_col() + xlab(NULL) + coord_flip()
    }


  ## FOR Lemma COUNTS ###



  if( is.na(topXNumberOfWords)==FALSE & names(countData)[1]=="lemma" ){
    # Filter the count tibble by the number of words
    countToPlot <- countData[1:topXNumberOfWords, ]

    # Rearrange the count tibble for use with ggplot2
    countToPlot <- mutate(countToPlot, lemma = reorder(lemma, prop))
    # Use ggplot to plot
    outPlot <- ggplot(countToPlot, aes(lemma, prop)) + geom_col() + xlab(NULL) + coord_flip()
  }



  outPlot

}
