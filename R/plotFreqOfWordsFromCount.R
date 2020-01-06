################################################################
# plotFreqOfWordsFromCount.R
#
# A function to plot the frequency of words from word frequency data
################################################################
#' @export
plotFreqOfWordsFromCount <- function(countData, countThreshold=NA, topXNumberOfWords=NA){

# ##FOR TESTING####
#   word <- c("venezuela", "bolívar", "caracas", "si", "escuela", "así", "cada", "dos", "después", "casa")
#   n <- c(700, 327, 326, 263, 258, 256, 256, 249, 246, 241)
#   countData <- tibble(word, n)
#
#   countThreshold <- quantile(countData$n, c(.50))
#   topXNumberOfWords <- 5
# ##FOR TESTING####

  # ##FOR TESTING####
  #   countData <- preChavezWordFrequency
  #   countThreshold <- NA
  #   topXNumberOfWords <- topXNumberOfWordsForChart
  # ##FOR TESTING####

  # ##FOR TESTING####
  #   countData <- preChavezLemmaFrequency
  #   countThreshold <- NA
  #   topXNumberOfWords <- topXNumberOfWordsForChart
  # ##FOR TESTING####


  if(is.na(topXNumberOfWords)==TRUE & is.na(countThreshold)==TRUE){stop("You must specify either the countThreshold or topXNumberOfWords.")}

    if(is.na(topXNumberOfWords)==FALSE & is.na(countThreshold)==FALSE){stop("You must specify either the countThreshold or topXNumberOfWords.")}


  ## FOR WORD COUNTS ###

  if(is.na(countThreshold)==FALSE){
  # Filter the count tibble by the threshold
  countToPlot <- filter(countData, n > countThreshold)

  # Rearrange the count tibble for use with ggplot2
  countToPlot <- mutate(countToPlot, word = reorder(word, n))

  # Use ggplot to plot
  outPlot <- ggplot(countToPlot, aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()
  }



  if( is.na(topXNumberOfWords)==FALSE & names(countData)[1]=="word" ){
    # Filter the count tibble by the number of words
    countToPlot <- countData[1:topXNumberOfWords, ]

    # Rearrange the count tibble for use with ggplot2
    countToPlot <- mutate(countToPlot, word = reorder(word, n))

    # Use ggplot to plot
    outPlot <- ggplot(countToPlot, aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()
    }


    ## FOR STEM COUNTS ###

    if(is.na(countThreshold)==FALSE){
      # Filter the count tibble by the threshold
      countToPlot <- filter(countData, n > countThreshold)

      # Rearrange the count tibble for use with ggplot2
      countToPlot <- mutate(countToPlot, theWordStem = reorder(theWordStem, n))

      # Use ggplot to plot
      outPlot <- ggplot(countToPlot, aes(theWordStem, n)) + geom_col() + xlab(NULL) + coord_flip()
    }



    if( is.na(topXNumberOfWords)==FALSE & names(countData)[1]=="theWordStem" ){
      # Filter the count tibble by the number of words
      countToPlot <- countData[1:topXNumberOfWords, ]

      # Rearrange the count tibble for use with ggplot2
      countToPlot <- mutate(countToPlot, theWordStem = reorder(theWordStem, n))
      # Use ggplot to plot
      outPlot <- ggplot(countToPlot, aes(theWordStem, n)) + geom_col() + xlab(NULL) + coord_flip()
    }


  ## FOR Lemma COUNTS ###



  if( is.na(topXNumberOfWords)==FALSE & names(countData)[1]=="lemma" ){
    # Filter the count tibble by the number of words
    countToPlot <- countData[1:topXNumberOfWords, ]

    # Rearrange the count tibble for use with ggplot2
    countToPlot <- mutate(countToPlot, lemma = reorder(lemma, n))
    # Use ggplot to plot
    outPlot <- ggplot(countToPlot, aes(lemma, n)) + geom_col() + xlab(NULL) + coord_flip()
  }


  outPlot

}
