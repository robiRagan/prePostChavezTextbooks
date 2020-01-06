################################################################
# correlationComparingFrequency.R
#
# A function to create a plot to compare the frequency of words in two sets of frequency counts
################################################################
#' @export
correlationComparingFrequency <- function(tidyWord1, tidyWord2, isStem=FALSE, isLemma = FALSE){

# ##FOR TESTING with Info Generated HERE ####
# pdfSet1Name <- "preChavez"
# wordList1 <- c("venezuela", "bolívar", "caracas", "si", "escuela", "robi", "cada", "dos", "después", "casa", "venezuela", "bolívar", "caracas", "si", "escuela", "robi", "cada", "dos", "después", "casa", "venezuela", "bolívar", "caracas", "si", "escuela", "robi", "cada", "dos", "después", "casa", "venezuela", "caracas", "bolívar", "si", "escuela", "así", "cada", "antonio", "después", "casa", "venezuela", "caracas", "bolívar", "si", "escuela", "así", "cada", "antonio", "después", "casa")
#   # theSet <- "firstSet"
#   tidyWord1 <- tibble(pdfSet= pdfSet1Name, word=wordList1, document="somethingSomething.pdf", pageGroup=c(1:length(wordList1) ), line=10)
#
#
#   pdfSet2Name <- "postChavez"
#   wordList2 <- c("venezuela", "caracas", "bolívar", "si", "escuela", "así", "cada", "antonio", "después", "casa", "venezuela", "bolívar", "caracas", "si", "escuela", "robi", "cada", "dos", "después", "casa", "venezuela", "caracas", "bolívar", "si", "escuela", "así", "cada", "antonio", "después", "casa", "venezuela", "caracas", "bolívar", "si", "escuela", "así", "cada", "antonio", "después", "casa", "venezuela", "caracas", "bolívar", "si", "escuela", "así", "cada", "antonio", "después", "casa")
#   # theSet <- "secondSet"
#   tidyWord2 <- tibble(pdfSet= pdfSet2Name, word=wordList2, document="somethingSomething.pdf", pageGroup=c(1:length(wordList2) ), line=10)
#   ##FOR TESTING with Info Generated HERE ####

#   ##FOR TESTING with Info Generated in Main Script ####
#
# tidyWord1 <- preChavezPdfTokens
#
# tidyWord2 <- postChavezPdfTokens
#
# isStem <- TRUE
#
#   ##FOR TESTING with Info Generated in Main Script ####


  # Organize the count data for plotting
  freq <- bind_rows(mutate(tidyWord1, setName = tidyWord1$setName[1]), mutate(tidyWord2, setName = tidyWord2$setName[1]))

  # freq <- mutate(freq, word = str_extract(word, "[a-z']+"))
  if(isStem==FALSE & isLemma==FALSE){
  freq <- count(freq, setName, word)
  }


  if(isStem==TRUE & isLemma==FALSE){
  freq <- count(freq, setName, theWordStem)
  }

  if(isStem==FALSE & isLemma==TRUE){
    freq <- count(freq, setName, lemma)
  }


  freq <-  group_by(freq, setName)
  freq <-  mutate(freq, proportion = n / sum(n))
  freq <-  select(freq, -n)
  freq <-  spread(freq, setName, proportion)
  freq <- gather(freq, setName, proportion, preChavez)


  outCorrelation <- cor.test(data = freq[freq$setName == "preChavez",],
           ~ proportion + postChavez)


  outCorrelation

}
