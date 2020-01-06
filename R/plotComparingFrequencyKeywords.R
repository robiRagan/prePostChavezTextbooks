################################################################
# plotComparingFrequency.R
#
# A function to create a plot to compare the frequency of words in two sets of frequency counts
################################################################
#' @export
plotComparingFrequencyKeywords <- function(tidyWord1, tidyWord2, isStem=FALSE, isLemma = FALSE){

# ##FOR TESTING with Info Generated HERE ####
# setName1Name <- "preChavez"
# wordList1 <- c("venezuela", "bolívar", "caracas", "si", "escuela", "robi", "cada", "dos", "después", "casa", "venezuela", "bolívar", "caracas", "si", "escuela", "robi", "cada", "dos", "después", "casa", "venezuela", "bolívar", "caracas", "si", "escuela", "robi", "cada", "dos", "después", "casa", "venezuela", "caracas", "bolívar", "si", "escuela", "así", "cada", "testwordlist1heyheyheyhey", "después", "casa", "venezuela", "caracas", "bolívar", "si", "escuela", "así", "cada", "antonio", "después", "casa")
#   # theSet <- "firstSet"
#   tidyWord1 <- tibble(setName= setName1Name, word=wordList1, document="somethingSomething.pdf", pageGroup=c(1:length(wordList1) ), line=10)
#
#
#   setName2Name <- "postChavez"
#   wordList2 <- c("venezuela", "caracas", "bolívar", "si", "escuela", "así", "cada", "antonio", "después", "casa", "venezuela", "bolívar", "caracas", "si", "escuela", "robi", "cada", "dos", "después", "casa", "venezuela", "caracas", "bolívar", "si", "escuela", "así", "cada", "antonio", "después", "casa", "venezuela", "caracas", "bolívar", "si", "escuela", "así", "cada", "antonio", "después", "casa", "venezuela", "caracas", "bolívar", "testwordlist2heyheyheyhey", "escuela", "así", "cada", "antonio", "después", "casa")
#   # theSet <- "secondSet"
#   tidyWord2 <- tibble(setName= setName2Name, word=wordList2, document="somethingSomething.pdf", pageGroup=c(1:length(wordList2) ), line=10)
#   ##FOR TESTING with Info Generated HERE ####

# # #   ##FOR TESTING with Info Generated in Main Script ####
# # #
#  tidyWord1 <- preChavezPdfTokensMasterKey
# #
#  tidyWord2 <- postChavezPdfTokensMasterKey
#  #
# isStem <- FALSE
# # #
# # #   ##FOR TESTING with Info Generated in Main Script ####



  # #   ##FOR TESTING with Info Generated in Main Script ####
  # #
  #  tidyWord1 <- preChavezPdfTokensMasterKeyStems
  # #
  #  tidyWord2 <- postChavezPdfTokensMasterKeyStems
  #  #
  # isStem <- TRUE
  # # #
  # # #   ##FOR TESTING with Info Generated in Main Script ####
  #




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
  freq <-  replace_na(data = freq, replace = list(postChavez=0, preChavez=0))
  freq <- gather(freq, setName, proportion, preChavez)
  # Add a column to sort by so that more importnat words are plotted first
  freq$sumOfProps <-  freq$postChavez + freq$proportion
  freq <- arrange(freq, desc(sumOfProps))
  freq$adjHjust <- ifelse(freq$proportion==0,-.5,1.5)
  freq$adjVjust <- ifelse(freq$postChavez==0,-.5,1.5)
  freq$customColor <- ifelse(freq$postChavez==0, 1, abs(freq$postChavez - freq$proportion))

 # freq <- sample_n(freq,500)
  if(isStem==FALSE & isLemma==FALSE){
  outPlot <- ggplot(freq, aes(x = proportion, y = postChavez, color = customColor ) ) +
    geom_abline(color = "gray40", lty = 2) +
    # geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = word, vjust = adjVjust, hjust = adjHjust, size=1, color=customColor), check_overlap = TRUE) +
    scale_x_log10(labels = percent_format(), expand = c(.1, .1)) +
    scale_y_log10(labels = percent_format(), expand = c(.1, .1)) +
    scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
    facet_wrap(~setName, ncol = 2) +
    theme(legend.position="none") +
    labs(y = "PostChavez", x = NULL)
  }



  if(isStem==TRUE & isLemma==FALSE){
    outPlot <- ggplot(freq, aes(x = proportion, y = postChavez, color = customColor ) ) +
      geom_abline(color = "gray40", lty = 2) +
      # geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
      geom_text(aes(label = theWordStem, vjust = adjVjust, hjust = adjHjust, size=1, color=customColor), check_overlap = TRUE) +
      scale_x_log10(labels = percent_format(), expand = c(.1, .1)) +
      scale_y_log10(labels = percent_format(), expand = c(.1, .1)) +
      scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
      facet_wrap(~setName, ncol = 2) +
      theme(legend.position="none") +
      labs(y = "PostChavez", x = NULL)
  }

  if(isStem==FALSE & isLemma==TRUE){
    outPlot <- ggplot(freq, aes(x = proportion, y = postChavez, color = customColor ) ) +
      geom_abline(color = "gray40", lty = 2) +
      # geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
      geom_text(aes(label = lemma, vjust = adjVjust, hjust = adjHjust, size=1, color=customColor), check_overlap = TRUE) +
      scale_x_log10(labels = percent_format(), expand = c(.1, .1)) +
      scale_y_log10(labels = percent_format(), expand = c(.1, .1)) +
      scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
      facet_wrap(~setName, ncol = 2) +
      theme(legend.position="none") +
      labs(y = "PostChavez", x = NULL)
  }




  # ggplot(freq, aes(x = proportion, y = postChavez, color = abs(postChavez - proportion))) +
  #   geom_abline(color = "gray40", lty = 2) +
  #   geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  #   geom_text(aes(label = word, vjust = adjVjust, hjust = adjHjust), check_overlap = TRUE) +
  #   scale_x_log10(labels = percent_format()) +
  #   scale_y_log10(labels = percent_format()) +
  #   scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  #   facet_wrap(~setName, ncol = 2) +
  #   theme(legend.position="none") +
  #   labs(y = "PostChavez", x = NULL)


  # ggplot(freq, aes(x = proportion, y = postChavez, color = abs(postChavez - proportion))) +
  #   geom_abline(color = "gray40", lty = 2) +
  #   geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  #   geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  #   scale_x_log10(labels = percent_format()) +
  #   scale_y_log10(labels = percent_format()) +
  #   scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  #   facet_wrap(~setName, ncol = 2) +
  #   theme(legend.position="none") +
  #   labs(y = "PostChavez", x = NULL)

  outPlot
}
