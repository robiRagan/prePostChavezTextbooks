################################################################
# devScript_Frequency.R
#
# A script to do frequency analysis
################################################################


rm(list = ls(all = TRUE)) # Clear the workspace

# options(scipen=999) # Turns off scientific notation


#################
# Packages
#################
# install.packages("textbooksPrePostChavez")

 library(textbooksPrePostChavez)


#####################################
### Read in the Tokenized Data   ###
### Tokenized Data was created   ###
### with the "Cleaning" Script   ###
###################################


preChavezPdfTokens <- read_csv(file = "tokenizedText/preChavezPdfTokenTable.csv")

postChavezPdfTokens <- read_csv(file = "tokenizedText/postChavezPdfTokenTable.csv")

keywordMasterList <- read_csv(file = "tokenizedText/keywordMasterListTable.csv")




########################################
# Graphing Parameters
#####################################
graphW <- 20

graphH <- 15

topXNumberOfWordsForChart <- 10

#####################################
## Extract Frequencies ##
####################################

# Construct a word count, stem word count and lemma count for the two sets of PDFs
preChavezWordFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "word", onlyKeywords = FALSE, theKeywordMasterList = NA)
postChavezWordFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "word", onlyKeywords = FALSE, theKeywordMasterList = NA)

preChavezStemWordFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "stem", onlyKeywords = FALSE, theKeywordMasterList = NA)
postChavezStemWordFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "stem", onlyKeywords = FALSE, theKeywordMasterList = NA)

preChavezLemmaFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "lemma", onlyKeywords = FALSE, theKeywordMasterList = NA)
postChavezLemmaFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "lemma", onlyKeywords = FALSE, theKeywordMasterList = NA)

numberOfUniquePreChavezLemmas <- nrow(preChavezLemmaFrequency)
numberOfUniquePostChavezLemmas <- nrow(postChavezLemmaFrequency)


totalPreChavezLemmaFrequency <- sum(preChavezLemmaFrequency$lemmaCount)
totalPostChavezLemmaFrequency <- sum(postChavezLemmaFrequency$lemmaCount)



# We can construct a chart that displays all of the words that appear more than X number of times.
# Set the threshold for a word to appear in the chart.
# # Here I am charting all words whose count is in the 99th percentile

# Set the threshold or topXWords

# countThresholdForChart <- 20


# Words #

AllWordsFreq <- plotPrePostFreq(preChavezTokenFrequency = preChavezWordFrequency, postChavezTokenFrequency = postChavezWordFrequency, WordsORStemsORLemmas = "Words", isKeywords = FALSE, topNWords = topXNumberOfWordsForChart)
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllWordsFreq.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllWordsFreq.png", sep=""), width = graphW, height = graphH, units = "cm" )
AllWordsFreq

# Stems #


AllStemsFreq <- plotPrePostFreq(preChavezTokenFrequency = preChavezStemWordFrequency, postChavezTokenFrequency = postChavezStemWordFrequency, WordsORStemsORLemmas = "Stems", isKeywords = FALSE, topNWords = topXNumberOfWordsForChart)
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllStemsFreq.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllStemsFreq.png", sep=""), width = graphW, height = graphH, units = "cm" )
AllStemsFreq



# Lemmas #

AllLemmasFreq <- plotPrePostFreq(preChavezTokenFrequency = preChavezLemmaFrequency, postChavezTokenFrequency = postChavezLemmaFrequency, WordsORStemsORLemmas = "Lemmas", isKeywords = FALSE, topNWords = topXNumberOfWordsForChart)
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllLemmasFreq.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllLemmasFreq.png", sep=""), width = graphW, height = graphH, units = "cm" )
AllLemmasFreq


# All lemmas Not just Top X#

AllLemmasFreq <- plotPrePostFreq(preChavezTokenFrequency = preChavezLemmaFrequency, postChavezTokenFrequency = postChavezLemmaFrequency, WordsORStemsORLemmas = "Lemmas", isKeywords = FALSE, topNWords = topXNumberOfWordsForChart)
ggsave( paste("images/AllLemmasFreq.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/AllLemmasFreq.png", sep=""), width = graphW, height = graphH, units = "cm" )
AllLemmasFreq




##########################################################################
##########################################################################
#               EXPLORE TEXT USING KEYWORDS                     ###
##########################################################################
##########################################################################



# Construct a word count, stem word count and lemma count for the two sets of PDFs
preChavezKeywordWordFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "word", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)
postChavezKeywordWordFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "word", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)

preChavezKeywordStemWordFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "stem", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)
postChavezKeywordStemWordFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "stem", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)

preChavezKeywordLemmaFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "lemma", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)
postChavezKeywordLemmaFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "lemma", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)

numberOfUniquePreChavezKeywordLemmas <- nrow(preChavezKeywordLemmaFrequency)
numberOfUniquePostChavezKeywordLemmas <- nrow(postChavezKeywordLemmaFrequency)


totalPreChavezKeywordLemmaFrequency <- sum(preChavezKeywordLemmaFrequency$lemmaCount)
totalPostChavezKeywordLemmaFrequency <- sum(postChavezKeywordLemmaFrequency$lemmaCount)

uniqueKeywordLemmasInPre <- preChavezKeywordLemmaFrequency$lemma

uniqueKeywordLemmasInPost <- postChavezKeywordLemmaFrequency$lemma

keywordLemmas <- keywordMasterList$lemma

allKeyWordLemmasInPreAndPost <- unique(uniqueKeywordLemmasInPre, uniqueKeywordLemmasInPost)

keywordLemmasNotInEitherSet <- setdiff(keywordLemmas, allKeyWordLemmasInPreAndPost)



# Words #

KeywordWordsFreq <- plotPrePostFreq(preChavezTokenFrequency = preChavezKeywordWordFrequency, postChavezTokenFrequency = postChavezKeywordWordFrequency, WordsORStemsORLemmas = "Words", isKeywords = TRUE, topNWords = topXNumberOfWordsForChart)
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordWordsFreq.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordWordsFreq.png", sep=""), width = graphW, height = graphH, units = "cm" )
KeywordWordsFreq

# Stems #


KeywordStemsFreq <- plotPrePostFreq(preChavezTokenFrequency = preChavezKeywordStemWordFrequency, postChavezTokenFrequency = postChavezKeywordStemWordFrequency, WordsORStemsORLemmas = "Stems", isKeywords = TRUE, topNWords = topXNumberOfWordsForChart)
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordStemsFreq.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordStemsFreq.png", sep=""), width = graphW, height = graphH, units = "cm" )
KeywordStemsFreq



# Lemmas #

KeywordLemmasFreq <- plotPrePostFreq(preChavezTokenFrequency = preChavezKeywordLemmaFrequency, postChavezTokenFrequency = postChavezKeywordLemmaFrequency, WordsORStemsORLemmas = "Lemmas", isKeywords = TRUE, topNWords = topXNumberOfWordsForChart)
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordLemmasFreq.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordLemmasFreq.png", sep=""), width = graphW, height = graphH, units = "cm" )
KeywordLemmasFreq



