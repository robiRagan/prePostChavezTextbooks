################################################################
# devScript_Proportions.R
#
# A script to do proportion analysis
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
graphW <- 40

graphH <- 30

topXNumberOfWordsForChart <- 10



#####################################
## Calculate Proportions ##
####################################

# Construct a word count, stem word count and lemma count for the two sets of PDFs
preChavezWordProportion <- getProp(tokenTable = preChavezPdfTokens, whichProp = "word", onlyKeywords = FALSE, theKeywordMasterList = NA)
postChavezWordProportion <- getProp(tokenTable = postChavezPdfTokens, whichProp = "word", onlyKeywords = FALSE, theKeywordMasterList = NA)

preChavezStemWordProportion <- getProp(tokenTable = preChavezPdfTokens, whichProp = "stem", onlyKeywords = FALSE, theKeywordMasterList = NA)
postChavezStemWordProportion <- getProp(tokenTable = postChavezPdfTokens, whichProp = "stem", onlyKeywords = FALSE, theKeywordMasterList = NA)

preChavezLemmaProportion <- getProp(tokenTable = preChavezPdfTokens, whichProp = "lemma", onlyKeywords = FALSE, theKeywordMasterList = NA)
postChavezLemmaProportion <- getProp(tokenTable = postChavezPdfTokens, whichProp = "lemma", onlyKeywords = FALSE, theKeywordMasterList = NA)


# We can construct a chart that displays all of the words that appear more than X number of times.
# Set the threshold for a word to appear in the chart.
# # Here I am charting all words whose count is in the 99th percentile

# Set the threshold or topXWords


# Words #

AllWordsProp <- plotPrePostProp(preChavezTokenProp = preChavezWordProportion, postChavezTokenProp = postChavezWordProportion, WordsORStemsORLemmas = "Words", isKeywords = FALSE, topNWords = topXNumberOfWordsForChart)
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllWordsProp.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllWordsProp.png", sep=""), width = graphW, height = graphH, units = "cm" )
AllWordsProp

# Stems #


AllStemsProp <- plotPrePostProp(preChavezTokenProp = preChavezStemWordProportion, postChavezTokenProp = postChavezStemWordProportion, WordsORStemsORLemmas = "Stems", isKeywords = FALSE, topNWords = topXNumberOfWordsForChart)
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllStemsProp.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllStemsProp.png", sep=""), width = graphW, height = graphH, units = "cm" )
AllStemsProp



# Lemmas #

AllLemmasProp <- plotPrePostProp(preChavezTokenProp = preChavezLemmaProportion, postChavezTokenProp = postChavezLemmaProportion, WordsORStemsORLemmas = "Lemmas", isKeywords = FALSE, topNWords = topXNumberOfWordsForChart)
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllLemmasProp.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllLemmasProp.png", sep=""), width = graphW, height = graphH, units = "cm" )
AllLemmasProp



#####################################
## Calculate Changes in the Proportions ##
####################################


AllWordsPropChange <- plotChangesInProp(preChavezItemProp = preChavezWordProportion, postChavezItemProp = postChavezWordProportion, WordORStemORLemma = "Word", numberOfItems = topXNumberOfWordsForChart, isKeywords = FALSE)
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllWordsPropChange.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllWordsPropChange.png", sep=""), width = graphW, height = graphH, units = "cm" )
AllWordsPropChange

# Stems #


AllStemsPropChange <- plotChangesInProp(preChavezItemProp = preChavezStemWordProportion, postChavezItemProp = postChavezStemWordProportion, WordORStemORLemma = "Stem", numberOfItems = topXNumberOfWordsForChart, isKeywords = FALSE)
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllStemsPropChange.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllStemsPropChange.png", sep=""), width = graphW, height = graphH, units = "cm" )
AllStemsPropChange



# Lemmas #

AllLemmasPropChange <- plotChangesInProp(preChavezItemProp = preChavezLemmaProportion, postChavezItemProp = postChavezLemmaProportion, WordORStemORLemma = "Lemma", numberOfItems = topXNumberOfWordsForChart, isKeywords = FALSE)
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllLemmasPropChange.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"AllLemmasPropChange.png", sep=""), width = graphW, height = graphH, units = "cm" )
AllLemmasPropChange



AllLemmasPropChange50 <- plotChangesInProp(preChavezItemProp = preChavezLemmaProportion, postChavezItemProp = postChavezLemmaProportion, WordORStemORLemma = "Lemma", numberOfItems = 50, isKeywords = FALSE)
ggsave( paste("images/top",50,"AllLemmasPropChange50.pdf", sep=""), width = 50, height = 50, units = "cm" )
ggsave( paste("images/top",50,"AllLemmasPropChange50.png", sep=""), width = 50, height = 50, units = "cm" )
AllLemmasPropChange50


AllLemmasPropChange100 <- plotChangesInProp(preChavezItemProp = preChavezLemmaProportion, postChavezItemProp = postChavezLemmaProportion, WordORStemORLemma = "Lemma", numberOfItems = 100, isKeywords = FALSE)
ggsave( paste("images/top",100,"AllLemmasPropChange100.pdf", sep=""), width = 60, height = 60, units = "cm" )
ggsave( paste("images/top",100,"AllLemmasPropChange100.png", sep=""), width = 60, height = 60, units = "cm" )
AllLemmasPropChange100



##########################################################################
##########################################################################
#               EXPLORE TEXT USING KEYWORDS                     ###
##########################################################################
##########################################################################



# Construct a word count, stem word count and lemma count for the two sets of PDFs
preChavezKeywordWordProportion <- getProp(tokenTable = preChavezPdfTokens, whichProp = "word", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)
postChavezKeywordWordProportion <- getProp(tokenTable = postChavezPdfTokens, whichProp = "word", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)

preChavezKeywordStemWordProportion <- getProp(tokenTable = preChavezPdfTokens, whichProp = "stem", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)
postChavezKeywordStemWordProportion <- getProp(tokenTable = postChavezPdfTokens, whichProp = "stem", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)

preChavezKeywordLemmaProportion <- getProp(tokenTable = preChavezPdfTokens, whichProp = "lemma", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)
postChavezKeywordLemmaProportion <- getProp(tokenTable = postChavezPdfTokens, whichProp = "lemma", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)




# Words #

KeywordWordsProp <- plotPrePostProp(preChavezTokenProp = preChavezKeywordWordProportion, postChavezTokenProp = postChavezKeywordWordProportion, WordsORStemsORLemmas = "Words", isKeywords = TRUE, topNWords = topXNumberOfWordsForChart)
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordWordsProp.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordWordsProp.png", sep=""), width = graphW, height = graphH, units = "cm" )
KeywordWordsProp

# Stems #


KeywordStemsProp <- plotPrePostProp(preChavezTokenProp = preChavezKeywordStemWordProportion, postChavezTokenProp = postChavezKeywordStemWordProportion, WordsORStemsORLemmas = "Stems", isKeywords = TRUE, topNWords = topXNumberOfWordsForChart)
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordStemsProp.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordStemsProp.png", sep=""), width = graphW, height = graphH, units = "cm" )
KeywordStemsProp



# Lemmas #

KeywordLemmasProp <- plotPrePostProp(preChavezTokenProp = preChavezKeywordLemmaProportion, postChavezTokenProp = postChavezKeywordLemmaProportion, WordsORStemsORLemmas = "Lemmas", isKeywords = TRUE, topNWords = topXNumberOfWordsForChart)
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordLemmasProp.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordLemmasProp.png", sep=""), width = graphW, height = graphH, units = "cm" )
KeywordLemmasProp

########################################
### Proportion Changes with Keywords ###
##########################################

KeywordsWordsPropChange <- plotChangesInProp(preChavezItemProp = preChavezKeywordWordProportion, postChavezItemProp = postChavezKeywordWordProportion, WordORStemORLemma = "Word", numberOfItems = topXNumberOfWordsForChart, isKeywords = FALSE)
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordsWordsPropChange.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordsWordsPropChange.png", sep=""), width = graphW, height = graphH, units = "cm" )
KeywordsWordsPropChange

# Stems #


KeywordsStemsPropChange <- plotChangesInProp(preChavezItemProp = preChavezKeywordStemWordProportion, postChavezItemProp = postChavezKeywordStemWordProportion, WordORStemORLemma = "Stem", numberOfItems = topXNumberOfWordsForChart, isKeywords = FALSE)
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordsStemsPropChange.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordsStemsPropChange.png", sep=""), width = graphW, height = graphH, units = "cm" )
KeywordsStemsPropChange



# Lemmas #

KeywordsLemmasPropChange <- plotChangesInProp(preChavezItemProp = preChavezKeywordLemmaProportion, postChavezItemProp = postChavezKeywordLemmaProportion, WordORStemORLemma = "Lemma", numberOfItems = topXNumberOfWordsForChart, isKeywords = FALSE)
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordsLemmasPropChange.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordsLemmasPropChange.png", sep=""), width = graphW, height = graphH, units = "cm" )
KeywordsLemmasPropChange




