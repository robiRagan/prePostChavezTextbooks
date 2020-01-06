################################################################
# devScript_KeywordChanges.R
#
# A script to examine keyword changes
################################################################


rm(list = ls(all = TRUE)) # Clear the workspace

# options(scipen=999) # Turns off scientific notation


#################
# Packages
#################
# install.packages("textbooksPrePostChavez")

 library(textbooksPrePostChavez)


preChavezPdfTokens <- read_csv(file = "tokenizedText/preChavezPdfTokenTable.csv")

postChavezPdfTokens <- read_csv(file = "tokenizedText/postChavezPdfTokenTable.csv")

keywordMasterList <- read_csv(file = "tokenizedText/keywordMasterListTable.csv")




filter(preChavezPdfTokens, word=="injusta" | word=="injustamente" | word=="injusto" | word=="injusticia" | word=="injustas" | word=="injusticias")
filter(postChavezPdfTokens, word=="injusta" | word=="injustamente" | word=="injusto" | word=="injusticia" | word=="injustas" | word=="injusticias")
filter(keywordMasterList, word=="injusta" | word=="injustamente" | word=="injusto" | word=="injusticia" | word=="injustas" | word=="injusticias")


########################################
# Graphing Parameters
#####################################
graphW1 <- 20 # For the all keywords graphs

graphH1 <- 40 # For the all keywords graphs

nCols1 <- 3

graphW2 <- 40 # For the category graphs

graphH2 <- 30 # For the category graphs


nCols2 <- 4

#####################################
## Extract Frequencies ##
####################################

# # Construct a word count, stem word count and lemma count for the two sets of PDFs
# preChavezWordFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "word", onlyKeywords = FALSE, theKeywordMasterList = NA)
# postChavezWordFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "word", onlyKeywords = FALSE, theKeywordMasterList = NA)
#
# preChavezStemWordFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "stem", onlyKeywords = FALSE, theKeywordMasterList = NA)
# postChavezStemWordFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "stem", onlyKeywords = FALSE, theKeywordMasterList = NA)
#
# preChavezLemmaFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "lemma", onlyKeywords = FALSE, theKeywordMasterList = NA)
# postChavezLemmaFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "lemma", onlyKeywords = FALSE, theKeywordMasterList = NA)
#


# ##########################################################################
# ##########################################################################
# #               Filter to Keywords ###
# ##########################################################################
# ##########################################################################
#
#
#
# # Construct a word count, stem word count and lemma count for the two sets of PDFs
# preChavezKeywordWordFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "word", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)
# postChavezKeywordWordFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "word", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)
#
# preChavezKeywordStemWordFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "stem", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)
# postChavezKeywordStemWordFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "stem", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)
#
# preChavezKeywordLemmaFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "lemma", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)
# postChavezKeywordLemmaFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "lemma", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)


# #####
# # Diagnostics
#
filter(preChavezPdfTokens, lemma=="mercados")
filter(postChavezPdfTokens, lemma=="mercados")





# Plot Keyword Changes

# All Keywords scale free.
plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "word", theKeywordMasterListToPlot = keywordMasterList, theCategory = "all", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols1, FreqOrProp = "Prop")
ggsave( paste("images/changesInAllKeywordsWords.pdf", sep=""), width = graphW1, height = graphH1, units = "cm" )
ggsave( paste("images/changesInAllKeywordsWords.png", sep=""), width = graphW1, height = graphH1, units = "cm" )


plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "stem", theKeywordMasterListToPlot = keywordMasterList, theCategory = "all", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols1, FreqOrProp = "Prop")
ggsave( paste("images/changesInAllKeywordsStems.pdf", sep=""), width = graphW1, height = graphH1, units = "cm" )
ggsave( paste("images/changesInAllKeywordsStems.png", sep=""), width = graphW1, height = graphH1, units = "cm" )


plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "lemma", theKeywordMasterListToPlot = keywordMasterList, theCategory = "all", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols1, FreqOrProp = "Prop")
ggsave( paste("images/changesInAllKeywordsLemmas.pdf", sep=""), width = graphW1, height = graphH1, units = "cm" )
ggsave( paste("images/changesInAllKeywordsLemmas.png", sep=""), width = graphW1, height = graphH1, units = "cm" )




# All Keywords scale fixed
plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "word", theKeywordMasterListToPlot = keywordMasterList, theCategory = "all", yScaleFree = FALSE, barsOrDots = "Dots", numCols = nCols1, FreqOrProp = "Prop")
ggsave( paste("images/changesInAllKeywordsWordsFixedScale.pdf", sep=""), width = graphW1, height = graphH1, units = "cm" )
ggsave( paste("images/changesInAllKeywordsWordsFixedScale.png", sep=""), width = graphW1, height = graphH1, units = "cm" )


plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "stem", theKeywordMasterListToPlot = keywordMasterList, theCategory = "all", yScaleFree = FALSE, barsOrDots = "Dots", numCols = nCols1, FreqOrProp = "Prop")
ggsave( paste("images/changesInAllKeywordsStemsFixedScale.pdf", sep=""), width = graphW1, height = graphH1, units = "cm" )
ggsave( paste("images/changesInAllKeywordsStemsFixedScale.png", sep=""), width = graphW1, height = graphH1, units = "cm" )


plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "lemma", theKeywordMasterListToPlot = keywordMasterList, theCategory = "all", yScaleFree = FALSE, barsOrDots = "Dots", numCols = nCols1, FreqOrProp = "Prop")
ggsave( paste("images/changesInAllKeywordsLemmasFixedScale.pdf", sep=""), width = graphW1, height = graphH1, units = "cm" )
ggsave( paste("images/changesInAllKeywordsLemmaFixedScale.png", sep=""), width = graphW1, height = graphH1, units = "cm" )






# surplus value
plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "word", theKeywordMasterListToPlot = keywordMasterList, theCategory = "surplus value", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
ggsave( paste("images/changesInSurplusValueKeywordsWords.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
ggsave( paste("images/changesInSurplusValueKeywordsWords.png", sep=""), width = graphW2, height = graphH2, units = "cm" )


plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "stem", theKeywordMasterListToPlot = keywordMasterList, theCategory = "surplus value", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
ggsave( paste("images/changesInSurplusValueKeywordsStems.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
ggsave( paste("images/changesInSurplusValueKeywordsStems.png", sep=""), width = graphW2, height = graphH2, units = "cm" )


plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "lemma", theKeywordMasterListToPlot = keywordMasterList, theCategory = "surplus value", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
ggsave( paste("images/changesInSurplusValueKeywordsLemmas.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
ggsave( paste("images/changesInSurplusValueKeywordsLemmas.png", sep=""), width = graphW2, height = graphH2, units = "cm" )




# subjective theory of value
plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "word", theKeywordMasterListToPlot = keywordMasterList, theCategory = "subjective theory of value", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
ggsave( paste("images/changesInSubjectiveTheoryOfValueKeywordsWords.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
ggsave( paste("images/changesInSubjectiveTheoryOfValueKeywordsWords.png", sep=""), width = graphW2, height = graphH2, units = "cm" )


plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "stem", theKeywordMasterListToPlot = keywordMasterList, theCategory = "subjective theory of value", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
 ggsave( paste("images/changesInSubjectiveTheoryOfValueKeywordsStems.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
 ggsave( paste("images/changesInSubjectiveTheoryOfValueKeywordsStems.png", sep=""), width = graphW2, height = graphH2, units = "cm" )


 plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "lemma", theKeywordMasterListToPlot = keywordMasterList, theCategory = "subjective theory of value", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
ggsave( paste("images/changesInSubjectiveTheoryOfValueKeywordsLemmas.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
ggsave( paste("images/changesInSubjectiveTheoryOfValueKeywordsLemmas.png", sep=""), width = graphW2, height = graphH2, units = "cm" )




# income or wealth redistribution
plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "word", theKeywordMasterListToPlot = keywordMasterList, theCategory = "income or wealth redistribution", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
ggsave( paste("images/changesInIncomeOrWealthRedistributionKeywordsWords.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
ggsave( paste("images/changesInIncomeOrWealthRedistributionKeywordsWords.png", sep=""), width = graphW2, height = graphH2, units = "cm" )


plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "stem", theKeywordMasterListToPlot = keywordMasterList, theCategory = "income or wealth redistribution", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
ggsave( paste("images/changesInIncomeOrWealthRedistributionKeywordsStems.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
ggsave( paste("images/changesInIncomeOrWealthRedistributionKeywordsStems.png", sep=""), width = graphW2, height = graphH2, units = "cm" )


plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "lemma", theKeywordMasterListToPlot = keywordMasterList, theCategory = "income or wealth redistribution", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
ggsave( paste("images/changesInIncomeOrWealthRedistributionKeywordsLemmas.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
ggsave( paste("images/changesInIncomeOrWealthRedistributionKeywordsLemmas.png", sep=""), width = graphW2, height = graphH2, units = "cm" )



# objective theory of value
plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "word", theKeywordMasterListToPlot = keywordMasterList, theCategory = "objective theory of value", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
ggsave( paste("images/changesInObjectiveTheoryOfValueKeywordsWords.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
ggsave( paste("images/changesInObjectiveTheoryOfValueKeywordsWords.png", sep=""), width = graphW2, height = graphH2, units = "cm" )


plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "stem", theKeywordMasterListToPlot = keywordMasterList, theCategory = "objective theory of value", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
ggsave( paste("images/changesInObjectiveTheoryOfValueKeywordsStems.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
ggsave( paste("images/changesInObjectiveTheoryOfValueKeywordsStems.png", sep=""), width = graphW2, height = graphH2, units = "cm" )


plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "lemma", theKeywordMasterListToPlot = keywordMasterList, theCategory = "objective theory of value", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
ggsave( paste("images/changesInObjectiveTheoryOfValueKeywordsLemmas.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
ggsave( paste("images/changesInObjectiveTheoryOfValueKeywordsLemmas.png", sep=""), width = graphW2, height = graphH2, units = "cm" )



# public goods
plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "word", theKeywordMasterListToPlot = keywordMasterList, theCategory = "public goods", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
ggsave( paste("images/changesInPublicGoodsKeywordsWords.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
ggsave( paste("images/changesInPublicGoodsKeywordsWords.png", sep=""), width = graphW2, height = graphH2, units = "cm" )


plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "stem", theKeywordMasterListToPlot = keywordMasterList, theCategory = "public goods", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
ggsave( paste("images/changesInPublicGoodsKeywordsStems.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
ggsave( paste("images/changesInPublicGoodsKeywordsStems.png", sep=""), width = graphW2, height = graphH2, units = "cm" )


plotKeywordChanges(preChavezTokens = preChavezPdfTokens, postChavezTokens = postChavezPdfTokens, whichPropToPlot = "lemma", theKeywordMasterListToPlot = keywordMasterList, theCategory = "public goods", yScaleFree = TRUE, barsOrDots = "Dots", numCols = nCols2, FreqOrProp = "Prop")
ggsave( paste("images/changesInPublicGoodsKeywordsLemmas.pdf", sep=""), width = graphW2, height = graphH2, units = "cm" )
ggsave( paste("images/changesInPublicGoodsKeywordsLemmas.png", sep=""), width = graphW2, height = graphH2, units = "cm" )

