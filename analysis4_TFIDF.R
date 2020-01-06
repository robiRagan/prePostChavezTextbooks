################################################################
# devScript_TFIDF.R
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


preChavezPdfTokens <- read_csv(file = "tokenizedText/preChavezPdfTokenTable.csv")

postChavezPdfTokens <- read_csv(file = "tokenizedText/postChavezPdfTokenTable.csv")

keywordMasterList <- read_csv(file = "tokenizedText/keywordMasterListTable.csv")

########################################
# Graphing Parameters
#####################################
graphW <- 40

graphH <- 30



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



plotTFIDF(preChavezTokenFrequency = preChavezWordFrequency, postChavezTokenFrequency = postChavezWordFrequency, WordsORStemsORLemmas = "Words", isKeywords = FALSE, topNWords = 10)
ggsave( paste("images/signatureWordsAllWords.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/signatureWordsAllWords.png", sep=""), width = graphW, height = graphH, units = "cm" )


plotTFIDF(preChavezTokenFrequency = preChavezStemWordFrequency, postChavezTokenFrequency = postChavezStemWordFrequency, WordsORStemsORLemmas = "Stems", isKeywords = FALSE, topNWords = 10)
ggsave( paste("images/signatureStemsAllWords.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/signatureStemsAllWords.png", sep=""), width = graphW, height = graphH, units = "cm" )


plotTFIDF(preChavezTokenFrequency = preChavezLemmaFrequency, postChavezTokenFrequency = postChavezLemmaFrequency, WordsORStemsORLemmas = "Lemmas", isKeywords = FALSE, topNWords = 10)
ggsave( paste("images/signatureLemmasAllWords.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/signatureLemmasAllWords.png", sep=""), width = graphW, height = graphH, units = "cm" )




##########################################################################
##########################################################################
#       ONLY KEYWORDS                     ###
##########################################################################
##########################################################################



# Construct a word count, stem word count and lemma count for the two sets of PDFs
preChavezKeywordWordFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "word", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)
postChavezKeywordWordFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "word", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)

preChavezKeywordStemWordFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "stem", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)
postChavezKeywordStemWordFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "stem", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)

preChavezKeywordLemmaFrequency <- getFreq(tokenTable = preChavezPdfTokens, whichFreq = "lemma", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)
postChavezKeywordLemmaFrequency <- getFreq(tokenTable = postChavezPdfTokens, whichFreq = "lemma", onlyKeywords = TRUE, theKeywordMasterList = keywordMasterList)






plotTFIDF(preChavezTokenFrequency = preChavezKeywordWordFrequency, postChavezTokenFrequency = postChavezKeywordWordFrequency, WordsORStemsORLemmas = "Words", isKeywords = TRUE, topNWords = 10)
ggsave( paste("images/signatureWordsKeywords.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/signatureWordsKeywords.png", sep=""), width = graphW, height = graphH, units = "cm" )


plotTFIDF(preChavezTokenFrequency = preChavezKeywordStemWordFrequency, postChavezTokenFrequency = postChavezKeywordStemWordFrequency, WordsORStemsORLemmas = "Stems", isKeywords = TRUE, topNWords = 10)
ggsave( paste("images/signatureStemsKeywords.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/signatureStemsKeywords.png", sep=""), width = graphW, height = graphH, units = "cm" )


plotTFIDF(preChavezTokenFrequency = preChavezKeywordLemmaFrequency, postChavezTokenFrequency = postChavezKeywordLemmaFrequency, WordsORStemsORLemmas = "Lemmas", isKeywords = TRUE, topNWords = 10)
ggsave( paste("images/signatureLemmasKeywords.pdf", sep=""), width = graphW, height = graphH, units = "cm" )
ggsave( paste("images/signatureLemmasKeywords.png", sep=""), width = graphW, height = graphH, units = "cm" )


