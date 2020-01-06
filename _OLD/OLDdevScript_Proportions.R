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

columnTypes <- cols(
  document = col_character(),
  pageGroup = col_double(),
  line = col_double(),
  word = col_character(),
  setName = col_character()
)

preChavezPdfTokens <- read_csv(file = "tokenizedText/tokenizedPreChavez.csv", col_types = columnTypes)
postChavezPdfTokens <- read_csv(file = "tokenizedText/tokenizedPostChavez.csv", col_types = columnTypes)



####################################################################
### Construct the Keyword List
### Note: To edit the list edit the file: "R/constructKeywordList.R"
#####################################################################

keywordMasterList <- constructKeywordList()




#####################################
###     Remove Stop Words       ###
###################################

# We need to remove uninformative words.
# # In text analysis these are often called "stop words"
# # First I extract a spanish "stop word" set from the {quanteda} package.
spanish_stop_words <- tibble(word = stopwords("spanish"), lexicon = "fromQuanteda")


#Now remove all of the stop words from each of the token sets
preChavezPdfTokens <- anti_join(preChavezPdfTokens, spanish_stop_words)
postChavezPdfTokens <- anti_join(postChavezPdfTokens, spanish_stop_words)

# #Remove all lines of underscores
preChavezPdfTokens <- preChavezPdfTokens[-grep("_+", preChavezPdfTokens$word),]
postChavezPdfTokens <- postChavezPdfTokens[-grep("_+", postChavezPdfTokens$word),]


# Remove Whitespace
preChavezPdfTokens$word <- gsub("\\s+","",preChavezPdfTokens$word)
postChavezPdfTokens$word <- gsub("\\s+","",postChavezPdfTokens$word)


# Remove numeric digits
preChavezPdfTokens <- preChavezPdfTokens[-grep("\\b\\d+\\b", preChavezPdfTokens$word),]
postChavezPdfTokens <- postChavezPdfTokens[-grep("\\b\\d+\\b", postChavezPdfTokens$word),]

# #Remove all words that have any  digits in them
preChavezPdfTokens <- filter(preChavezPdfTokens, !str_detect(string = preChavezPdfTokens$word, pattern = "\\d") )
postChavezPdfTokens <- filter(postChavezPdfTokens, !str_detect(string = postChavezPdfTokens$word, pattern = "\\d") )

# #Remove all words that have a dot in them
preChavezPdfTokens <- filter(preChavezPdfTokens, !str_detect(string = preChavezPdfTokens$word, pattern = "\\.") )
postChavezPdfTokens <- filter(postChavezPdfTokens, !str_detect(string = postChavezPdfTokens$word, pattern = "\\.") )


#Remove all words less than the number of letters in the shortest keyword file
shortestKeyword <- min( str_count(keywordMasterList$word) )
preChavezPdfTokens <- filter(preChavezPdfTokens, str_count(preChavezPdfTokens$word)>shortestKeyword )
postChavezPdfTokens <- filter(postChavezPdfTokens, str_count(postChavezPdfTokens$word)>shortestKeyword )

# Correct the encoding from apostrophe symbol ’ to apostrophe symbol ' in the Post Chavez tokens
postChavezPdfTokens$word <- str_replace_all(string = postChavezPdfTokens$word, pattern = "’", replacement = "'")


##########################################################################
##########################################################################
# Steming the words and Keywords ###
# See: https://abndistro.com/post/2019/02/10/tidy-text-mining-in-r/
##########################################################################
##########################################################################


preChavezPdfTokens <- mutate(preChavezPdfTokens, theWordStem = SnowballC::wordStem(word, language = "spanish"))

postChavezPdfTokens <- mutate(postChavezPdfTokens, theWordStem = SnowballC::wordStem(word, language = "spanish"))

stemKeywordMasterList <- mutate(keywordMasterList, theWordStem = SnowballC::wordStem(word, language = "spanish"))



##########################################################################
##########################################################################
# Lemmatization of the words and Keywords ###
# See: http://www.bernhardlearns.com/2017/04/cleaning-words-with-r-stemming.html
##########################################################################
##########################################################################

# To create lemmas I use a simple approach. For a given Stem I find the most common base word across both pre-chavez, post-chavez and the keywords

# Select only the word and theWordStem variables from each dataframe
preChavezPdfWordsAndStemsOnly <- select(preChavezPdfTokens, word, theWordStem)
postChavezPdfWordsAndStemsOnly <- select(postChavezPdfTokens, word, theWordStem)
stemKeywordWordsAndStemsOnly <- select(stemKeywordMasterList, word, theWordStem)

# Combine the words and tehWordStem from all three dataframes
AllTokensAndKeywords <- rbind(preChavezPdfWordsAndStemsOnly, postChavezPdfWordsAndStemsOnly, stemKeywordWordsAndStemsOnly)

# Group the dataframe by the stems
AllTokensAndKeywords <- group_by(AllTokensAndKeywords, theWordStem)

# For each theWordStem select the word that is the most common
AllTokensAndKeywordsLemmas <- summarize(AllTokensAndKeywords, lemma = names(which.max(table(word))))

# Now create a lemma variable in each of the original 3 data frames based on theWordStem
preChavezPdfTokens <- inner_join(x = preChavezPdfTokens, y = AllTokensAndKeywordsLemmas)
postChavezPdfTokens <- inner_join(x = postChavezPdfTokens, y = AllTokensAndKeywordsLemmas)
stemKeywordMasterList <- inner_join(x = stemKeywordMasterList, y = AllTokensAndKeywordsLemmas)



# clean up global environment
remove(columnTypes, shortestKeyword, spanish_stop_words, AllTokensAndKeywords, AllTokensAndKeywordsLemmas, preChavezPdfWordsAndStemsOnly, postChavezPdfWordsAndStemsOnly, stemKeywordWordsAndStemsOnly)



##########################################################################
##########################################################################
# EXPLORE TEXT USING ALL WORDS REMAINING AFTER STOPWORDS ARE REMOVED ###
##########################################################################
##########################################################################


#####################################
## ALL WORD and ALL STEM FREQUENCY ##
####################################

# Construct a word count, stem word count and lemma count for the two sets of PDFs
preChavezWordFrequency <- count(x = preChavezPdfTokens, word, sort = TRUE)
postChavezWordFrequency <- count(x = postChavezPdfTokens, word, sort = TRUE)

preChavezStemWordFrequency <- count(x = preChavezPdfTokens, theWordStem, sort = TRUE)
postChavezStemWordFrequency <- count(x = postChavezPdfTokens, theWordStem, sort = TRUE)

preChavezLemmaFrequency <- count(x = preChavezPdfTokens, lemma, sort = TRUE)
postChavezLemmaFrequency <- count(x = postChavezPdfTokens, lemma, sort = TRUE)


########################################
## ALL WORD and ALL STEM PROPORTIONS ##
#######################################

# Construct a word count and stem word count for the two sets of PDFs
preChavezWordProportion <- tibble(word=preChavezWordFrequency$word, prop = ( preChavezWordFrequency$n/sum(preChavezWordFrequency$n) ) )
postChavezWordProportion <- tibble(word=postChavezWordFrequency$word, prop = ( postChavezWordFrequency$n/sum(postChavezWordFrequency$n) ) )

preChavezStemWordProportion <- tibble(theWordStem=preChavezStemWordFrequency$theWordStem, prop = ( preChavezStemWordFrequency$n/sum(preChavezStemWordFrequency$n) ) )
postChavezStemWordProportion <- tibble(theWordStem=postChavezStemWordFrequency$theWordStem, prop = ( postChavezStemWordFrequency$n/sum(postChavezStemWordFrequency$n) ) )


preChavezLemmaProportion <- tibble(lemma=preChavezLemmaFrequency$lemma, prop = ( preChavezLemmaFrequency$n/sum(preChavezLemmaFrequency$n) ) )
postChavezLemmaProportion <- tibble(lemma=postChavezLemmaFrequency$lemma, prop = ( postChavezLemmaFrequency$n/sum(postChavezLemmaFrequency$n) ) )



# We can construct a chart that displays all of the words that appear more than X number of times.
# Set the threshold for a word to appear in the chart.
# # Here I am charting all words whose count is in the 99th percentile

# Set the threshold or topXWords

# countThresholdForChart <- 20
topXNumberOfWordsForChart <- 20


preChavezAllWordsProp <- plotPropOfWordsFromProp(countData = preChavezWordProportion, topXNumberOfWords = topXNumberOfWordsForChart) + ggtitle( paste("Proportion of Top", topXNumberOfWordsForChart, "Words in Pre-Chavez Textbooks After Cleaning", sep=" ") )
ggsave( paste("images/top",topXNumberOfWordsForChart,"WordPropsPreChavez.pdf", sep=""), width = 20, height = 20, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"WordPropsChavez.png", sep=""), width = 20, height = 20, units = "cm" )
preChavezAllWordsProp

postChavezAllWordsProp <- plotPropOfWordsFromProp(countData = postChavezWordProportion, topXNumberOfWords = topXNumberOfWordsForChart) + ggtitle( paste("Proportion of Top", topXNumberOfWordsForChart, "Words in Post-Chavez Textbooks After Cleaning", sep=" ") )
ggsave( paste("images/top",topXNumberOfWordsForChart,"WordPropsPostChavez.pdf", sep=""), width = 20, height = 20, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"WordPropsPostChavez.png", sep=""), width = 20, height = 20, units = "cm" )
postChavezAllWordsProp




preChavezAllStemsProp <- plotPropOfWordsFromProp(countData = preChavezStemWordProportion, topXNumberOfWords = topXNumberOfWordsForChart) + ggtitle( paste("Proportion of Top", topXNumberOfWordsForChart, "Word Stems in Pre-Chavez Textbooks After Cleaning", sep=" ") )
ggsave( paste("images/top",topXNumberOfWordsForChart,"WordStemPropsPreChavez.pdf", sep=""), width = 20, height = 20, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"WordStemPropsPreChavez.png", sep=""), width = 20, height = 20, units = "cm" )
preChavezAllStemsProp


postChavezAllStemsProp <- plotPropOfWordsFromProp(countData = postChavezStemWordProportion, topXNumberOfWords = topXNumberOfWordsForChart) + ggtitle( paste("Proportion of Top", topXNumberOfWordsForChart, "Word Stems in Post-Chavez Textbooks After Cleaning", sep=" ") )
ggsave( paste("images/top",topXNumberOfWordsForChart,"WordStemPropsPostChavez.pdf", sep=""), width = 20, height = 20, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"WordStemPropsPostChavez.png", sep=""), width = 20, height = 20, units = "cm" )
postChavezAllStemsProp




preChavezAllLemmasProp <- plotPropOfWordsFromProp(countData = preChavezLemmaProportion, topXNumberOfWords = topXNumberOfWordsForChart) + ggtitle( paste("Proportion of Top", topXNumberOfWordsForChart, "Lemmas in Pre-Chavez Textbooks After Cleaning", sep=" ") )
ggsave( paste("images/top",topXNumberOfWordsForChart,"LemmaPropsPreChavez.pdf", sep=""), width = 20, height = 20, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"LemmaPropsPreChavez.png", sep=""), width = 20, height = 20, units = "cm" )
preChavezAllLemmasProp


postChavezAllLemmasProp <- plotPropOfWordsFromProp(countData = postChavezLemmaProportion, topXNumberOfWords = topXNumberOfWordsForChart) + ggtitle( paste("Proportion of Top", topXNumberOfWordsForChart, "Lemmas in Post-Chavez Textbooks After Cleaning", sep=" ") )
ggsave( paste("images/top",topXNumberOfWordsForChart,"LemmaPropsPostChavez.pdf", sep=""), width = 20, height = 20, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"LemmaPropsPostChavez.png", sep=""), width = 20, height = 20, units = "cm" )
postChavezAllLemmasProp


##########################################################################
#               EXPLORE TEXT USING KEYWORDS                     ###
##########################################################################
##########################################################################






# Now remove all words not on the master list of keywords

preChavezPdfTokensMasterKey <- filter(preChavezPdfTokens, word %in% keywordMasterList$word)
postChavezPdfTokensMasterKey <- filter(postChavezPdfTokens, word %in% keywordMasterList$word)

# Construct a word count for the two sets of PDFs
preChavezWordFrequencyMasterKey <- count(x = preChavezPdfTokensMasterKey, word, sort = TRUE)
postChavezWordFrequencyMasterKey <- count(x = postChavezPdfTokensMasterKey, word, sort = TRUE)



# Now remove all words not on the master list of stemed keywords

preChavezPdfTokensMasterKeyStems <- filter(preChavezPdfTokens, theWordStem %in% stemKeywordMasterList$theWordStem)
postChavezPdfTokensMasterKeyStems <- filter(postChavezPdfTokens, theWordStem %in% stemKeywordMasterList$theWordStem)

# Construct a stem word count for the two sets of PDFs
preChavezFrequencyMasterKeyStems <- count(x = preChavezPdfTokensMasterKeyStems, theWordStem, sort = TRUE)
postChavezFrequencyMasterKeyStems <- count(x = postChavezPdfTokensMasterKeyStems, theWordStem, sort = TRUE)


# Now remove all words not on the master list of stemed keywords

preChavezPdfTokensMasterKeyLemmas <- filter(preChavezPdfTokens, lemma %in% stemKeywordMasterList$lemma)
postChavezPdfTokensMasterKeyLemmas <- filter(postChavezPdfTokens, lemma %in% stemKeywordMasterList$lemma)

# Construct a stem word count for the two sets of PDFs
preChavezFrequencyMasterKeyLemmas <- count(x = preChavezPdfTokensMasterKeyLemmas, lemma, sort = TRUE)
postChavezFrequencyMasterKeyLemmas <- count(x = postChavezPdfTokensMasterKeyLemmas, lemma, sort = TRUE)



########################################
## KEYWORD and KEYWORD STEM PROPORTIONS ##
#######################################



# Construct a keyword count, keyword stem count, and keyword lemma count for the two sets of PDFs
preChavezKeywordProportion <- tibble(word=preChavezWordFrequencyMasterKey$word, prop = ( preChavezWordFrequencyMasterKey$n/sum(preChavezWordFrequencyMasterKey$n) ) )
postChavezKeywordProportion <- tibble(word=postChavezWordFrequencyMasterKey$word, prop = ( postChavezWordFrequencyMasterKey$n/sum(postChavezWordFrequencyMasterKey$n) ) )

preChavezStemKeywordProportion <- tibble(theWordStem=preChavezStemWordFrequency$theWordStem, prop = ( preChavezStemWordFrequency$n/sum(preChavezStemWordFrequency$n) ) )
postChavezStemKeywordProportion <- tibble(theWordStem=postChavezStemWordFrequency$theWordStem, prop = ( postChavezStemWordFrequency$n/sum(postChavezStemWordFrequency$n) ) )


preChavezLemmaKeywordProportion <- tibble(lemma=preChavezLemmaFrequency$lemma, prop = ( preChavezLemmaFrequency$n/sum(preChavezLemmaFrequency$n) ) )
postChavezLemmaKeywordProportion <- tibble(lemma=postChavezLemmaFrequency$lemma, prop = ( postChavezLemmaFrequency$n/sum(postChavezLemmaFrequency$n) ) )



# We can construct a chart that displays all of the words that appear more than X number of times.
# Set the threshold for a word to appear in the chart.
# # Here I am charting all words whose count is in the 99th percentile

# Set the threshold or topXWords

# countThresholdForChart <- 20
topXNumberOfWordsForChart3 <- 20


preChavezKeywordsProp <- plotPropOfWordsFromProp(countData = preChavezKeywordProportion, topXNumberOfWords = topXNumberOfWordsForChart) + ggtitle( paste("Proportion of Top", topXNumberOfWordsForChart, "Keywords in Pre-Chavez Textbooks After Cleaning", sep=" ") )
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordPropsPreChavez.pdf", sep=""), width = 20, height = 20, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordPropsChavez.png", sep=""), width = 20, height = 20, units = "cm" )
preChavezKeywordsProp

postChavezKeywordsProp <- plotPropOfWordsFromProp(countData = postChavezKeywordProportion, topXNumberOfWords = topXNumberOfWordsForChart) + ggtitle( paste("Proportion of Top", topXNumberOfWordsForChart, "Keywords in Post-Chavez Textbooks After Cleaning", sep=" ") )
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordPropsPostChavez.pdf", sep=""), width = 20, height = 20, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"KeywordPropsPostChavez.png", sep=""), width = 20, height = 20, units = "cm" )
postChavezKeywordsProp





preChavezKeywordStemsProp <- plotPropOfWordsFromProp(countData = preChavezStemWordProportion, topXNumberOfWords = topXNumberOfWordsForChart) + ggtitle( paste("Proportion of Top", topXNumberOfWordsForChart, "Word Stems in Pre-Chavez Textbooks After Cleaning", sep=" ") )
ggsave( paste("images/top",topXNumberOfWordsForChart,"WordStemPropsPreChavez.pdf", sep=""), width = 20, height = 20, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"WordStemPropsPreChavez.png", sep=""), width = 20, height = 20, units = "cm" )
preChavezAllStemsProp

postChavezkeywordStemsProp <- plotPropOfWordsFromProp(countData = postChavezStemWordProportion, topXNumberOfWords = topXNumberOfWordsForChart) + ggtitle( paste("Proportion of Top", topXNumberOfWordsForChart, "Word Stems in Post-Chavez Textbooks After Cleaning", sep=" ") )
ggsave( paste("images/top",topXNumberOfWordsForChart,"WordStemPropsPostChavez.pdf", sep=""), width = 20, height = 20, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"WordStemPropsPostChavez.png", sep=""), width = 20, height = 20, units = "cm" )
postChavezAllStemsProp





preChavezKeywordLemmaProp <- plotPropOfWordsFromProp(countData = preChavezLemmaKeywordProportion, topXNumberOfWords = topXNumberOfWordsForChart) + ggtitle( paste("Proportion of Top", topXNumberOfWordsForChart, "Lemmas in Pre-Chavez Textbooks After Cleaning", sep=" ") )
ggsave( paste("images/top",topXNumberOfWordsForChart,"LemmaPropsPreChavez.pdf", sep=""), width = 20, height = 20, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"LemmaPropsPreChavez.png", sep=""), width = 20, height = 20, units = "cm" )
preChavezKeywordLemmaProp

postChavezkeywordLemmaProp <- plotPropOfWordsFromProp(countData = postChavezLemmaKeywordProportion, topXNumberOfWords = topXNumberOfWordsForChart) + ggtitle( paste("Proportion of Top", topXNumberOfWordsForChart, "Lemmas in Post-Chavez Textbooks After Cleaning", sep=" ") )
ggsave( paste("images/top",topXNumberOfWordsForChart,"LemmaPropsPostChavez.pdf", sep=""), width = 20, height = 20, units = "cm" )
ggsave( paste("images/top",topXNumberOfWordsForChart,"LemmaPropsPostChavez.png", sep=""), width = 20, height = 20, units = "cm" )
postChavezkeywordLemmaProp


