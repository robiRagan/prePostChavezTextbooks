################################################################
# devScript_Corelations.R
#
# A script to do corelation analysis
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


# #####################################
# ## ALL WORD and ALL STEM FREQUENCY ##
# ####################################
#
# # Construct a word count, stem word count and lemma count for the two sets of PDFs
# preChavezWordFrequency <- count(x = preChavezPdfTokens, word, sort = TRUE)
# postChavezWordFrequency <- count(x = postChavezPdfTokens, word, sort = TRUE)
#
# preChavezStemWordFrequency <- count(x = preChavezPdfTokens, theWordStem, sort = TRUE)
# postChavezStemWordFrequency <- count(x = postChavezPdfTokens, theWordStem, sort = TRUE)
#
# preChavezLemmaFrequency <- count(x = preChavezPdfTokens, lemma, sort = TRUE)
# postChavezLemmaFrequency <- count(x = postChavezPdfTokens, lemma, sort = TRUE)
#


# Finding corelation coefficent
# Find Correlations
correlationAllWords <- correlationComparingFrequency(tidyWord1 = preChavezPdfTokens, tidyWord2 = postChavezPdfTokens, isStem = FALSE, isLemma = FALSE)

correlationStemWords <- correlationComparingFrequency(tidyWord1 = preChavezPdfTokens, tidyWord2 = postChavezPdfTokens, isStem = TRUE, isLemma = FALSE)

correlationStemWords <- correlationComparingFrequency(tidyWord1 = preChavezPdfTokens, tidyWord2 = postChavezPdfTokens, isStem = FALSE, isLemma = TRUE)


# Plot Differences
allWordsComparing <- plotComparingFrequencyAll(tidyWord1 = preChavezPdfTokens, tidyWord2 = postChavezPdfTokens, isStem = FALSE, isLemma = FALSE)  + ggtitle( paste("Comparing Word Frequency of Pre- and Post-Chavez Textbooks After Cleaning\n(",correlationAllWords$method,": ", round(correlationAllWords$estimate, digits = 2),")"))
ggsave( paste("images/allWordsFreqComparision.pdf"), width = 20, height = 20, units = "cm" )
ggsave( paste("images/allWordsFreqComparision.png"), width = 20, height = 20, units = "cm" )
allWordsComparing

# Words that are close to the line in these plots have similar frequencies in both sets of texts,
#
# Words that are far from the line are words that are found more in one set of texts than another.



stemWordsComparing <- plotComparingFrequencyAll(tidyWord1 = preChavezPdfTokens, tidyWord2 = postChavezPdfTokens, isStem = TRUE, isLemma = FALSE)  + ggtitle( paste("Comparing Stem Word Frequency of Pre- and Post-Chavez Textbooks After Cleaning\n(",correlationStemWords$method,": ", round(correlationStemWords$estimate, digits = 2),")"))
ggsave( paste("images/stemWordsFreqComparision.pdf"), width = 20, height = 20, units = "cm" )
ggsave( paste("images/stemWordsFreqComparision.png"), width = 20, height = 20, units = "cm" )
stemWordsComparing

# Words that are close to the line in these plots have similar frequencies in both sets of texts,
#
# Words that are far from the line are words that are found more in one set of texts than another.



lemmaComparing <- plotComparingFrequencyAll(tidyWord1 = preChavezPdfTokens, tidyWord2 = postChavezPdfTokens, isStem = FALSE, isLemma = TRUE)  + ggtitle( paste("Comparing Lemma Frequency of Pre- and Post-Chavez Textbooks After Cleaning\n(",correlationStemWords$method,": ", round(correlationStemWords$estimate, digits = 2),")"))
ggsave( paste("images/lemmaFreqComparision.pdf"), width = 20, height = 20, units = "cm" )
ggsave( paste("images/lemmaFreqComparision.png"), width = 20, height = 20, units = "cm" )
lemmaComparing

# Words that are close to the line in these plots have similar frequencies in both sets of texts,
#
# Words that are far from the line are words that are found more in one set of texts than another.




##########################################################################
##########################################################################
#               EXPLORE TEXT USING KEYWORDS                     ###
##########################################################################
##########################################################################






# Now remove all words not on the master list of keywords

preChavezPdfTokensMasterKey <- filter(preChavezPdfTokens, word %in% keywordMasterList$word)
postChavezPdfTokensMasterKey <- filter(postChavezPdfTokens, word %in% keywordMasterList$word)

# # Construct a word count for the two sets of PDFs
# preChavezWordFrequencyMasterKey <- count(x = preChavezPdfTokensMasterKey, word, sort = TRUE)
# postChavezWordFrequencyMasterKey <- count(x = postChavezPdfTokensMasterKey, word, sort = TRUE)
#


# Now remove all words not on the master list of stemed keywords

preChavezPdfTokensMasterKeyStems <- filter(preChavezPdfTokens, theWordStem %in% stemKeywordMasterList$theWordStem)
postChavezPdfTokensMasterKeyStems <- filter(postChavezPdfTokens, theWordStem %in% stemKeywordMasterList$theWordStem)

# # Construct a stem word count for the two sets of PDFs
# preChavezFrequencyMasterKeyStems <- count(x = preChavezPdfTokensMasterKeyStems, theWordStem, sort = TRUE)
# postChavezFrequencyMasterKeyStems <- count(x = postChavezPdfTokensMasterKeyStems, theWordStem, sort = TRUE)


# Now remove all words not on the master list of stemed keywords

preChavezPdfTokensMasterKeyLemmas <- filter(preChavezPdfTokens, lemma %in% stemKeywordMasterList$lemma)
postChavezPdfTokensMasterKeyLemmas <- filter(postChavezPdfTokens, lemma %in% stemKeywordMasterList$lemma)

# # Construct a stem word count for the two sets of PDFs
# preChavezFrequencyMasterKeyLemmas <- count(x = preChavezPdfTokensMasterKeyLemmas, lemma, sort = TRUE)
# postChavezFrequencyMasterKeyLemmas <- count(x = postChavezPdfTokensMasterKeyLemmas, lemma, sort = TRUE)
#





# Finding corelation coefficent
# Find Correlations
correlationKeywords <- correlationComparingFrequency(tidyWord1 = preChavezPdfTokensMasterKey, tidyWord2 = postChavezPdfTokensMasterKey, isStem = FALSE, isLemma = FALSE)

correlationKeywordsStems <- correlationComparingFrequency(tidyWord1 = preChavezPdfTokensMasterKeyStems, tidyWord2 = postChavezPdfTokensMasterKeyStems, isStem = TRUE, isLemma = FALSE)


correlationKeywordsLemmas <- correlationComparingFrequency(tidyWord1 = preChavezPdfTokensMasterKeyStems, tidyWord2 = postChavezPdfTokensMasterKeyStems, isStem = FALSE, isLemma = TRUE)


# Plot Differences
keywordsComparing <- plotComparingFrequencyKeywords(tidyWord1 = preChavezPdfTokensMasterKey, tidyWord2 = postChavezPdfTokensMasterKey, isStem=FALSE, isLemma = FALSE) + ggtitle( paste("Comparing Keyword Frequency of Pre- and Post-Chavez Textbooks After Cleaning\n(",correlationKeywords$method,": ", round(correlationKeywords$estimate, digits = 2),")"))
ggsave( paste("images/keywordsFreqComparision.pdf"), width = 20, height = 20, units = "cm" )
ggsave( paste("images/keywordsFreqComparision.png"), width = 20, height = 20, units = "cm" )
keywordsComparing



keywordsComparingStems <- plotComparingFrequencyKeywords(tidyWord1 = preChavezPdfTokensMasterKey, tidyWord2 = postChavezPdfTokensMasterKey, isStem=TRUE, isLemma=FALSE) + ggtitle( paste("Comparing Keyword Stem Frequency of Pre- and Post-Chavez Textbooks After Cleaning\n(",correlationKeywords$method,": ", round(correlationKeywords$estimate, digits = 2),")"))
ggsave( paste("images/keywordsStemFreqComparision.pdf"), width = 20, height = 20, units = "cm" )
ggsave( paste("images/keywordsStemFreqComparision.png"), width = 20, height = 20, units = "cm" )
keywordsComparingStems


keywordsComparingLemmas <- plotComparingFrequencyKeywords(tidyWord1 = preChavezPdfTokensMasterKey, tidyWord2 = postChavezPdfTokensMasterKey, isStem=FALSE, isLemma=TRUE) + ggtitle( paste("Comparing Keyword Lemma Frequency of Pre- and Post-Chavez Textbooks After Cleaning\n(",correlationKeywords$method,": ", round(correlationKeywords$estimate, digits = 2),")"))
ggsave( paste("images/keywordsLemmaFreqComparision.pdf"), width = 20, height = 20, units = "cm" )
ggsave( paste("images/keywordsLemmaFreqComparision.png"), width = 20, height = 20, units = "cm" )
keywordsComparingLemmas



# Words that are close to the line in these plots have similar frequencies in both sets of texts,
#
# Words that are far from the line are words that are found more in one set of texts than another.

###############################################
## Keywords by Category ####
###############################################


forCategory(preChavezPdfTokensIn = preChavezPdfTokens, postChavezPdfTokensIn = postChavezPdfTokens, theKeywordMasterList = keywordMasterList, theCategory = "surplus value", findFrequency = FALSE, findCorr = TRUE, saveGraphs = TRUE)

forCategory(preChavezPdfTokensIn = preChavezPdfTokens, postChavezPdfTokensIn = postChavezPdfTokens, theKeywordMasterList = keywordMasterList, theCategory = "labor theory of value", findFrequency = FALSE, findCorr = TRUE, saveGraphs = TRUE)

## There are not enough words in this category to plot a correlation graph
# forCategory(preChavezPdfTokensIn = preChavezPdfTokens, postChavezPdfTokensIn = postChavezPdfTokens, theKeywordMasterList = keywordMasterList, theCategory = "subjective theory of value", findFrequency = FALSE, findCorr = TRUE, saveGraphs = TRUE)

forCategory(preChavezPdfTokensIn = preChavezPdfTokens, postChavezPdfTokensIn = postChavezPdfTokens, theKeywordMasterList = keywordMasterList, theCategory = "income or wealth redistribution",findFrequency = FALSE, findCorr = TRUE, saveGraphs = TRUE)

forCategory(preChavezPdfTokensIn = preChavezPdfTokens, postChavezPdfTokensIn = postChavezPdfTokens, theKeywordMasterList = keywordMasterList, theCategory = "public goods", findFrequency = FALSE, findCorr = TRUE, saveGraphs = TRUE)
