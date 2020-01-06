################################################################
# devScript_network.R
#
# A script to do network analysis
################################################################

rm(list = ls(all = TRUE)) # Clear the workspace

# options(scipen=999) # Turns off scientific notation


#################
# Packages
#################
# install.packages("textbooksPrePostChavez")

 library(textbooksPrePostChavez)



#############################################################
#############################################################
# PREPARE TEXT FOR ANALYSIS ###
#############################################################
#############################################################



# Extract and tokenize, and store the text from the PDFs and combine the tokenized data into one dataframe
# # Keep commented out except for first run of PDFs
# # # After that use the read_csv() below to read in the already tokenized data

  # For the PDF Data
#    readPdfTokenizeAndStore(path = "PDFs/postChavez/")

  # For the text Data
#  readTxtTokenizeAndStore(path = "PDFs/preChavez/")


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

# # Construct a word count, stem word count and lemma count for the two sets of PDFs
# preChavezWordFrequency <- count(x = preChavezPdfTokens, word, sort = TRUE)
# postChavezWordFrequency <- count(x = postChavezPdfTokens, word, sort = TRUE)
#
# preChavezStemWordFrequency <- count(x = preChavezPdfTokens, theWordStem, sort = TRUE)
# postChavezStemWordFrequency <- count(x = postChavezPdfTokens, theWordStem, sort = TRUE)
#
# preChavezLemmaFrequency <- count(x = preChavezPdfTokens, lemma, sort = TRUE)
# postChavezLemmaFrequency <- count(x = postChavezPdfTokens, lemma, sort = TRUE)







##########################################################################
##########################################################################
#           KEYWORDS                     ###
##########################################################################
##########################################################################



# Now remove all words not on the master list of keywords

preChavezPdfTokensMasterKey <- filter(preChavezPdfTokens, word %in% keywordMasterList$word)
postChavezPdfTokensMasterKey <- filter(postChavezPdfTokens, word %in% keywordMasterList$word)


# Now remove all words not on the master list of stemed keywords

preChavezPdfTokensMasterKeyStems <- filter(preChavezPdfTokens, theWordStem %in% stemKeywordMasterList$theWordStem)
postChavezPdfTokensMasterKeyStems <- filter(postChavezPdfTokens, theWordStem %in% stemKeywordMasterList$theWordStem)


# Now remove all words not on the master list of stemed keywords

preChavezPdfTokensMasterKeyLemmas <- filter(preChavezPdfTokens, lemma %in% stemKeywordMasterList$lemma)
postChavezPdfTokensMasterKeyLemmas <- filter(postChavezPdfTokens, lemma %in% stemKeywordMasterList$lemma)




###############################################
## Network Analysis ####
###############################################

# https://juliasilge.com/blog/life-changing-magic/



###############################################
## Keyword Pair Counts ####
## Two words are a pair if they appear on the same line.
###############################################

preChavezPairCountWords <- pairwise_count(tbl = preChavezPdfTokens, word, line, sort = TRUE)

preChavezPairCountStems <- pairwise_count(tbl = preChavezPdfTokens, theWordStem, line, sort = TRUE)

preChavezPairCountLemmas <- pairwise_count(tbl = preChavezPdfTokens, lemma, line, sort = TRUE)


postChavezPairCountWords <- pairwise_count(tbl = postChavezPdfTokens, word, line, sort = TRUE)

postChavezPairCountStems <- pairwise_count(tbl = postChavezPdfTokens, theWordStem, line, sort = TRUE)

postChavezPairCountLemmas <- pairwise_count(tbl = postChavezPdfTokens, lemma, line, sort = TRUE)



###############################################
## Networks ####
###############################################

preChavezPairCountWords %>%
  filter(n >= 40) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8) +
  #ggtitle(expression(paste("Word Network in Jane Austen's ", italic("Pride and Prejudice")))) +
  theme_void()


postChavezPairCountWords %>%
  filter(n >= 40) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8) +
  #ggtitle(expression(paste("Word Network in Jane Austen's ", italic("Pride and Prejudice")))) +
  theme_void()



preChavezPairCountStems %>%
  filter(n >= 40) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8) +
  #ggtitle(expression(paste("Word Network in Jane Austen's ", italic("Pride and Prejudice")))) +
  theme_void()


postChavezPairCountStems %>%
  filter(n >= 40) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8) +
  #ggtitle(expression(paste("Word Network in Jane Austen's ", italic("Pride and Prejudice")))) +
  theme_void()



preChavezPairCountLemmas %>%
  filter(n >= 40) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8) +
  #ggtitle(expression(paste("Word Network in Jane Austen's ", italic("Pride and Prejudice")))) +
  theme_void()


postChavezPairCountLemmas %>%
  filter(n >= 40) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8) +
  #ggtitle(expression(paste("Word Network in Jane Austen's ", italic("Pride and Prejudice")))) +
  theme_void()
