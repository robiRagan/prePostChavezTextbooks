################################################################
# getFreq.R
#
# Extracts the frequency of words, lemmas or stems from the token table created
# by cleaning2_FreqPropStore.R
################################################################
#' @export
getFreq <- function(tokenTable, whichFreq="word", onlyKeywords=FALSE, theKeywordMasterList=NA){


  # # # #   ##FOR TESTING with Info Generated in Main Script ####
  # # # #
  #  tokenTable <- preChavezPdfTokens
  #  whichFreq="word"
  #  onlyKeywords=FALSE
  #  theKeywordMasterList <- keywordMasterList
  #    # # #   ##FOR TESTING with Info Generated in Main Script ###

   if(onlyKeywords==TRUE){

        if(whichFreq=="word"){
        tokenTable <- filter(tokenTable, word %in% theKeywordMasterList$word)
        }

        if(whichFreq=="stem"){
        tokenTable <- filter(tokenTable, stem %in% theKeywordMasterList$stem)
        }

        if(whichFreq=="lemma"){
        tokenTable <- filter(tokenTable, lemma %in% theKeywordMasterList$lemma)
        }

     }

   if(whichFreq=="word"){
     tokenTable <- group_by(tokenTable, word)
     tokenTable <- filter(tokenTable, row_number()==1)
     tokenTable <- select(tokenTable, word, wordCount)
     }

   if(whichFreq=="stem"){
     tokenTable <- group_by(tokenTable, stem)
     tokenTable <- filter(tokenTable, row_number()==1)
     tokenTable <- select(tokenTable, stem, stemCount)
   }

   if(whichFreq=="lemma"){
     tokenTable <- group_by(tokenTable, lemma)
     tokenTable <- filter(tokenTable, row_number()==1)
     tokenTable <- select(tokenTable, lemma, lemmaCount)

   }

outFreq <- ungroup(tokenTable)

outFreq

}

