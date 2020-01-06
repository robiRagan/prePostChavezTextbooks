################################################################
# getProp.R
#
# Extracts the proportion of words, lemmas or stems from the token table created
# by cleaning2_FreqPropStore.R
################################################################
#' @export
getProp <- function(tokenTable, whichProp="word", onlyKeywords=FALSE, theKeywordMasterList=NA){


  # # # #   ##FOR TESTING with Info Generated in Main Script ####
  # # # #
  #  tokenTable <- preChavezPdfTokens
  #  whichProp="lemma"
  #  onlyKeywords=TRUE
  #  theKeywordMasterList <- keywordMasterList
  #    # # #   ##FOR TESTING with Info Generated in Main Script ###

   if(onlyKeywords==TRUE){

        if(whichProp=="word"){
        tokenTable <- filter(tokenTable, word %in% theKeywordMasterList$word)
        }

        if(whichProp=="stem"){
        tokenTable <- filter(tokenTable, stem %in% theKeywordMasterList$stem)
        }

        if(whichProp=="lemma"){
        tokenTable <- filter(tokenTable, lemma %in% theKeywordMasterList$lemma)
        }

     }

   if(whichProp=="word"){
     tokenTable <- group_by(tokenTable, word)
     tokenTable <- filter(tokenTable, row_number()==1)
     tokenTable <- select(tokenTable, word, wordProp)
     }

   if(whichProp=="stem"){
     tokenTable <- group_by(tokenTable, stem)
     tokenTable <- filter(tokenTable, row_number()==1)
     tokenTable <- select(tokenTable, stem, stemProp)
   }

   if(whichProp=="lemma"){
     tokenTable <- group_by(tokenTable, lemma)
     tokenTable <- filter(tokenTable, row_number()==1)
     tokenTable <- select(tokenTable, lemma, lemmaProp)

   }

outProp <- ungroup(tokenTable)

outProp

}

