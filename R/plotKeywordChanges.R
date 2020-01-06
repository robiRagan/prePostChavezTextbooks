################################################################
# plotKeywordChanges.R
#
# A function to create a plot to compare the frequency of words in two sets of frequency counts
# See: https://dzone.com/articles/who-wrote-the-anti-trump-new-york-times-op-ed-usin
################################################################
#' @export
plotKeywordChanges <- function(preChavezTokens, postChavezTokens, whichPropToPlot="lemma", theCategory="all", theKeywordMasterListToPlot, FreqOrProp="Prop", yScaleFree=FALSE, barsOrDots="Dots", numCols=4){

#
  # # # # #   ##FOR TESTING with Info Generated in Main Script JUST KEYWORDS ####
  #  preChavezTokens <- preChavezPdfTokens
  # #
  #  postChavezTokens <- postChavezPdfTokens
  # #
  #  whichPropToPlot="lemma"
  #
  #  theKeywordMasterListToPlot <- keywordMasterList
  #
  #  theCategory <- "all"
  #
  #  yScaleFree=TRUE
  #
  #  barsOrDots <- "Dots"
  #
  #  numCols=2
  #
  #  FreqOrProp="Prop"
  #
  #  #    # # #   ##FOR TESTING with Info Generated in Main Script ####


   # Select the relevant term type for the keyword list
   theKeywordMasterListToPlot <- select(keywordMasterList, category, whichPropToPlot)


   # Extract the proportions for only the keywords
if(FreqOrProp=="Prop"){
   preChavezKeywordTermProp <- getProp(tokenTable = preChavezTokens, whichProp = whichPropToPlot, onlyKeywords = TRUE, theKeywordMasterList = theKeywordMasterListToPlot)
   postChavezKeywordTermProp <- getProp(tokenTable = postChavezTokens, whichProp = whichPropToPlot, onlyKeywords = TRUE, theKeywordMasterList = theKeywordMasterListToPlot)
   combinedPropOrFreq <- bind_rows(list(preChavez=preChavezKeywordTermProp, postChavez=postChavezKeywordTermProp), .id="id")
}

   # Extract the frequencies for only the keywords
   if(FreqOrProp=="Freq"){
   preChavezKeywordTermFreq <- getFreq(tokenTable = preChavezTokens, whichFreq = whichPropToPlot, onlyKeywords = TRUE, theKeywordMasterList = theKeywordMasterListToPlot)
   postChavezKeywordTermFreq <- getFreq(tokenTable = postChavezTokens, whichFreq = whichPropToPlot, onlyKeywords = TRUE, theKeywordMasterList = theKeywordMasterListToPlot)
   combinedPropOrFreq <- bind_rows(list(preChavez=preChavezKeywordTermFreq, postChavez=postChavezKeywordTermFreq), .id="id")
   }



   # Rename the columns so they have same name within this function regardless of which
   # types of terms (Words/Stems/Lemmas) are being analyzed.

   internalFunctionVarNamesForKeywords <- c("aCategory", "theTerm")

   names(theKeywordMasterListToPlot) <- internalFunctionVarNamesForKeywords


   internalFunctionVarNamesForTokenTable <- c("theDocumentSet", "theTerm", "theFreqOrProp")

   names(combinedPropOrFreq) <- internalFunctionVarNamesForTokenTable



   # Drop any duplicate keyword terms for the relevant term type
   theKeywordMasterListToPlot <- group_by(theKeywordMasterListToPlot, theTerm)
   theKeywordMasterListToPlot <- slice(theKeywordMasterListToPlot, 1)
   theKeywordMasterListToPlot <- ungroup(theKeywordMasterListToPlot)
   theKeywordMasterListToPlot <- arrange(theKeywordMasterListToPlot, aCategory)

  # Add the keyword categories to the main dataset
   combinedPropOrFreq <- left_join(x = combinedPropOrFreq, y = theKeywordMasterListToPlot)

    # #### DIAGNOSTIC TO FIND ERROR ###
    # filter(combinedPropOrFreq, lemma=="precio")



   # Rename the varibiles in teh combined data frameso they have same name within this function regardless of which
   # types of terms (Words/Stems/Lemmas) are being analyzed.


   internalFunctionVarNamesForTokenTable2 <- c("theDocumentSet", "theTerm", "theFreqOrProp", "theCategory")

   names(combinedPropOrFreq) <- internalFunctionVarNamesForTokenTable2








   # Change make theDocumentSet a Factor and set the order of the levels of the factor
  #   This will make the order of the two facets in the plot as desired.
  combinedPropOrFreq <- mutate(combinedPropOrFreq, theDocumentSet=factor(theDocumentSet, levels = c("preChavez", "postChavez") ) )

  combinedPropOrFreq <- mutate(combinedPropOrFreq, theTerm=factor(theTerm, levels=theKeywordMasterListToPlot$theTerm) )


  #### DIAGNOSTIC TO FIND ERROR ###
 #  filter(combinedPropOrFreq, theTerm=="comunidad")



  if(theCategory!="all"){
    categoryKeywords <- filter(theKeywordMasterListToPlot, aCategory==theCategory)$theTerm
    combinedPropOrFreq <- filter(combinedPropOrFreq, theTerm %in% categoryKeywords)
  }



  # Title Case the Category for use in the plot
  theCategoryInTitleCase <- str_to_title(theCategory)

  # If the category is not All then put the category name in quotes.
  if(theCategory!="all"){
  theCategoryInTitleCase <- paste("\"",theCategoryInTitleCase,"\"", sep = "")
  }

  # Title Case the Term Type for use in the plot
  theTermTypeInTitleCase <- str_to_title(whichPropToPlot)
  theTermTypeInTitleCase <- paste(theTermTypeInTitleCase,"s", sep = "")


  outPlot <- ggplot(combinedPropOrFreq)

  outPlot <- outPlot + aes(x = theDocumentSet, y = theFreqOrProp )


  if(barsOrDots=="Bars"){
  outPlot <- outPlot + geom_bar(stat = "identity", aes(fill = as.factor(theCategory))) + scale_fill_discrete(name = "Category")
  }

  if(barsOrDots=="Dots"){
    outPlot <- outPlot + geom_point(stat = "identity", aes(color = as.factor(theCategory), shape = as.factor(theCategory))) + scale_color_discrete(name = "Category") + scale_shape_discrete(name = "Category")
  }

  if(yScaleFree==TRUE){
  outPlot <- outPlot + facet_wrap(~ theTerm, scales = "free_y", ncol = numCols)
  }
  if(yScaleFree==FALSE){
  outPlot <- outPlot + facet_wrap(~ theTerm, ncol = numCols)
  }
  outPlot <- outPlot + scale_y_continuous(labels = scales::percent_format())
  outPlot <- outPlot + ylab("% Frequency")
  outPlot <- outPlot + xlab("")
  outPlot <- outPlot + ggtitle(paste("% Frequency of",theCategoryInTitleCase,"Keywords as", theTermTypeInTitleCase,"in Each Textbook Set", sep = " "))
  if(theCategory=="all"){
    outPlot <- outPlot + theme(legend.position = "bottom", legend.direction = "vertical")
    outPlot <- outPlot + guides(col=guide_legend(nrow=2, byrow=TRUE) )
  }
  if(theCategory!="all"){
  outPlot <- outPlot + theme(legend.position = "none")
  }
  outPlot
}

