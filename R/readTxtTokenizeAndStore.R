################################################################
# readTxtTokenizeAndStore.R
#
# Reads in PDFs, Toekizes the text and Stores as a .csv
################################################################

#' @export
readTxtTokenizeAndStore <- function(pathToTxts, pathForOutput){

# # # # #### FOR TESTING #########
# pathToTxts <- "PDFs/preChavez/"
# pathForOutput <- "tokenizedText/"
# # # # #### FOR TESTING

# Exract the pdfSetNames from the fielpaths

setName <- str_remove(string = pathToTxts, pattern = "PDFs/")
setName <- str_remove(string = setName, pattern = "/")
setNameForWrite <- paste("tokenized",toupper(substring(setName, 1, 1)), substring(setName, 2), sep = "")




### TOKENIZE###


# Extract and tokenize the text from each of the preChavez PDFs
# # Combine the tokenized data into one dataframe
setTxtTokensRaw <- extractFromTxtAndTokenizeSet(filePath = pathToTxts, txtSetName = setName)



write_csv(x = setTxtTokensRaw, path = paste(pathForOutput,"/",setNameForWrite,".csv", sep="") )


}
