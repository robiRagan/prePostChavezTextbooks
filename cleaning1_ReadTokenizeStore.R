################################################################
# devScript_Cleaning.R
#
# A script to read in and tokenize the texbooks
################################################################

rm(list = ls(all = TRUE)) # Clear the workspace

#################
# Packages
#################
# install.packages("textbooksPrePostChavez")

# library(textbooksPrePostChavez)



#############################################################
#############################################################
# PREPARE TEXT FOR ANALYSIS ###
#############################################################
#############################################################



# Extract and tokenize, and store the text from the PDFs and combine the tokenized data into one dataframe
# # Keep commented out except for first run of PDFs
# # # After that use the read_csv() below to read in the already tokenized data

# For the PDF Data
readPdfTokenizeAndStore(pathToPDFs = "PDFs/postChavez/", pathForOutput = "tokenizedText")

# For the text Data
readTxtTokenizeAndStore(pathToTxts = "PDFs/preChavez/", pathForOutput = "tokenizedText")

# Contruct and Store Keyword Master list
constructKeywordList(pathForOutput = "keywords")

