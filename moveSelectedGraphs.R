################################################################
# moveSelectedGraphs.R
#
# A script to do frequency analysis
################################################################


rm(list = ls(all = TRUE)) # Clear the workspace

# options(scipen=999) # Turns off scientific notation


#################
# Packages
#################
# install.packages("textbooksPrePostChavez")

library(textbooksPrePostChavez)


# find the files that you want to clear
filesToDelete <- list.files("/Users/ragan_ra/Dropbox/Indoctrination/graphsForPaper/", full.names = TRUE)


# clear the directory
file.remove(filesToDelete)


# find the files that you want
list.of.files <- c("images/top10KeywordLemmasProp.pdf",  "images/top10KeywordsLemmasPropChange.pdf", "images/top10AllLemmasPropChange.pdf", "images/top10AllLemmasProp.pdf")

# copy the files to the new folder
file.copy(list.of.files, "/Users/ragan_ra/Dropbox/Indoctrination/graphsForPaper")

