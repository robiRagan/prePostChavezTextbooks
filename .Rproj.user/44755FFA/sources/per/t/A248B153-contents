################################################################
# moveAllGraphs.R
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
filesToDelete <- list.files("/Users/ragan_ra/Dropbox/Indoctrination/allOutputGraphs", full.names = TRUE)


# clear the directory
file.remove(filesToDelete)



# find the files that you want to clear
filesToCopy <- list.files("images", full.names = TRUE)

# copy the files to the new folder
file.copy(filesToCopy, "/Users/ragan_ra/Dropbox/Indoctrination/allOutputGraphs")

