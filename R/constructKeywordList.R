################################################################
# constructKeywordList.R
#
# Creates a keyword list using the words in this file.
# Revised based on Antonios email
################################################################


#' @export
constructKeywordList <- function(pathForOutput){

  # # # #### FOR TESTING #########
   #   pathForOutput <- "keywords/"
  # # # #### FOR TESTING

  # Exract the pdfSetNames from the fielpaths



# First create each category of keywords
surplusValueList <- tibble::tibble(category= "surplus value",
                           word=c("abuso",
                                  "apropiación",
                                  "aprovechar",
                                  "capital",
                                  "despojo",
                                  "obreros")
)




objectiveTheoryOfValue <- tibble::tibble(category="objective theory of value",
                             word=c("costo",
                                    "injusto",
                                    "justo",
                                    "trabajo",
                                    "esclavo")
)



subjectiveTheoryOfValue <- tibble::tibble(category="subjective theory of value",
                                  word=c("oferta",
                                         "demanda",
                                         "precio",
                                         "mercado")
)


incomeOrWealthRedist <- tibble::tibble(category="income or wealth redistribution",
                               word=c("comunidad",
                                      "derecho",
                                      "desigualdad",
                                      "distribución",
                                      "igualdad",
                                      "injusticia",
                                      "justicia",
                                      "repartición",
                                      "indígena",
                                      "pueblo",
                                      "lucha",
                                      "popular")
)


publicGoods <- tibble::tibble(category="public goods",
                      word=c("bienes",
                             "públicos",
                             "bienestar",
                             "educación",
                             "explotación",
                             "gratis",
                             "gratuita",
                             "salud",
                              "servicios",
                              "vivienda")
)


nationalism <- tibble::tibble(category="nationalism",
                                    word=c("bolívar",
                                          "patria",
                                          "Latinoamérica",
                                          "república",
                                          "Venezuela",
                                          "colonia")
)


production <- tibble::tibble(category="production",
                                    word=c("crecimiento",
                                      "desarrollo",
                                      "recursos",
                                      "población",
                                      "economía",
                                      "petróleo")
)



# Combine the categories into one master list
keywordMasterList <- rbind(surplusValueList, objectiveTheoryOfValue, subjectiveTheoryOfValue, incomeOrWealthRedist, publicGoods, nationalism, production)

write_csv(x = keywordMasterList, path = paste(pathForOutput,"/keywords.csv", sep="") )

}
