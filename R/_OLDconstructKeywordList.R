################################################################
# constructKeywordList.R
#
# Creates a keyword list using the words in this file.
################################################################


#' @export
constructKeywordList <- function(pathForOutput){

  # # # #### FOR TESTING #########
      pathForOutput <- "keywords/"
  # # # #### FOR TESTING

  # Exract the pdfSetNames from the fielpaths



# First create each category of keywords
surplusValueList <- tibble::tibble(category= "surplus value",
                           word=c("plusvalía",
                                  "sobreprecio",
                                  "explotación",
                                  "aprovechar",
                                  "aprovecharse",
                                  "aprovechan",
                                  "abuso",
                                  "despojo",
                                  "apropiación",
                                  "usurpación",
                                  "obreros",
                                  "trabajo",
                                  "trabajador",
                                  "trabajades",
                                  "proletario",
                                  "proletariado",
                                  "capital",
                                  "capitalistas")
)




laborTheoryOfValue <- tibble::tibble(category="labor theory of value",
                             word=c("trabajo",
                                    "trabajador",
                                    "trabajades",
                                    "obrero",
                                    "costo",
                                    "valor")
)



subjectiveTheoryOfValue <- tibble::tibble(category="subjective theory of value",
                                  word=c("oferta",
                                         "demanda",
                                         "precio",
                                         "mercado",
                                         "justo",
                                         "injusto")
)


IncomeOrWealthRedist <- tibble::tibble(category="income or wealth redistribution",
                               word=c("distribución",
                                      "igualdad",
                                      "justicia",
                                      "reparto",
                                      "repartición",
                                      "redistribución",
                                      "desigualdad",
                                      "comunidad",
                                      "derecho",
                                      "derechs",
                                      "injusticia")
)


publicGoods <- tibble::tibble(category="public goods",
                      word=c("públicos",
                             "bienes",
                             "bienestar",
                             "servicios",
                             "salud",
                             "educación",
                             "gratuita",
                             "gratis",
                             "vivienda",
                             "todos",
                             "todas")
)

# Combine the categories into one master list
keywordMasterList <- rbind(surplusValueList, laborTheoryOfValue, subjectiveTheoryOfValue, IncomeOrWealthRedist, publicGoods)

write_csv(x = keywordMasterList, path = paste(pathForOutput,"/keywords.csv", sep="") )

}
