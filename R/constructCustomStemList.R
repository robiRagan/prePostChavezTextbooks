################################################################
# constructCustomStemList.R
#
# Creates a table of custom stems
################################################################


#' @export
constructCustomStemList <- function(){

  # # #### FOR TESTING #########
  # # #### FOR TESTING

  # Exract the pdfSetNames from the fielpaths



# First create each category of keywords
customStemList <- tibble::tibble(word= c("bolivariano",
                                          "bolivariana",
                                         "comunica",
                                         "comunicación",
                                         "comunicaciones",
                                         "convivencia",
                                          "crecimiento",
                                         "ciudades",
                                          "esclavizado",
                                          "esclavizada",
                                          "esclavizados",
                                         "gratuita",
                                         "hombros",
                                         "importación",
                                         "importaciones",
                                         "importamos",
                                         "injusticia",
                                         "injusticias",
                                         "latinoamérical",
                                         "latinoamericana",
                                         "latinoamericanas",
                                         "latinoamericano",
                                         "latinoamericanos",
                                         "libertador",
                                         "merced",
                                         "patriota",
                                        "patriotismo",
                                          "poblacional",
                                          "republicana",
                                          "republicano",
                                          "saluda",
                                         "saludado",
                                         "saludan",
                                         "saludo",
                                         "saludó",
                                         "saludos"),
                                 stem=c("boliv",
                                        "boliv",
                                        "comunic",
                                        "comunic",
                                        "comunic",
                                        "conviven",
                                        "crec",
                                        "ciud",
                                        "esclav",
                                        "esclav",
                                        "esclav",
                                        "gratis",
                                        "hombro",
                                        "importación",
                                        "importación",
                                        "importación",
                                        "injust",
                                        "injust",
                                        "latinoamer",
                                        "latinoamer",
                                        "latinoamer",
                                        "latinoamer",
                                        "latinoamer",
                                        "libertador",
                                        "merce",
                                        "patri",
                                        "patri",
                                        "poblacion",
                                        "republ",
                                        "republ",
                                        "saluda",
                                        "saluda",
                                        "saluda",
                                        "saluda",
                                        "saluda",
                                        "saluda")
)

#Dignostic
# print( customStemList, n=Inf )


customStemList




}
