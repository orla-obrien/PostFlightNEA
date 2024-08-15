
# Change your WD
setwd("~/Documents/R_Work_Directory/NEA_PostFlight")

# after changing your WD, run the pair of lines that corresponds to the report you are trying to make

source(".//PostFlightReport_InProgress.R")

rmarkdown::render(".//post_flight_email.Rmd", output_format = "pdf_document",
                  output_file = paste0("./output/", text.date,"New England Aquarium Aerial Survey Report", ".pdf"))



source(".//PostFlightReport_InProgress.R")

rmarkdown::render(".//Mainepost_flight_email.Rmd", output_format = "pdf_document",
                  output_file = paste0("./output/", text.date,"New England Aquarium Aerial Survey Report", ".pdf"))


source(".//PostFlightReport_InProgress.R")

rmarkdown::render(".//MonumentPostFlight.Rmd", output_format = "pdf_document",
                  output_file = paste0("./output/", text.date,"New England Aquarium Aerial Survey Report", ".pdf"))
