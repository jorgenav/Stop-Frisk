# Carga de datos del Proyecto

if(!file.exists("2014.csv")) {
  download.file("http://www.nyc.gov/html/nypd/downloads/zip/analysis_and_planning/2014_sqf_csv.zip",
    "2014_sqf_csv")
  unzip("2014_sqf_csv")
}

SQFdata <- read.csv("2014.csv")

dim(SQFdata)
