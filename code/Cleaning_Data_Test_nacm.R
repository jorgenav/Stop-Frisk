# title: "Data mining project with NYC Stop&Frisk data"
# author: "iartalejo"

# CARGA DE DATOS DEL PROYECTO

if(!file.exists("2014.csv")) {
        download.file("http://www.nyc.gov/html/nypd/downloads/zip/analysis_and_planning/2015_sqf_csv.zip",
                      "2015_sqf_csv")
        unzip("2015_sqf_csv.zip")
}

if (!"lubridate" %in% installed.packages()){
        install.packages("lubridate")
}

SQFdata_2015 <- read.csv("./2015_sqf_csv/2015_sqf_csv.csv")

# LIMPIEZA DE DATOS

# Corrección inout
SQFdata_2015$inout <- factor(SQFdata_2015$inout, levels = c('I','O'), labels=c('Inside', 'Outside'))

# Corrección trhsloc
SQFdata_2015$trhsloc <- factor(SQFdata_2015$trhsloc, levels = c("H", "P", "T"),
                          labels=c('Housing', 'Neither', 'Transit'))

# Corrección perstop
SQFdata_2015$perstop = as.integer(SQFdata_2015$perstop)

# Corrección typeofid
SQFdata_2015$typeofid <- factor(SQFdata_2015$typeofid, levels = c("O", "P", "R", "V"),
                           labels=c('Other', 'Photo', 'Refused', 'Verbal'))

# Corregir NAs y unificar valores en adtlrept
SQFdata_2015$adtlrept[which(SQFdata_2015$adtlrept == " ")] <- NA

# Corregir NAs y unificar valores en pistol
SQFdata_2015$pistol[which(SQFdata_2015$pistol == " ")] <- NA
SQFdata_2015$pistol[which(SQFdata_2015$pistol == "1")] <- "Y"
SQFdata_2015$pistol <- factor(SQFdata_2015$pistol, levels = c('Y','N'))

# Corregir NAs y unificar valores en knifcuti
SQFdata_2015$knifcuti[which(SQFdata_2015$knifcuti == " ")] <- NA
SQFdata_2015$knifcuti[which(SQFdata_2015$knifcuti == "1")] <- "Y"
SQFdata_2015$knifcuti <- factor(SQFdata_2015$knifcuti, levels = c('Y','N'))

# Corregir NAs y unificar valores en radio
SQFdata_2015$radio[which(SQFdata_2015$radio == "0")] <- "N"
SQFdata_2015$radio[which(SQFdata_2015$radio == "1")] <- "Y"
SQFdata_2015$radio <- factor(SQFdata_2015$radio, levels = c('Y','N'))

# Corregir NAs y unificar valores en forceuse
SQFdata_2015$forceuse[which(SQFdata_2015$forceuse == " ")] <- NA
SQFdata_2015$forceuse <- factor(SQFdata_2015$forceuse, levels = c("DO", "DS", "OR", "OT", "SF", "SW"),
                           labels=c('Defense of other', 'Defense of self', 'Overcome resistence',
                                    'Other', 'Suspected flight', 'Suspected weapon'))

# Corrección sex
SQFdata_2015$sex <- factor(SQFdata_2015$sex, levels = c("F", "M", "Z"), labels=c('Female', 'Male', 'Unknown'))

# Corrección race
SQFdata_2015$race <- factor(SQFdata_2015$race, levels = c("A", "B", "I", "P", "Q", "U", "W", "Z"),
                       labels=c('ASIAN/PACIFIC ISLANDER', 'BLACK', 'AMERICAN INDIAN/ALASKAN NATIVE',
                                'BLACK-HISPANIC', 'WHITE-HISPANIC', 'UNKNOWN', 'WHITE', 'OTHER'))

# Corregir NAs y unificar valores en age
SQFdata_2015$age[which(SQFdata_2015$age == "**")] <- NA
SQFdata_2015$age = as.integer(sub("([[:space:]])","",SQFdata_2015$age))
SQFdata_2015$age[which(SQFdata_2015$age < 10 | SQFdata_2015$age > 90)] <- NA

# Corrección eyecolor
SQFdata_2015$eyecolor <- factor(SQFdata_2015$eyecolor, levels = c("BK","BL","BR","DF","GR","GY","HA","MA","MC","P","VI","XX","Z"),
                           labels=c('Black', 'Blue', 'Brown', 'Two different', 'Green', 'Gray',
                                    'Hazel', 'Maroon', 'MC', 'Pink', 'Violet', 'Unknown', 'Other'))

# Corrección haircolr
SQFdata_2015$haircolr <- factor(SQFdata_2015$haircolr, levels = c("BA","BK","BL","BR","DY","FR","GY","RA","SN","SP","WH","XX","ZZ"),
                           labels=c('Bald', 'Black', 'Blond', 'Brown', 'Dyed', 'Frosted', 'Gray',
                                    'Red', 'Sandy', 'Salt and pepper', 'White', 'Unknown', 'Other'))

# Corrección build
SQFdata_2015$build <- factor(SQFdata_2015$build, levels = c("H", "M", "T", "U", "Z"),
                        labels=c('Heavy', 'Medium', 'Thin', 'Muscular', 'Unknown'))



data_2015 <- dplyr::select(SQFdata_2015,
                           arstmade,
                           inout,  
                           trhsloc,
                           perobs,
                           #crimsusp,#tiene varias categoría
                           perstop,
                           typeofid,
                           explnstp,
                           othpers,
                           #arstoffn,#tiene varias categoría
                           sumissue,
                           #sumoffen, #NA
                           offunif,
                           frisked,
                           searched,
                           contrabn,
                           adtlrept, #WERE ADDITIONAL REPORTS PREPARED ?
                           pistol,
                           #riflshot, no avanza el modelo logit pocos "Y"
                           #asltweap, no avanza el modelo logit pocos "Y"
                           knifcuti,
                           #machgun,
                           #othrweap, no avanza el modelo logit pocos "Y"
                           radio,
                           forceuse,
                           sex,
                           race,
                           age,
                           ht_feet,
                           ht_inch,
                           weight,
                           haircolr,
                           #eyecolor, TENGO que reducir las clases
                           build,
                           #othfeatr,
                           city
)


data_2015$forceuse <- as.character(data_2015$forceuse)
data_2015$forceuse[is.na(data_2015$forceuse)] <- "NoFuerza"
data_2015$forceuse <- as.factor(data_2015$forceuse)

data_2015$adtlrept <- as.character(data_2015$adtlrept)
data_2015$adtlrept[is.na(data_2015$adtlrept)] <- "Y"
data_2015$adtlrept <- as.factor(data_2015$adtlrept)

data_2015$city <- as.character(data_2015$city)
data_2015$city[which(data_2015$city == "STATEN IS")] <- "STATEN ISLAND"
data_2015$city <- as.factor(data_2015$city)
data_2015$city<- factor(data_2015$city, labels=c('BRONX','BROOKLYN', 'MANHATTAN', 'QUEENS', "STATEN ISLAND"))

