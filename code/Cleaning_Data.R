# title: "Data mining project with NYC Stop&Frisk data"
# author: "iartalejo"

# CARGA DE DATOS DEL PROYECTO

if(!file.exists("2014.csv")) {
  download.file("http://www.nyc.gov/html/nypd/downloads/zip/analysis_and_planning/2014_sqf_csv.zip",
                "2014_sqf_csv")
  unzip("2014_sqf_csv")
}

SQFdata <- read.csv("2014.csv")

# LIMPIEZA DE DATOS

## Tratamiento fechas y horas

#install.packages("lubridate")
library(lubridate)

# Variable datestop
SQFdata$datestop <- mdy(SQFdata$datestop)  #Convert Strings to Dates

# Variable timestop
myfunc_time <- function(x){
    if(nchar(x)==3){x <- paste('0', as.character(x),sep='')}
    if(nchar(x)==2){x <- paste('0', '0', as.character(x),sep='')}
    if(nchar(x)==1){x <- paste('0', '0', '0', as.character(x),sep='')}
    return(as.character(strptime(x, format ='%H%M'),format='%H:%M'))
  }
SQFdata$timestop <- sapply(SQFdata$timestop,myfunc_time)
SQFdata$timestop <- hm(SQFdata$timestop)  #Convert Strings to Times

# Variable dob
SQFdata$dob <- mdy(SQFdata$dob)  #Convert to Dates

## Unificación valores

# Corrección perstop
SQFdata$perstop = as.integer(SQFdata$perstop)

# Corregir NAs y unificar valores en pistol
SQFdata$pistol[which(SQFdata$pistol == " ")] <- NA
SQFdata$pistol[which(SQFdata$pistol == "1")] <- "Y"
SQFdata$pistol <- factor(SQFdata$pistol, levels = c('Y','N'))

# Corregir NAs y unificar valores en riflshot
SQFdata$riflshot[which(SQFdata$riflshot == " ")] <- NA
SQFdata$riflshot[which(SQFdata$riflshot == "1")] <- "Y"
SQFdata$riflshot <- factor(SQFdata$riflshot, levels = c('Y','N'))

# Corregir NAs y unificar valores en asltweap
SQFdata$asltweap[which(SQFdata$asltweap == " ")] <- NA
SQFdata$asltweap[which(SQFdata$asltweap == "1")] <- "Y"
SQFdata$asltweap <- factor(SQFdata$asltweap, levels = c('Y','N'))

# Corregir NAs y unificar valores en knifcuti
SQFdata$knifcuti[which(SQFdata$knifcuti == " ")] <- NA
SQFdata$knifcuti[which(SQFdata$knifcuti == "1")] <- "Y"
SQFdata$knifcuti <- factor(SQFdata$knifcuti, levels = c('Y','N'))

# Corregir NAs y unificar valores en machgun
SQFdata$machgun[which(SQFdata$machgun == " ")] <- NA
SQFdata$machgun <- factor(SQFdata$machgun, levels = c('N'))

# Corregir NAs y unificar valores en othrweap
SQFdata$othrweap[which(SQFdata$othrweap == " ")] <- NA
SQFdata$othrweap[which(SQFdata$othrweap == "1")] <- "Y"
SQFdata$othrweap <- factor(SQFdata$othrweap, levels = c('Y','N'))

# Corregir NAs y unificar valores en pf_hands
SQFdata$pf_hands[which(SQFdata$pf_hands == " ")] <- NA
SQFdata$pf_hands[which(SQFdata$pf_hands == "1")] <- "Y"
SQFdata$pf_hands <- factor(SQFdata$pf_hands, levels = c('Y','N'))

# Corregir NAs y unificar valores en pf_wall
SQFdata$pf_wall[which(SQFdata$pf_wall == " ")] <- NA
SQFdata$pf_wall[which(SQFdata$pf_wall == "1")] <- "Y"
SQFdata$pf_wall <- factor(SQFdata$pf_wall, levels = c('Y','N'))

# Corregir NAs y unificar valores en pf_grnd
SQFdata$pf_grnd[which(SQFdata$pf_grnd == " ")] <- NA
SQFdata$pf_grnd[which(SQFdata$pf_grnd == "1")] <- "Y"
SQFdata$pf_grnd <- factor(SQFdata$pf_grnd, levels = c('Y','N'))

# Corregir NAs y unificar valores en pf_drwep
SQFdata$pf_drwep[which(SQFdata$pf_drwep == " ")] <- NA
SQFdata$pf_drwep[which(SQFdata$pf_drwep == "1")] <- "Y"
SQFdata$pf_drwep <- factor(SQFdata$pf_drwep, levels = c('Y','N'))

# Corregir NAs y unificar valores en pf_ptwep
SQFdata$pf_ptwep[which(SQFdata$pf_ptwep == " ")] <- NA
SQFdata$pf_ptwep[which(SQFdata$pf_ptwep == "1")] <- "Y"
SQFdata$pf_ptwep <- factor(SQFdata$pf_ptwep, levels = c('Y','N'))

# Corregir NAs y unificar valores en pf_baton
SQFdata$pf_baton[which(SQFdata$pf_baton == " ")] <- NA
SQFdata$pf_baton[which(SQFdata$pf_baton == "1")] <- "Y"
SQFdata$pf_baton <- factor(SQFdata$pf_baton, levels = c('Y','N'))

# Corregir NAs y unificar valores en pf_hcuff
SQFdata$pf_hcuff[which(SQFdata$pf_hcuff == " ")] <- NA
SQFdata$pf_hcuff[which(SQFdata$pf_hcuff == "1")] <- "Y"
SQFdata$pf_hcuff <- factor(SQFdata$pf_hcuff, levels = c('Y','N'))

# Corregir NAs y unificar valores en pf_pepsp
SQFdata$pf_pepsp[which(SQFdata$pf_pepsp == " ")] <- NA
SQFdata$pf_pepsp[which(SQFdata$pf_pepsp == "1")] <- "Y"
SQFdata$pf_pepsp <- factor(SQFdata$pf_pepsp, levels = c('Y','N'))

# Corregir NAs y unificar valores en pf_other
SQFdata$pf_other[which(SQFdata$pf_other == " ")] <- NA
SQFdata$pf_other[which(SQFdata$pf_other == "1")] <- "Y"
SQFdata$pf_other <- factor(SQFdata$pf_other, levels = c('Y','N'))

# Corregir NAs y unificar valores en radio
SQFdata$radio[which(SQFdata$radio == "0")] <- "N"
SQFdata$radio[which(SQFdata$radio == "1")] <- "Y"
SQFdata$radio <- factor(SQFdata$radio, levels = c('Y','N'))

# Corregir NAs y unificar valores en ac_rept
SQFdata$ac_rept[which(SQFdata$ac_rept == " ")] <- NA
SQFdata$ac_rept[which(SQFdata$ac_rept == "1")] <- "Y"
SQFdata$ac_rept <- factor(SQFdata$ac_rept, levels = c('Y','N'))

# Corregir NAs y unificar valores en ac_inves
SQFdata$ac_inves[which(SQFdata$ac_inves == " ")] <- NA
SQFdata$ac_inves[which(SQFdata$ac_inves == "1")] <- "Y"
SQFdata$ac_inves <- factor(SQFdata$ac_inves, levels = c('Y','N'))

# Corregir NAs y unificar valores en rf_vcrim
SQFdata$rf_vcrim[which(SQFdata$rf_vcrim == " ")] <- NA
SQFdata$rf_vcrim[which(SQFdata$rf_vcrim == "1")] <- "Y"
SQFdata$rf_vcrim <- factor(SQFdata$rf_vcrim, levels = c('Y','N'))

# Corregir NAs y unificar valores en rf_othsw
SQFdata$rf_othsw[which(SQFdata$rf_othsw == " ")] <- NA
SQFdata$rf_othsw[which(SQFdata$rf_othsw == "1")] <- "Y"
SQFdata$rf_othsw <- factor(SQFdata$rf_othsw, levels = c('Y','N'))

# Corregir NAs y unificar valores en ac_proxm
SQFdata$ac_proxm[which(SQFdata$ac_proxm == " ")] <- NA
SQFdata$ac_proxm[which(SQFdata$ac_proxm == "1")] <- "Y"
SQFdata$ac_proxm <- factor(SQFdata$ac_proxm, levels = c('Y','N'))

# Corregir NAs y unificar valores en rf_attir
SQFdata$rf_attir[which(SQFdata$rf_attir == " ")] <- NA
SQFdata$rf_attir[which(SQFdata$rf_attir == "1")] <- "Y"
SQFdata$rf_attir <- factor(SQFdata$rf_attir, levels = c('Y','N'))

# Corregir NAs y unificar valores en cs_objcs
SQFdata$cs_objcs[which(SQFdata$cs_objcs == " ")] <- NA
SQFdata$cs_objcs[which(SQFdata$cs_objcs == "1")] <- "Y"
SQFdata$cs_objcs <- factor(SQFdata$cs_objcs, levels = c('Y','N'))

# Corregir NAs y unificar valores en cs_descr
SQFdata$cs_descr[which(SQFdata$cs_descr == " ")] <- NA
SQFdata$cs_descr[which(SQFdata$cs_descr == "1")] <- "Y"
SQFdata$cs_descr <- factor(SQFdata$cs_descr, levels = c('Y','N'))

# Corregir NAs y unificar valores en cs_casng
SQFdata$cs_casng[which(SQFdata$cs_casng == " ")] <- NA
SQFdata$cs_casng[which(SQFdata$cs_casng == "1")] <- "Y"
SQFdata$cs_casng <- factor(SQFdata$cs_casng, levels = c('Y','N'))

# Corregir NAs y unificar valores en cs_lkout
SQFdata$cs_lkout[which(SQFdata$cs_lkout == " ")] <- NA
SQFdata$cs_lkout[which(SQFdata$cs_lkout == "1")] <- "Y"
SQFdata$cs_lkout <- factor(SQFdata$cs_lkout, levels = c('Y','N'))

# Corregir NAs y unificar valores en rf_vcact
SQFdata$rf_vcact[which(SQFdata$rf_vcact == " ")] <- NA
SQFdata$rf_vcact[which(SQFdata$rf_vcact == "1")] <- "Y"
SQFdata$rf_vcact <- factor(SQFdata$rf_vcact, levels = c('Y','N'))

# Corregir NAs y unificar valores en cs_cloth
SQFdata$cs_cloth[which(SQFdata$cs_cloth == " ")] <- NA
SQFdata$cs_cloth[which(SQFdata$cs_cloth == "1")] <- "Y"
SQFdata$cs_cloth <- factor(SQFdata$cs_cloth, levels = c('Y','N'))

# Corregir NAs y unificar valores en cs_drgtr
SQFdata$cs_drgtr[which(SQFdata$cs_drgtr == " ")] <- NA
SQFdata$cs_drgtr[which(SQFdata$cs_drgtr == "1")] <- "Y"
SQFdata$cs_drgtr <- factor(SQFdata$cs_drgtr, levels = c('Y','N'))

# Corregir NAs y unificar valores en ac_evasv
SQFdata$ac_evasv[which(SQFdata$ac_evasv == " ")] <- NA
SQFdata$ac_evasv[which(SQFdata$ac_evasv == "1")] <- "Y"
SQFdata$ac_evasv <- factor(SQFdata$ac_evasv, levels = c('Y','N'))

# Corregir NAs y unificar valores en ac_assoc
SQFdata$ac_assoc[which(SQFdata$ac_assoc == " ")] <- NA
SQFdata$ac_assoc[which(SQFdata$ac_assoc == "1")] <- "Y"
SQFdata$ac_assoc <- factor(SQFdata$ac_assoc, levels = c('Y','N'))

# Corregir NAs y unificar valores en cs_furtv
SQFdata$cs_furtv[which(SQFdata$cs_furtv == " ")] <- NA
SQFdata$cs_furtv[which(SQFdata$cs_furtv == "1")] <- "Y"
SQFdata$cs_furtv <- factor(SQFdata$cs_furtv, levels = c('Y','N'))

# Corregir NAs y unificar valores en rf_rfcmp
SQFdata$rf_rfcmp[which(SQFdata$rf_rfcmp == " ")] <- NA
SQFdata$rf_rfcmp[which(SQFdata$rf_rfcmp == "1")] <- "Y"
SQFdata$rf_rfcmp <- factor(SQFdata$rf_rfcmp, levels = c('Y','N'))

# Corregir NAs y unificar valores en ac_cgdir
SQFdata$ac_cgdir[which(SQFdata$ac_cgdir == " ")] <- NA
SQFdata$ac_cgdir[which(SQFdata$ac_cgdir == "1")] <- "Y"
SQFdata$ac_cgdir <- factor(SQFdata$ac_cgdir, levels = c('Y','N'))

# Corregir NAs y unificar valores en rf_verbl
SQFdata$rf_verbl[which(SQFdata$rf_verbl == " ")] <- NA
SQFdata$rf_verbl[which(SQFdata$rf_verbl == "1")] <- "Y"
SQFdata$rf_verbl <- factor(SQFdata$rf_verbl, levels = c('Y','N'))

# Corregir NAs y unificar valores en cs_vcrim
SQFdata$cs_vcrim[which(SQFdata$cs_vcrim == " ")] <- NA
SQFdata$cs_vcrim[which(SQFdata$cs_vcrim == "1")] <- "Y"
SQFdata$cs_vcrim <- factor(SQFdata$cs_vcrim, levels = c('Y','N'))

# Corregir NAs y unificar valores en cs_bulge
SQFdata$cs_bulge[which(SQFdata$cs_bulge == " ")] <- NA
SQFdata$cs_bulge[which(SQFdata$cs_bulge == "1")] <- "Y"
SQFdata$cs_bulge <- factor(SQFdata$cs_bulge, levels = c('Y','N'))

# Corregir NAs y unificar valores en cs_other
SQFdata$cs_other[which(SQFdata$cs_other == " ")] <- NA
SQFdata$cs_other[which(SQFdata$cs_other == "1")] <- "Y"
SQFdata$cs_other <- factor(SQFdata$cs_other, levels = c('Y','N'))

# Corregir NAs y unificar valores en ac_incid
SQFdata$ac_incid[which(SQFdata$ac_incid == " ")] <- NA
SQFdata$ac_incid[which(SQFdata$ac_incid == "1")] <- "Y"
SQFdata$ac_incid <- factor(SQFdata$ac_incid, levels = c('Y','N'))

# Corregir NAs y unificar valores en ac_time
SQFdata$ac_time[which(SQFdata$ac_time == " ")] <- NA
SQFdata$ac_time[which(SQFdata$ac_time == "1")] <- "Y"
SQFdata$ac_time <- factor(SQFdata$ac_time, levels = c('Y','N'))

# Corregir NAs y unificar valores en rf_knowl
SQFdata$rf_knowl[which(SQFdata$rf_knowl == " ")] <- NA
SQFdata$rf_knowl[which(SQFdata$rf_knowl == "1")] <- "Y"
SQFdata$rf_knowl <- factor(SQFdata$rf_knowl, levels = c('Y','N'))

# Corregir NAs y unificar valores en ac_stsnd
SQFdata$ac_stsnd[which(SQFdata$ac_stsnd == " ")] <- NA
SQFdata$ac_stsnd[which(SQFdata$ac_stsnd == "1")] <- "Y"
SQFdata$ac_stsnd <- factor(SQFdata$ac_stsnd, levels = c('Y','N'))

# Corregir NAs y unificar valores en ac_other
SQFdata$ac_other[which(SQFdata$ac_other == " ")] <- NA
SQFdata$ac_other[which(SQFdata$ac_other == "1")] <- "Y"
SQFdata$ac_other <- factor(SQFdata$ac_other, levels = c('Y','N'))

# Corregir NAs y unificar valores en sb_hdobj
SQFdata$sb_hdobj[which(SQFdata$sb_hdobj == " ")] <- NA
SQFdata$sb_hdobj[which(SQFdata$sb_hdobj == "1")] <- "Y"
SQFdata$sb_hdobj <- factor(SQFdata$sb_hdobj, levels = c('Y','N'))

# Corregir NAs y unificar valores en sb_outln
SQFdata$sb_outln[which(SQFdata$sb_outln == " ")] <- NA
SQFdata$sb_outln[which(SQFdata$sb_outln == "1")] <- "Y"
SQFdata$sb_outln <- factor(SQFdata$sb_outln, levels = c('Y','N'))

# Corregir NAs y unificar valores en sb_admis
SQFdata$sb_admis[which(SQFdata$sb_admis == " ")] <- NA
SQFdata$sb_admis[which(SQFdata$sb_admis == "1")] <- "Y"
SQFdata$sb_admis <- factor(SQFdata$sb_admis, levels = c('Y','N'))

# Corregir NAs y unificar valores en sb_other
SQFdata$sb_other[which(SQFdata$sb_other == " ")] <- NA
SQFdata$sb_other[which(SQFdata$sb_other == "1")] <- "Y"
SQFdata$sb_other <- factor(SQFdata$sb_other, levels = c('Y','N'))

# Corregir NAs y unificar valores en rf_furt
SQFdata$rf_furt[which(SQFdata$rf_furt == " ")] <- NA
SQFdata$rf_furt[which(SQFdata$rf_furt == "1")] <- "Y"
SQFdata$rf_furt <- factor(SQFdata$rf_furt, levels = c('Y','N'))

# Corregir NAs y unificar valores en rf_bulg
SQFdata$rf_bulg[which(SQFdata$rf_bulg == " ")] <- NA
SQFdata$rf_bulg[which(SQFdata$rf_bulg == "1")] <- "Y"
SQFdata$rf_bulg <- factor(SQFdata$rf_bulg, levels = c('Y','N'))

# Corregir NAs y unificar valores en othfeatr
SQFdata$othfeatr[which(SQFdata$othfeatr == " ")] <- NA

# Corregir NAs y unificar valores en addrnum
SQFdata$addrnum[which(SQFdata$addrnum == " ")] <- NA

# Corregir NAs y unificar valores en adtlrept
SQFdata$adtlrept[which(SQFdata$adtlrept == " ")] <- NA

# Corregir NAs y unificar valores en age
SQFdata$age[which(SQFdata$age == "**")] <- NA
SQFdata$age = as.integer(sub("([[:space:]])","",SQFdata$age))
SQFdata$age[which(SQFdata$age < 10 | SQFdata$age > 90)] <- NA

# Corregir NAs y unificar valores en arstoffn
SQFdata$arstoffn[which(SQFdata$arstoffn == " ")] <- NA

# Corregir NAs y unificar valores en crossst
SQFdata$crossst[which(SQFdata$crossst == " ")] <- NA

# Corregir NAs y unificar valores en forceuse
SQFdata$forceuse[which(SQFdata$forceuse == " ")] <- NA
SQFdata$forceuse <- factor(SQFdata$forceuse, levels = c(" ", "DO", "DS", "OR", "OT", "SF", "SW"),
                           labels=c('NA', 'Defense of other', 'Defense of self', 'Overcome resistence',
                                    'Other', 'Suspected flight', 'Suspected weapon'))

# Corregir NAs y unificar valores en officrid
SQFdata$officrid[which(SQFdata$officrid == " ")] <- NA
SQFdata$officrid <- factor(SQFdata$officrid, levels = c(" ", "I", "0"), labels=c('NA', 'Id', 'No'))

# Corregir NAs y unificar valores en offshld
SQFdata$offshld[which(SQFdata$offshld == " ")] <- NA
SQFdata$offshld <- factor(SQFdata$offshld, levels = c(" ", "S", "0"), labels=c('NA', 'Shield', 'No'))

# Corregir NAs y unificar valores en offverb
SQFdata$offverb[which(SQFdata$offverb == " ")] <- NA
SQFdata$offverb <- factor(SQFdata$offverb, levels = c(" ", "V", "0"), labels=c('NA', 'Verbal', 'No'))

# Corregir NAs y unificar valores en othfeatr
SQFdata$othfeatr[which(SQFdata$othfeatr == " ")] <- NA

# Corregir NAs y unificar valores en premname
SQFdata$premname[which(SQFdata$premname == " ")] <- NA

# Corregir NAs y unificar valores en sector
SQFdata$sector[which(SQFdata$sector == " ")] <- NA

# Corregir NAs y unificar valores en stinter
SQFdata$stinter[which(SQFdata$stinter == " ")] <- NA

# Corregir NAs y unificar valores en stname
SQFdata$stname[which(SQFdata$stname == " ")] <- NA

# Corregir NAs y unificar valores en sumoffen
SQFdata$sumoffen[which(SQFdata$sumoffen == " ")] <- NA

# Corrección pct
SQFdata$pct <- factor(SQFdata$pct)

# Corrección addrpct
SQFdata$addrpct <- factor(SQFdata$addrpct)

# Corrección beat
SQFdata$beat <- factor(SQFdata$beat)

# Corrección post
SQFdata$post <- factor(SQFdata$post)

# Corrección detailCM
SQFdata$detailCM <- factor(SQFdata$detailCM)

# Corrección inout
SQFdata$inout <- factor(SQFdata$inout, levels = c('I','O'), labels=c('Inside', 'Outside'))

# Corrección trhsloc
SQFdata$trhsloc <- factor(SQFdata$trhsloc, levels = c("H", "P", "T"),
                          labels=c('Housing', 'Neither', 'Transit'))

# Corrección typeofid
SQFdata$typeofid <- factor(SQFdata$typeofid, levels = c("O", "P", "R", "V"),
                           labels=c('Other', 'Photo', 'Refused', 'Verbal'))

# Corrección sex
SQFdata$sex <- factor(SQFdata$sex, levels = c("F", "M", "Z"), labels=c('Female', 'Male', 'Unknown'))

# Corrección race
SQFdata$race <- factor(SQFdata$race, levels = c("A", "B", "I", "P", "Q", "U", "W", "Z"),
                       labels=c('Asian', 'Black', 'American indian',
                                'Black-hispan', 'White-hispan', 'Unknown', 'White', 'Other'))

# Corrección eyecolor
SQFdata$eyecolor <- factor(SQFdata$eyecolor, levels = c("BK","BL","BR","DF","GR","GY","HA","MA","MC","P","VI","XX","Z"),
                       labels=c('Black', 'Blue', 'Brown', 'Two different', 'Green', 'Gray',
                                'Hazel', 'Maroon', 'MC', 'Pink', 'Violet', 'Unknown', 'Other'))

# Corrección haircolr
SQFdata$haircolr <- factor(SQFdata$haircolr, levels = c("BA","BK","BL","BR","DY","FR","GY","RA","SN","SP","WH","XX","ZZ"),
                       labels=c('Bald', 'Black', 'Blond', 'Brown', 'Dyed', 'Frosted', 'Gray',
                                'Red', 'Sandy', 'Salt and pepper', 'White', 'Unknown', 'Other'))

# Corrección build
SQFdata$build <- factor(SQFdata$build, levels = c("H", "M", "T", "U", "Z"),
                        labels=c('Heavy', 'Medium', 'Thin', 'Muscular', 'Unknown'))
                        
#summary(SQFdata)
