# Reparto de variables


if(!file.exists("2014.csv")) {
        download.file("http://www.nyc.gov/html/nypd/downloads/zip/analysis_and_planning/2014_sqf_csv.zip",
                      "2014_sqf_csv")
        unzip("2014_sqf_csv")
}

SQFdata <- read.csv("2014.csv")


library(dplyr)
library(ggplot2)

SQFdata$perstop = as.integer(SQFdata$perstop)
hist(SQFdata$perstop, breaks = 87,  prob=T, xlim=c(0,87))
plotdist(SQFdata$perstop , histo = TRUE, demp = TRUE)


SQFdata$stopfact<- factor(SQFdata$perstop)
tab_stop <- as.data.frame(prop.table(table(SQFdata$stopfact))*100)
tab_stop
'"stopfact<- SQFdata %>% 
        group_by(stopfact) %>%
        summarise(count=n()) %>%
        arrange(desc(count))"'



# pct
# precint of stop 1 through 123
SQFdata$pct <- factor(SQFdata$pct)

pct<- SQFdata %>% 
        group_by(pct) %>%
        summarise(count=n()) %>%
        arrange(desc(count))
ggplot(aes(perstop), data = SQFdata) + geom_bar()

# ser_num
# UF-250 serial number

SQFdata %>% group_by(ser_num) %>%
        summarise(count=n()) %>%
        arrange(desc(count))

SQFdata$ser_num[which(is.na(SQFdata$ser_num))]

# city
# Condados
SQFdata$city <- factor(SQFdata$city, labels=c("Manhattan", "Brooklyn", "Bronx",
                                      "Queens", "Staten Island"))
SQFdata %>% group_by(city) %>%
        summarise(count=n(), porcentaje= count/sum(dim(SQFdata[1]))*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))

SQFdata$city[which(is.na(SQFdata$city))]

# searched
# la mayor parte de personas paradas son buscadas pero el tiempo medio es casi igual para buscados y no 
SQFdata %>% group_by(searched) %>%
        summarise(count=n(), porcentaje= count/sum(dim(SQFdata[1]))*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))

SQFdata$searched[which(is.na(SQFdata$searched))]

SQFdata %>% filter(perstop < 20) %>%
        ggplot(mapping = aes(x = perstop, fill = searched)) +
        geom_bar() +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0,20,2),labels = seq(0,20,2))

# pistol

SQFdata$pistol[which(SQFdata$pistol == " ")] <- NA
SQFdata$pistol[which(SQFdata$pistol == "1")] <- "Y"
SQFdata$pistol <- factor(SQFdata$pistol, levels = c('Y','N'))

SQFdata %>% group_by(pistol) %>%
        summarise(count=n(), porcentaje= count/sum(dim(SQFdata[1]))*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))

SQFdata %>% 
        ggplot(mapping = aes(x = perstop, fill = pistol)) +
        geom_bar() +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0,87,1),labels = seq(0,87,1))

# perobs
# 
SQFdata %>% group_by(perobs) %>%
        summarise(count=n())%>%
        arrange(desc(count))

SQFdata$perobs[which(is.na(SQFdata$perobs))]

SQFdata %>% 
        ggplot(aes(perobs, perstop)) +
        geom_point() +
        labs(x="período de observación ", y="Período de parada") +
        geom_smooth(method = 'lm') +
        scale_size_area() +
        ggtitle("Relación entre tiempo de observación y parada")


# uso de fuerza fisica
# 9714 NA's
# hay más parados sin que se haya usado la fuerza y el comparar medias no distantan mucho entre grupos
SQFdata$pf_hcuff[which(SQFdata$pf_hcuff == " ")] <- NA
SQFdata$pf_hcuff[which(SQFdata$pf_hcuff == "1")] <- "Y"
SQFdata$pf_hcuff <- factor(SQFdata$pf_hcuff, levels = c('Y','N'))

SQFdata %>% group_by(pf_hcuff) %>%
        summarise(count=n(), porcentaje= count/sum(dim(SQFdata[1]))*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))
      

SQFdata %>% filter(perstop < 88) %>%
        ggplot(mapping = aes(x = perstop, fill = pf_hcuff)) +
        geom_bar() +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0,88,1),labels = seq(0,88,1))

# pf_pepsp
# 10857 NAs
# se podría decir que en promedio se demoran el mismo tiempo si usan o no gas pimienta
SQFdata$pf_pepsp[which(SQFdata$pf_pepsp == " ")] <- NA
SQFdata$pf_pepsp[which(SQFdata$pf_pepsp == "1")] <- "Y"
SQFdata$pf_pepsp <- factor(SQFdata$pf_pepsp, levels = c('Y','N'))

SQFdata %>% group_by(pf_pepsp) %>%
        summarise(count=n(), porcentaje= count/sum(dim(SQFdata[1]))*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))


SQFdata %>% filter(perstop < 88) %>%
        ggplot(mapping = aes(x = perstop, fill = pf_hcuff)) +
        geom_bar() +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0,88,1),labels = seq(0,88,1))

# cs_drgtr
# 10037 NAs
SQFdata$cs_drgtr[which(SQFdata$cs_drgtr == " ")] <- NA
SQFdata$cs_drgtr[which(SQFdata$cs_drgtr == "1")] <- "Y"
SQFdata$cs_drgtr <- factor(SQFdata$cs_drgtr, levels = c('Y','N'))

SQFdata %>% group_by(cs_drgtr) %>%
        summarise(count=n(), porcentaje= count/sum(dim(SQFdata[1]))*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))


SQFdata %>% filter(perstop < 88) %>%
        ggplot(mapping = aes(x = perstop, fill = cs_drgtr)) +
        geom_bar() +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0,88,1),labels = seq(0,88,1))



# cs_furtv
# 6479 NAs
SQFdata$cs_furtv[which(SQFdata$cs_furtv == " ")] <- NA
SQFdata$cs_furtv[which(SQFdata$cs_furtv == "1")] <- "Y"
SQFdata$cs_furtv <- factor(SQFdata$cs_furtv, levels = c('Y','N'))

SQFdata %>% group_by(cs_furtv) %>%
        summarise(count=n(), porcentaje= count/sum(dim(SQFdata[1]))*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))


SQFdata %>% filter(perstop < 88) %>%
        ggplot(mapping = aes(x = perstop, fill = cs_furtv)) +
        geom_bar() +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0,88,1),labels = seq(0,88,1))

# cs_vcrim
# 9709 NAs
# Corregir NAs y unificar valores en rf_vcrim
SQFdata$rf_vcrim[which(SQFdata$rf_vcrim == " ")] <- NA
SQFdata$rf_vcrim[which(SQFdata$rf_vcrim == "1")] <- "Y"
SQFdata$rf_vcrim <- factor(SQFdata$rf_vcrim, levels = c('Y','N'))

SQFdata %>% group_by(rf_vcrim) %>%
        summarise(count=n(), porcentaje= count/sum(dim(SQFdata[1]))*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))


SQFdata %>% filter(perstop < 88) %>%
        ggplot(mapping = aes(x = perstop, fill = rf_vcrim)) +
        geom_bar() +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0,88,1),labels = seq(0,88,1))

# rf_rfcmp
# 9475 NAs
SQFdata$rf_rfcmp[which(SQFdata$rf_rfcmp == " ")] <- NA
SQFdata$rf_rfcmp[which(SQFdata$rf_rfcmp == "1")] <- "Y"
SQFdata$rf_rfcmp <- factor(SQFdata$rf_rfcmp, levels = c('Y','N'))

SQFdata %>% filter(frisked=="Y") %>%
        group_by(rf_rfcmp) %>%
        summarise(count=n(), 
                  porcentaje= count/sum(dim(SQFdata[1]))*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))

SQFdata %>% filter(frisked=="Y"& perstop < 88) %>%
        ggplot(mapping = aes(x = perstop, fill = rf_rfcmp)) +
        geom_bar() +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0,88,1),labels = seq(0,88,1))
# rf_verbl
# 10741 NAs

SQFdata$rf_verbl[which(SQFdata$rf_verbl == " ")] <- NA
SQFdata$rf_verbl[which(SQFdata$rf_verbl == "1")] <- "Y"
SQFdata$rf_verbl <- factor(SQFdata$rf_verbl, levels = c('Y','N'))

frisked<- SQFdata%>% filter(frisked=="Y")
count(frisked)
SQFdata %>% filter(frisked=="Y") %>%
        group_by(rf_verbl) %>%
        summarise(contar=n(),
                  porcentaje= contar/30345*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))

SQFdata %>% filter(frisked=="Y"& perstop < 88) %>%
        ggplot(mapping = aes(x = perstop, fill = rf_verbl)) +
        geom_bar() +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0,88,1),labels = seq(0,88,1))

# sb_other
# 9981 NAs
SQFdata$sb_other[which(SQFdata$sb_other == " ")] <- NA
SQFdata$sb_other[which(SQFdata$sb_other == "1")] <- "Y"
SQFdata$sb_other <- factor(SQFdata$sb_other, levels = c('Y','N'))

searched<- SQFdata%>% filter(searched=="Y")
count(searched)
SQFdata %>% filter(searched=="Y") %>%
        group_by(rf_verbl) %>%
        summarise(contar=n(),
                  porcentaje= contar/7283*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))

SQFdata %>% filter(searched=="Y"& perstop < 88) %>%
        ggplot(mapping = aes(x = perstop, fill = sb_other)) +
        geom_bar() +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0,88,1),labels = seq(0,88,1))


# ac_proxm
# 7566 NAs
SQFdata$ac_proxm[which(SQFdata$ac_proxm == " ")] <- NA
SQFdata$ac_proxm[which(SQFdata$ac_proxm == "1")] <- "Y"
SQFdata$ac_proxm <- factor(SQFdata$ac_proxm, levels = c('Y','N'))

SQFdata %>% 
        group_by(ac_proxm) %>%
        summarise(contar=n(),
                  porcentaje= contar/sum(dim(SQFdata[1]))*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))

SQFdata %>% filter(perstop < 88) %>%
        ggplot(mapping = aes(x = perstop, fill = ac_proxm)) +
        geom_bar() +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0,88,1),labels = seq(0,88,1))

# ac_rept
# 8272 NAs
SQFdata$ac_rept[which(SQFdata$ac_rept == " ")] <- NA
SQFdata$ac_rept[which(SQFdata$ac_rept == "1")] <- "Y"
SQFdata$ac_rept <- factor(SQFdata$ac_rept, levels = c('Y','N'))

SQFdata %>% 
        group_by(ac_rept) %>%
        summarise(contar=n(),
                  porcentaje= contar/sum(dim(SQFdata[1]))*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))

SQFdata %>% filter(perstop < 88) %>%
        ggplot(mapping = aes(x = perstop, fill = ac_rept)) +
        geom_bar() +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0,88,1),labels = seq(0,88,1))

# inout
# 
SQFdata$inout <- factor(SQFdata$inout, labels=c("inside", "outside"))

SQFdata %>% 
        group_by(inout) %>%
        summarise(contar=n(),
                  porcentaje= contar/sum(dim(SQFdata[1]))*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))

SQFdata %>% filter(perstop < 88) %>%
        ggplot(mapping = aes(x = perstop, fill = inout)) +
        geom_bar() +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0,88,1),labels = seq(0,88,1))

# trhsloc
# 

dat$trhsloc <- factor(dat$trhsloc+1L, labels=c("neither",
                                               "housing authority", "transit authority"))

SQFdata %>% 
        group_by(trhsloc) %>%
        summarise(contar=n(),
                  porcentaje= contar/sum(dim(SQFdata[1]))*100,
                  tiempStop= sum(perstop),
                  media=mean(perstop))

SQFdata %>% filter(perstop < 88) %>%
        ggplot(mapping = aes(x = perstop, fill = thrsloc)) +
        geom_bar() +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0,88,1),labels = seq(0,88,1))


# addrpct
# location of address precinct -> Es interesante?


'"SQFdata %>% group_by(addrpct) %>%
        summarise(count=n())%>%
        arrange(desc(count))
SQFdata$addrpct[which(is.na(SQFdata$addrpct))]"'

# repcmd
# reporting officer's command -> Es interesante?

SQFdata %>% group_by(repcmd) %>%
        summarise(count=n())%>%
        arrange(desc(count))

SQFdata$repcmd[which(is.na(SQFdata$repcmd))]

# revcmd
# reviewing officer's command -> Es interesante?

SQFdata %>% group_by(revcmd) %>%
        summarise(count=n())%>%
        arrange(desc(count))

SQFdata$revcmd[which(is.na(SQFdata$revcmd))]

is.factor(SQFdata$frisked)
