####################################################################################################
#################Program for creating monthly indices of pseudo-stocks from CEDEFOP-OJAs
#####################################################################################################


Sys.setenv(LANG = "en")

options(scipen = 999)

library(RColorBrewer)
#library(xlsx)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape2)
library(Hmisc)
library(data.table)

rm(list = ls())

gc()
gc()


################################### 30 day index ===========================


# get 2018 daylist ==========

daylist <- readRDS(file = "daylist_step3_30d_2018.rds")


# define the sequence of months as days ---------

seq1 <- as.Date(c("2018-08-1","2018-09-1","2018-10-1","2018-11-1","2018-12-1","2019-01-1","2019-02-1","2019-03-1","2019-04-1","2019-05-1","2019-06-1"))

seq2 <- as.Date(c("2018-08-31","2018-09-30","2018-10-31","2018-11-30","2018-12-31","2019-01-31","2019-02-28","2019-03-31", "2019-04-30", "2019-05-31", "2019-06-30"))


min(daylist[[1]]$grab_date)
max(daylist[[length(daylist)]]$grab_date)

dates <- data.frame(dates = seq(as.Date(names(daylist)[1])  ,  as.Date(names(daylist[length(daylist)]))  ,"days")) 

seq1 <- dates %>% group_by(quarter = quarter(dates))  %>% filter(dates==min(dates)) 
seq1 <- seq1$dates
seq1[1] <- seq1[1] + months(1)

seq2 <- dates %>% group_by(quarter = quarter(dates))  %>% filter(dates==max(dates)) 
seq2 <- seq2$dates


# - Loop over each day of a month and count the number of job ads which are active on this day
# - Take the monthly average over these daily job ad counts for each quarter

wmeans <- numeric()

for (k  in (1:length(seq1))) {
  period <- seq(seq1[k], seq2[k], 1)
  
  nvec <- numeric()
  j <- 1
  for (i in as.character(period)) {
    tmp <- daylist[[paste(i)]]
    nvec[j] <- count(tmp)$n
    
    j <- j + 1
  }
  
  wmeans[k] <- mean(nvec)
  
}

index <- wmeans/wmeans[1]


date <- (seq1)+14

# add JVS data for comparison

jvs <- read_excel("JVSdata.xls")
jvs <- subset(jvs, select = c("year", "quarter", "vacancies", "immediately"))
jvs <- arrange(jvs, year, quarter)
colnames(jvs) <- c("year", "quarter", "jvs", "jvs_urgent")
jvs$jvs <- jvs$jvs * 1000
jvs$jvs_urgent <- jvs$jvs_urgent  * 1000

wmat <- data.frame()
for (i in (1:2))   wmat <- rbind(wmat,jvs[1,])
for (i in (3:5))   wmat <- rbind(wmat,jvs[2,])
for (i in (6:8))   wmat <- rbind(wmat,jvs[3,])
for (i in (9:11))   wmat <- rbind(wmat,jvs[4,])

wmat$jvs <- wmat$jvs/wmat$jvs[1]
wmat$jvs_urgent <- wmat$jvs_urgent/wmat$jvs_urgent[1]

imat <- data.frame(cbind(wmat,date, cedefop_oja = index))


imat_long <- reshape2::melt(subset(imat, select = c(-year, -quarter)), id.vars = "date")


ggplot(imat_long)+
  geom_line(aes(x=date, y = value, color = variable))+
  labs( x = "month", y = "job ad index",
        title = "Job ads/vacancies for CEDEFOP-OJA and JVS",
        subtitle = "pseudo-stocks avg. over last 2 months each quarter",
        caption = "OJA posted max 30 days before the reference day and not yet expired.\njvs_urgent denotes vacancies which are to be filled immediately"   ) +
  theme(axis.text.x=element_text(angle=45,hjust=1,size=8))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  geom_point(aes(x=date, y = value, color = variable))

ggsave("Results/pseudostock_monthly_index_30.png", width = 15, height = 10, units = "cm")


ggplot(imat_long)+
  geom_line(aes(x=date, y = value, color = variable))+
  labs( x = "month", y = "job ad index"   ) +
  theme(axis.text.x=element_text(angle=45,hjust=1,size=8))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  geom_point(aes(x=date, y = value, color = variable))

ggsave("Results/headless/pseudostock_monthly_index_30.png", width = 15, height = 10, units = "cm")

############ add unemployment rate for comparison

ue <- read_excel("unemployment_timeline.xls", trim_ws = TRUE)

ue <- select(ue, -c( year, month, month_label, unemployment))

#ue$date <- dmy(ue$date)
ue$date <- ue$date+14

uemat <- left_join(imat, ue, by="date")

uemat$unemployment_rate <- uemat$unemployment_rate/uemat$unemployment_rate[1]

uemat <- select(uemat, -c(jvs, jvs_urgent))

uemat_long <- reshape2::melt(subset(uemat, select = c(-year, -quarter)), id.vars = "date")

#imat_long$date <- as.Date(imat_long$date)

ggplot(uemat_long)+
  geom_line(aes(x=date, y = value, color = variable))+
  labs( x = "month", y = "job ad index",
        title = "Job ads/vacancies for CEDEFOP-OJA and JVS",
        subtitle = "pseudo-stocks avg. over last 2 months each quarter",
        caption = "OJA posted max 30 days before the reference day and not yet expired."   ) +
  theme(axis.text.x=element_text(angle=45,hjust=1,size=8))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  geom_point(aes(x=date, y = value, color = variable))

ggsave("Results/ps_monthly_index_againstUER_30.png", width = 15, height = 10, units = "cm")


ggplot(uemat_long)+
  geom_line(aes(x=date, y = value, color = variable))+
  labs( x = "month", y = "job ad index"  ) +
  theme(axis.text.x=element_text(angle=45,hjust=1,size=8))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  geom_point(aes(x=date, y = value, color = variable))

ggsave("Results/headless/ps_monthly_index_againstUER_30.png", width = 15, height = 10, units = "cm")



############ And Employment rate  (pointless, too little variation)--------------

er <- read_excel("Auxiliary data/employment_timeline.xls", trim_ws = TRUE)

er$date <-ymd(paste(er$year, "-",er$month,"-",1))

er <- select(er, -c( year, month, month_name, wohnort))

colnames(er) <- c("employment", "date")

#ue$date <- dmy(ue$date)
er$date <- er$date+14

uemat <- left_join(imat, er, by="date")

uemat$employment <- uemat$employment/uemat$employment[1]

uemat <- select(uemat, -c(jvs, jvs_urgent))

uemat_long <- reshape2::melt(subset(uemat, select = c(-year, -quarter)), id.vars = "date")

#imat_long$date <- as.Date(imat_long$date)

ggplot(uemat_long)+
  geom_line(aes(x=date, y = value, color = variable))+
  labs( x = "month", y = "job ad index",
        title = "Job ads/vacancies for CEDEFOP-OJA and JVS",
        subtitle = "pseudo-stocks avg. over last 2 months each quarter",
        caption = "OJA posted max 30 days before the reference day and not yet expired."   ) +
  theme(axis.text.x=element_text(angle=45,hjust=1,size=8))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  geom_point(aes(x=date, y = value, color = variable))



ggplot(uemat_long)+
  geom_line(aes(x=date, y = value, color = variable))+
  labs( x = "month", y = "job ad index"  ) +
  theme(axis.text.x=element_text(angle=45,hjust=1,size=8))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  geom_point(aes(x=date, y = value, color = variable))


################################################################
######Add IFO Beschäftigungsbarometer, montly comparison ===================
##############################################

bbm <- read_excel("IFO_BBM.xls")
bbm$date <- as.Date(bbm$date)

#bbm <- select(bbm, -c( year, month))

cdf <- data.frame(date=seq1, cedefop_oja = index)
cdf$month <- month(cdf$date)
cdf$year <- year(cdf$date)
cdf <- subset(cdf, select = -date)

bmat <- left_join(cdf, bbm, by = c("year", "month"))

bmat$cedefop_oja <- bmat$cedefop_oja*100
  
#bmat$bbm_index <- bmat$bbm_index/bmat$bbm_index[1]


bmat_long <- reshape2::melt(subset(bmat, select = c(-year, -month)), id.vars = "date")

cedef <- subset(bmat_long, variable == "cedefop_oja")

bbm_ind <- subset(bmat_long, variable == "bbm_index")

#imat_long$date <- as.Date(imat_long$date)

ggplot(bmat_long)+
  geom_line(aes(x=date, y = value, color = variable))+
  labs( x = "month", y = "job ad index",
        title = "Job ads/vacancies for CEDEFOP-OJA and IFO-Beschäftigungsbarometer",
        subtitle = "pseudo-stocks avg. over last 2 months each quarter",
        caption = "OJA posted max 30 days before the reference day and not yet expired."   ) +
  theme(axis.text.x=element_text(angle=90,hjust=1,size=7))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  geom_point(aes(x=date, y = value, color = variable))

ggsave("Results/ps_monthly_index_againstBBM_30.png", width = 20, height = 13.3, units = "cm")



################################### 40 day index ===========================

daylist <- readRDS(file = "daylist_step3_40d.Rdata")


# define the sequence of months as days ---------

seq1 <- as.Date(c("2018-08-1","2018-09-1","2018-10-1","2018-11-1","2018-12-1","2019-01-1","2019-02-1","2019-03-1"))

seq2 <- as.Date(c("2018-08-31","2018-09-30","2018-10-31","2018-11-30","2018-12-31","2019-01-31","2019-02-28","2019-03-31"))

#seq1 <- seq(as.Date("2018-08-"), as.Date("2019-03-1"), 31)
#seq2 <- seq(as.Date("2018-08-30"), as.Date("2019-03-30"), 31)

wmeans <- numeric()

for (k  in (1:length(seq1))) {
  period <- seq(seq1[k], seq2[k], 1)
  
  nvec <- numeric()
  j <- 1
  for (i in as.character(period)) {
    tmp <- daylist[[paste(i)]]
    nvec[j] <- count(tmp)$n
    
    j <- j + 1
  }
  
  wmeans[k] <- mean(nvec)
  
}

index <- wmeans/wmeans[1]


date <- seq1


jvs <- read.xlsx("JVSdata.xls", sheetIndex = 1)
jvs <- subset(jvs, select = c("year", "quarter", "vacancies", "immediately"))
jvs <- arrange(jvs, year, quarter)
colnames(jvs) <- c("year", "quarter", "jvs", "jvs_urgent")
jvs$jvs <- jvs$jvs * 1000
jvs$jvs_urgent <- jvs$jvs_urgent  * 1000

wmat <- data.frame()
for (i in (1:2))   wmat <- rbind(wmat,jvs[1,])
for (i in (3:5))   wmat <- rbind(wmat,jvs[2,])
for (i in (6:8))   wmat <- rbind(wmat,jvs[3,])

wmat$jvs <- wmat$jvs/wmat$jvs[1]
wmat$jvs_urgent <- wmat$jvs_urgent/wmat$jvs_urgent[1]

imat <- data.frame(cbind(wmat,date, cedefop_oja = index))

#imat$date <- as.character(imat$date)

imat_long <- melt(subset(imat, select = c(-year, -quarter)), id.vars = "date")

#imat_long$date <- as.Date(imat_long$date)


ggplot(imat_long)+
  geom_step(aes(x=date, y = value, color = variable))+
  labs( x = "month", y = "job ad index",
        title = "Job ads/vacancies for CEDEFOP-OJA and JVS",
        subtitle = "pseudo-stocks avg. over last 2 months each quarter",
        caption = "OJA posted max 40 days before the reference day and not yet expired.\njvs_urgent denotes vacancies which are to be filled immediately"   ) +
  theme(axis.text.x=element_text(angle=90,hjust=1,size=7))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")

ggsave("Results/pseudostock_monthly_index_40.png", width = 20, height = 13.3, units = "cm")


################################### 20 day index ===========================

daylist <- readRDS(file = "daylist_step3_20d.Rdata")


# define the sequence of months as days ---------

seq1 <- as.Date(c("2018-08-1","2018-09-1","2018-10-1","2018-11-1","2018-12-1","2019-01-1","2019-02-1","2019-03-1"))

seq2 <- as.Date(c("2018-08-31","2018-09-30","2018-10-31","2018-11-30","2018-12-31","2019-01-31","2019-02-28","2019-03-31"))

#seq1 <- seq(as.Date("2018-08-"), as.Date("2019-03-1"), 31)
#seq2 <- seq(as.Date("2018-08-30"), as.Date("2019-03-30"), 31)

wmeans <- numeric()

for (k  in (1:length(seq1))) {
  period <- seq(seq1[k], seq2[k], 1)
  
  nvec <- numeric()
  j <- 1
  for (i in as.character(period)) {
    tmp <- daylist[[paste(i)]]
    nvec[j] <- count(tmp)$n
    
    j <- j + 1
  }
  
  wmeans[k] <- mean(nvec)
  
}

index <- wmeans/wmeans[1]


date <- seq1


jvs <- read.xlsx("JVSdata.xls", sheetIndex = 1)
jvs <- subset(jvs, select = c("year", "quarter", "vacancies", "immediately"))
jvs <- arrange(jvs, year, quarter)
colnames(jvs) <- c("year", "quarter", "jvs", "jvs_urgent")
jvs$jvs <- jvs$jvs * 1000
jvs$jvs_urgent <- jvs$jvs_urgent  * 1000

wmat <- data.frame()
for (i in (1:2))   wmat <- rbind(wmat,jvs[1,])
for (i in (3:5))   wmat <- rbind(wmat,jvs[2,])
for (i in (6:8))   wmat <- rbind(wmat,jvs[3,])

wmat$jvs <- wmat$jvs/wmat$jvs[1]
wmat$jvs_urgent <- wmat$jvs_urgent/wmat$jvs_urgent[1]

imat <- data.frame(cbind(wmat,date, cedefop_oja = index))

#imat$date <- as.character(imat$date)

imat_long <- melt(subset(imat, select = c(-year, -quarter)), id.vars = "date")

#imat_long$date <- as.Date(imat_long$date)


ggplot(imat_long)+
  geom_step(aes(x=date, y = value, color = variable))+
  labs( x = "month", y = "job ad index",
        title = "Job ads/vacancies for CEDEFOP-OJA and JVS",
        subtitle = "pseudo-stocks avg. over last 2 months each quarter",
        caption = "OJA posted max 20 days before the reference day and not yet expired.\njvs_urgent denotes vacancies which are to be filled immediately"   ) +
  theme(axis.text.x=element_text(angle=90,hjust=1,size=7))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")

ggsave("Results/pseudostock_monthly_index_20.png", width = 20, height = 13.3, units = "cm")
