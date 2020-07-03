####################################################################################################
#################Program for creating outputs and plots from CEDEFOP Pseudostock data
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
library(parallel)
library(fst)

rm(list = ls())

#read in the CEDEFOP dataset 

dframe <- readRDS("OJVsample_step1_noimput_redux.rds")

setDT(dframe)

####################################################################################################################
# Daily frequency counts summed over each quarter (for comparison purposes only, these are not pseudo-stocks) =====
####################################################################################################################


subtab_q <- count(dframe, grab_date)

subtab_q <- subtab_q %>% mutate(qtr = paste0(year(grab_date), "-", "q", quarter(grab_date)))

grouptab_q <- aggregate(subtab_q$n, list(subtab_q$qtr), sum)
colnames(grouptab_q) <- c("date", "cedefop")

# load comparison data from JVS

jvs <- read_excel("JVSdata.xls")
jvs <- subset(jvs, select = c("year", "quarter", "vacancies"))
jvs <- arrange(jvs, year, quarter)
colnames(jvs) <- c("year", "quarter", "jvs")
jvs$jvs <- jvs$jvs * 1000


table_q <- cbind(grouptab_q, jvs[1:nrow(grouptab_q),])
table_q$difference <- table_q$cedefop - table_q$jvs

table_q_long <- reshape2::melt(subset(table_q, select = c(-year, -quarter, -difference)), id.vars = "date")

ggplot(table_q_long, aes(date, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + labs(
    x = "quarter", y = "vacancies/job ads",
    title = "Job ads/vacancies for CEDEFOP-OJV and JVS",
    subtitle = "quarterly sum of daily grab numbers",
    caption = "OJV ads excluding internships.\nJVS numbers based on preliminary data."
  )
ggsave("Results/cedefopJVS_summarized.png", width = 15, height = 10, units = "cm")

#####################################################################################################################
# Daily frequency counts averaged over each quarter (for comparison purposes only, these are not pseudo-stocks)===
###################################################################################################################


grouptab_q <- aggregate(subtab_q$n, list(subtab_q$qtr), mean)
colnames(grouptab_q) <- c("date", "cedefop")

table_q <- cbind(grouptab_q, jvs[1:nrow(grouptab_q),])
table_q$difference <- table_q$cedefop - table_q$jvs

table_q_long <- reshape2::melt(subset(table_q, select = c(-year, -quarter, -difference)), id.vars = "date")

ggplot(table_q_long, aes(date, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + labs(
    x = "quarter", y = "vacancies/job ads",
    title = "Job ads/vacancies for CEDEFOP-OJV and JVS",
    subtitle = "quarterly avg. over daily grab numbers",
    caption = "OJV ads excluding internships.\nJVS numbers based on preliminary data."
  )
ggsave("Results/cedefopJVS_mean.png", width = 15, height = 10, units = "cm")


#########################################################################
######### Quarterly pseudo-stocks ================
######### Mean of ads which are "active" at this point in time
###########################################################################
# ads which are "active" at a given day were calculated  beforehand and saved as "daylists" seperately for each year

rm(dframe)

# load 2018 daylist with a 30-day validity window for job ads 

daylist <- readRDS(file = "daylist_step3_30d_2018.rds")

# define the sequence of quarters as days 

min(daylist[[1]]$grab_date)
max(daylist[[length(daylist)]]$grab_date)

dates <- data.frame(dates = seq(as.Date(names(daylist)[1])  ,  as.Date(names(daylist[length(daylist)]))  ,"days")) 

seq1 <- dates %>% group_by(quarter = quarter(dates))  %>% filter(dates==min(dates)) 
seq1 <- seq1$dates
seq1[1] <- seq1[1] + months(1)

seq2 <- dates %>% group_by(quarter = quarter(dates))  %>% filter(dates==max(dates)) 
seq2 <- seq2$dates


# - Loop over each day of a quarter and count the number of job ads which are active on this day
# - Take the quarterly average over these daily job ad counts for each quarter

qmeans <- numeric()

for (k  in (1:length(seq1))) {
  period <- seq(seq1[k], seq2[k], 1)
  
  nvec <- numeric()
  j <- 1
  for (i in as.character(period)) {
    tmp <- daylist[[paste(i)]]
    nvec[j] <- count(tmp)$n
    
    j <- j + 1
  }
  
  qmeans[k] <- mean(nvec)
  
}


qmeans18 <- qmeans
seq18 <- seq1


# load 2019 daylist with a 30-day validity window for job ads

daylist <- readRDS(file = "daylist_step3_30d_2019.rds")

# define the sequence of quarters as days 

min(daylist[[1]]$grab_date)
max(daylist[[length(daylist)]]$grab_date)

dates <- data.frame(dates = seq(as.Date(names(daylist)[1])  ,  as.Date(names(daylist[length(daylist)]))  ,"days")) 

seq1 <- dates %>% group_by(quarter = quarter(dates))  %>% filter(dates==min(dates)) 
seq1 <- seq1$dates

seq2 <- dates %>% group_by(quarter = quarter(dates))  %>% filter(dates==max(dates)) 
seq2 <- seq2$dates

# - Loop over each day of a quarter and count the number of job ads which are active on this day
# - Take the quarterly average over these daily job ad counts for each quarter

qmeans <- numeric()

for (k  in (1:length(seq1))) {
  period <- seq(seq1[k], seq2[k], 1)
  
  nvec <- numeric()
  j <- 1
  for (i in as.character(period)) {
    tmp <- daylist[[paste(i)]]
    nvec[j] <- count(tmp)$n
    
    j <- j + 1
  }
  
  qmeans[k] <- mean(nvec)
  
}

qmeans19 <- qmeans
seq19 <- seq1


rm(daylist)


# put together the pseudo-stocks for 2018 and 2019

qmeans <- c(qmeans18, qmeans19)

seq1 <- c(seq18, seq19)

# wrangle dates

date <- paste0(year(seq1)," q",quarter(seq1))

#load comparison data from the JVS
#a sub-category of the JVS is the number of urgent vacancies which are to be filled immediately

jvs <- read_excel("JVSdata.xls")
jvs <- subset(jvs, select = c("year", "quarter", "vacancies", "immediately"))
jvs <- arrange(jvs, year, quarter)
colnames(jvs) <- c("year", "quarter", "jvs", "jvs_urgent")
jvs$jvs <- jvs$jvs * 1000
jvs$jvs_urgent <- jvs$jvs_urgent * 1000

#use only the jvs quarters for which we have oja data 
jvs <- jvs[1:length(qmeans),]

qmat <- data.frame(date = date, cedefop_oja = qmeans)
qmat <- cbind(qmat, jvs)

saveRDS(qmat, "qmat_jvs.Rdata")

colnames(qmat) <- c( "date", "CEDEFOP_OJA", "year", "quarter", "JVS", "JVS_urgent")

qmat_long <- reshape2::melt(subset(qmat, select = c(-year, -quarter)), id.vars = "date")

qmat_long$value <- round(qmat_long$value)

# plot the quarterly pseudo-stocks against quarterly JVS counts ==========

ggplot(qmat_long, aes(date, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs( x = "quarter", y = "vacancies/job ads",
        title = "Job ads/vacancies for CEDEFOP-OJA and JVS",
        subtitle = "pseudo-stocks avg. over last 2 months each quarter",
        caption = "OJA posted before the reference day and not yet expired, excluding internships.\nJVS numbers based on preliminary data.\njvs_urgent denotes vacancies which are to be filled immediately"   ) +
  geom_text(aes(date, value, label = value),   size = 2.5, position = position_dodge(width = 1), vjust=1.5)

ggsave("Results/pseudostock_quarterly.png", width = 15, height = 10, units = "cm")


# simplified version of the plot with only one JVS score 

qmat_long <- reshape2::melt(subset(qmat, select = c(-year, -quarter, -JVS_urgent)), id.vars = "date")

qmat_long$value <- round(qmat_long$value)

ggplot(qmat_long, aes(date, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs( x = "quarter", y = "job ads/vacancies",
        title = "Job ads/vacancies for CEDEFOP-OJA and JVS",
        subtitle = "pseudo-stocks",
        caption = "OJA posted before the reference day and not yet expired, excluding internships"   ) +
  theme(axis.text.x = element_text(angle=45, vjust=0.6), axis.text.y = element_text(angle=45, vjust=0.6), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(size = 0.3, linetype = 'solid',colour = "lightgrey"),panel.border = element_rect(colour = "grey", fill=NA, size=0.3))+
  geom_text(aes(date, value, label = value),   size = 2.5, position = position_dodge(width = 1), vjust=1.5)

ggsave("Results/pseudostock_quarterly_simple.png", width = 15, height = 10, units = "cm")



########### In German ==========

qmat <- readRDS("qmat_jvs.Rdata")

qmat_de <- qmat[,1:5]
colnames(qmat_de) <- c("Datum", "CEDEFOP_OJA", "Jahr", "Quartal", "IAB_Stellenerhebung" )

qmat_long_de <- reshape2::melt(subset(qmat_de, select = c(-Jahr, -Quartal)), id.vars = "Datum")

qmat_long_de$value <- round(qmat_long_de$value)

ggplot(qmat_long_de, aes(Datum, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs( x = "Quartal", y = "Vakanzen/Stellenanzeigen",
        title = "Vakanzen u. Stellenanzeigen \nin OJA-Daten und IAB Stellenerhebung",
        caption = "Quelle: IAB Stellenerhebung, CEDEFOP, eigene Auswertungen"   ) +
  geom_text(aes(Datum, value, label = value),   size = 2.5, position = position_dodge(width = 1), vjust=1.5)

ggsave("Results/pseudostock_quarterly_de.png", width = 15, height = 10, units = "cm")





####################################################################################################################
############### Compare the CEDEFOP pseudo-stocks with the number of open vacancies published by the Federal Employment Agency (FEA) ===============
###############################################################################################################################

jvs <- read_excel("JVSdata.xls")
jvs <- subset(jvs, select = c("year", "quarter", "BA", "vacancies", "immediately"))
jvs <- arrange(jvs, year, quarter)
colnames(jvs) <- c("year", "quarter","FEA_open_vac", "jvs", "jvs_urgent")
jvs$FEA_open_vac<- jvs$FEA_open_vac * 1000
jvs$jvs_urgent <- jvs$jvs_urgent * 1000
jvs$jvs <- jvs$jvs * 1000

#use only the jvs quarters for which we have oja data 
jvs <- jvs[1:length(qmeans),]

qmat <- data.frame(date = date, cedefop_oja = qmeans)
qmat <- cbind(qmat, jvs)

saveRDS(qmat, "qmat_FEA.Rdata")

qmat_long <- reshape2::melt(subset(qmat, select = c(-year, -quarter)), id.vars = "date")

qmat_long$value <- round(qmat_long$value)


ggplot(qmat_long, aes(date, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs( x = "quarter", y = "vacancies/job ads",
        title = "Job ads/vacancies for CEDEFOP-OJA and FEA open vacancies",
        subtitle = "pseudo-stocks avg. over last 2 months each quarter",
        caption = "OJA posted before the reference day and not yet expired, excluding internships.\nFEA-data includes internships, which we purposely exclude in the Cedefop data."   ) +
  geom_text(aes(date, value, label = value),   size = 2.5, position = position_dodge(width = 1), vjust=1.5)

ggsave("Results/pseudostock_quarterly_FEA.png", width = 15, height = 10, units = "cm")


######## FEA Up to Q1 2019 ==========================

qmat <- readRDS("qmat_FEA.Rdata")

qmat_de <- qmat[1:3,1:6]

colnames(qmat_de) <- c("Datum", "OJV_Daten", "Jahr", "Quartal", "BA_offene_Stellen", "IAB_Stellenerhebung" )

qmat_long_de <- reshape2::melt(subset(qmat_de, select = c(-Jahr, -Quartal)), id.vars = "Datum")

qmat_long_de$value <- round(qmat_long_de$value)

ggplot(qmat_long_de, aes(Datum, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs( x = "Quartal", y = "Vakanzen/Stellenanzeigen",
        title = "Vakanzen u. Stellenanzeigen in OJV-Daten und IAB Stellenerhebung",
        caption = "Quelle: IAB Stellenerhebung, CEDEFOP, eigene Auswertungen"   ) +
  geom_text(aes(Datum, value, label = value),   size = 2.5, position = position_dodge(width = 1), vjust=1.5)

ggsave("Results/pseudostock_quarterly_FEA_uptoq1_de.png", width = 15, height = 10, units = "cm")



########### Upto1 graph in German ==========

qmat_de <- qmat[1:3,1:5]
colnames(qmat_de) <- c("Datum", "OJV_Daten", "Jahr", "Quartal", "IAB_Stellenerhebung" )

qmat_long_de <- reshape2::melt(subset(qmat_de, select = c(-Jahr, -Quartal)), id.vars = "Datum")

qmat_long_de$value <- round(qmat_long_de$value)

ggplot(qmat_long_de, aes(Datum, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs( x = "Quartal", y = "Vakanzen/Stellenanzeigen",
        title = "Vakanzen u. Stellenanzeigen \nin OJV-Daten und IAB Stellenerhebung",
        caption = "Quelle: IAB Stellenerhebung, CEDEFOP, eigene Auswertungen"   ) +
  geom_text(aes(Datum, value, label = value),   size = 2.5, position = position_dodge(width = 1), vjust=1.5)

ggsave("Results/pseudostock_quarterly_uptoq1_de.png", width = 15, height = 10, units = "cm")



#########################################################################
######### Monthly pseudo-stocks* ================
######### Mean of ads which are "active" at this point in time for 30 days
###########################################################################
#*Same procedure as for quarterly pseudo-stocks described above, just with the reference period set to one month


# define the sequence of months as days ---------

seq1 <- as.Date(c("2018-08-1","2018-09-1","2018-10-1","2018-11-1","2018-12-1","2019-01-1","2019-02-1","2019-03-1" ,"2019-04-1" ,"2019-05-1" ,"2019-06-1"))

seq2 <- as.Date(c("2018-08-31","2018-09-30","2018-10-31","2018-11-30","2018-12-31","2019-01-31","2019-02-28","2019-03-31" ,"2019-04-30" ,"2019-05-30", "2019-06-30"))

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


date <- seq1


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

wmat <- cbind(wmat,date, cedefop_oja = wmeans)

wmat_long <- reshape2::melt(subset(wmat, select = c(-year, -quarter)), id.vars = "date")

wmat_long$value <- round(wmat_long$value)

saveRDS(wmat_long, "wmat_long_jvs.Rdata")

# bar graph 

ggplot(wmat_long, aes(date, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs( x = "date", y = "vacancies/job ads",
        title = "Job ads/vacancies for CEDEFOP-OJA and JVS",
        subtitle = "OJA pseudo-stocks monthly avg., JVS quarterly avg.",
        caption = "OJA posted max 30 days before the reference day and not yet expired.\njvs_urgent denotes vacancies which are to be filled immediately"   ) +
  theme(axis.text.x=element_text(angle=90,hjust=1,size=7))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")

ggsave("Results/pseudostock_monthly_30.png", width = 15, height = 10, units = "cm")


# line graph


ggplot(wmat_long)+
  geom_line(aes(x=date, y = value, color = variable))+
       labs( x = "month", y = "vacancies/job ads",
        title = "Job ads/vacancies for CEDEFOP-OJA and JVS",
        subtitle = "pseudo-stocks avg. over last 2 months each quarter",
        caption = "OJA posted max 30 days before the reference day and not yet expired.\njvs_urgent denotes vacancies which are to be filled immediately"   ) +
  theme(axis.text.x=element_text(angle=90,hjust=1,size=7))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")

ggsave("Results/pseudostock_monthly_line_30.png", width = 15, height = 10, units = "cm")



#########################################################################
######### Monthly pseudo-stocks ================
######### Mean of ads which are "active" at this point in time for 40 days
###########################################################################

daylist <- readRDS(file = "daylist_step3_40d.Rdata")


# define the sequence of months as days ---------

seq1 <- as.Date(c("2018-08-1","2018-09-1","2018-10-1","2018-11-1","2018-12-1","2019-01-1","2019-02-1","2019-03-1" ,"2019-04-1" ,"2019-05-1" ,"2019-06-1"))

seq2 <- as.Date(c("2018-08-31","2018-09-30","2018-10-31","2018-11-30","2018-12-31","2019-01-31","2019-02-28","2019-03-31" ,"2019-04-30" ,"2019-05-30", "2019-06-30"))

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
for (i in (9:11))   wmat <- rbind(wmat,jvs[4,])


wmat <- cbind(wmat,date, cedefop_oja = wmeans)

wmat_long <- melt(subset(wmat, select = c(-year, -quarter)), id.vars = "date")

wmat_long$value <- round(wmat_long$value)

saveRDS(wmat_long, "wmat_long_jvs_40.Rdata")

ggplot(wmat_long, aes(date, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs( x = "month", y = "vacancies/job ads",
        title = "Job ads/vacancies for CEDEFOP-OJA and JVS",
        subtitle = "pseudo-stocks avg. over last 2 months each quarter",
        caption = "OJA posted max 40 days before the reference day and not yet expired.\njvs_urgent denotes vacancies which are to be filled immediately"   ) +
  theme(axis.text.x=element_text(angle=90,hjust=1,size=7))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")


ggsave("Results/pseudostock_monthly_40.png", width = 15, height = 10, units = "cm")


#########################################################################
######### Monthly pseudo-stocks ================
######### Mean of ads which are "active" at this point in time for 20 days
###########################################################################

daylist <- readRDS(file = "daylist_step3_20d.Rdata")


# define the sequence of months as days ---------

seq1 <- as.Date(c("2018-08-1","2018-09-1","2018-10-1","2018-11-1","2018-12-1","2019-01-1","2019-02-1","2019-03-1"))

seq2 <- as.Date(c("2018-08-31","2018-09-30","2018-10-31","2018-11-30","2018-12-31","2019-01-31","2019-02-28","2019-03-31"))

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

wmat <- cbind(wmat,date, cedefop_oja = wmeans)

wmat_long <- melt(subset(wmat, select = c(-year, -quarter)), id.vars = "date")

wmat_long$value <- round(wmat_long$value)


ggplot(wmat_long, aes(date, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs( x = "month", y = "vacancies/job ads",
        title = "Job ads/vacancies for CEDEFOP-OJA and JVS",
        subtitle = "pseudo-stocks avg. over last 2 months each quarter",
        caption = "OJA posted max 20 days before the reference day and not yet expired.\njvs_urgent denotes vacancies which are to be filled immediately"   ) +
  theme(axis.text.x=element_text(angle=90,hjust=1,size=7))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")


ggsave("Results/pseudostock_monthly_20.png", width = 15, height = 10, units = "cm")




