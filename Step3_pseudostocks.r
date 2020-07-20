####################################################################################################
#################Program for creating outputs and plots from CEDEFOP Pseudostock data
#####################################################################################################

#adapt paths according to your own file structure


path <- "alldata_june20/"
resultspath <- "Results/alldata_june20/"

options(scipen = 999)

rm(list = ls())


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



dframe <- read_fst(paste0(path,"OJVsample_step1_redux.fst"), c("general_id", "grab_date", "expire_date" ), as.data.table = TRUE)


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
ggsave(paste0(resultspath, "cedefopJVS_summarized.png"), width = 15, height = 10, units = "cm")


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
ggsave(paste0(resultspath, "cedefopJVS_mean.png"), width = 15, height = 10, units = "cm")


#########################################################################
######### Quarterly pseudo-stocks ================
######### Mean of ads which are "active" at this point in time
###########################################################################
# ads which are "active" at a given day were calculated  beforehand and saved as "daylists" seperately for each year

years <- c(2018, 2019, 2020)

for (l in years) {
  
  
	# load daylist with a 30-day validity window for job ads 
  
  daylist <- readRDS(file = paste0(path, "daylist_step3_30d_", l , ".rds"))
  
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
  
  #put the yearly results together
  
  if (l == years[1]) {
    allqmeans <- qmeans
    seq <- seq1
    
  } else {
    
    allqmeans <- c(allqmeans, qmeans)
    seq <- c(seq, seq1)
  }
  
}

rm(daylist, tmp)

# wrangle dates

date <- paste0(year(seq)," q",quarter(seq))

#load comparison data from the JVS
#a sub-category of the JVS is the number of urgent vacancies which are to be filled immediately

jvs <- read_excel("JVSdata.xls")
jvs <- subset(jvs, select = c("year", "quarter", "vacancies", "immediately"))
jvs <- arrange(jvs, year, quarter)
colnames(jvs) <- c("year", "quarter", "jvs", "jvs_urgent")
jvs$jvs <- jvs$jvs * 1000
jvs$jvs_urgent <- jvs$jvs_urgent * 1000

#use only the jvs quarters for which we have oja data 
jvs <- jvs[1:length(allqmeans),]

qmat <- data.frame(date = date, cedefop_oja = allqmeans)
qmat <- cbind(qmat, jvs)

saveRDS(qmat, paste0(resultspath, "qmat_jvs.Rdata"))

colnames(qmat) <- c( "date", "Cedefop_OJA", "year", "quarter", "JVS", "JVS_urgent")

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

ggsave(paste0(resultspath, "pseudostock_quarterly.png"), width = 15, height = 10, units = "cm")


# simplified version with only one JVS score 

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

ggsave(paste0(resultspath, "pseudostock_quarterly_simple.png"), width = 15, height = 10, units = "cm")



########### In German ==========

qmat <- readRDS(paste0(resultspath, "qmat_jvs.Rdata"))

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

ggsave(paste0(resultspath, "pseudostock_quarterly_de.png"), width = 15, height = 10, units = "cm")


####################################################################################################################
############### Compare the CEDEFOP pseudo-stocks with the number of open vacancies published by the Federal Employment Agency (FEA) ===============
###############################################################################################################################

qmat <- readRDS(paste0(resultspath, "qmat_jvs.Rdata"))

jvs <- read_excel("JVSdata.xls")
jvs <- arrange(jvs, year, quarter)
jvs <- subset(jvs, select = c("BA"))
colnames(jvs) <- c("FEA_open_vac")
jvs$FEA_open_vac<- jvs$FEA_open_vac * 1000


#use only the jvs quarters for which we have oja data 
jvs <- jvs[1:dim(qmat)[1],]

qmat <- cbind(qmat, jvs)

qmat_long <- reshape2::melt(subset(qmat, select = c(-year, -quarter)), id.vars = "date")

qmat_long$value <- round(qmat_long$value)


ggplot(qmat_long, aes(date, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs( x = "quarter", y = "vacancies/job ads",
        title = "Job ads/vacancies for CEDEFOP-OJA and FEA open vacancies",
        subtitle = "pseudo-stocks avg. over last 2 months each quarter",
        caption = "OJA posted before the reference day and not yet expired, excluding internships.\nFEA-data includes internships, which we purposely exclude in the Cedefop data."   ) +
  geom_text(aes(date, value, label = value),   size = 2.5, position = position_dodge(width = 1), vjust=1.5)

ggsave(paste0(resultspath, "pseudostock_quarterly_FEA.png"), width = 15, height = 10, units = "cm")



