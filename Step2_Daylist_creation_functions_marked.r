#######################################################################
################Program for generating the list of job ads which are active each day
################ This is the first step for calculating pseudostocks of job ads
###############################################################

options(scipen = 999)

Sys.setenv(LANG = "en")

rm(list = ls())

path <- "Data/alldata_12_2020/"
dir.create( "Results/alldata_12_2020/" , showWarnings = FALSE)
dir.create( "Results/alldata_12_2020/working_data/" , showWarnings = FALSE)
resultspath <- "Results/alldata_12_2020/"

library(RColorBrewer)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape2)
library(Hmisc)
library(data.table)
library(parallel)
library(fst)

years <- c(2018, 2019, 2020)

dframe <- data.table()

for (year in years) { 
  tmp <- read.fst(paste0(path,"OJVsample_step1_marked_", year, ".fst"), as.data.table = TRUE, columns = c("general_id", "grab_date", "expire_date", "dup" , "staffing", "contract", "category"))
  dframe <- rbind(dframe, tmp)
  
}

rm(tmp)
setDT(dframe)


####### [removed] Daylists are calculated without duplicates, internships or staffing agencies

#dframe <- dframe[staffing==FALSE & (contract!="Internship" | is.na(contract))]

# index <- sample(1:nrow(dframe), 10000)

# dframe <- dframe[index, ]


#############################################################################
########## Daylist creation function ---------------------
#############################################################################

#with all observations

dayx <- function(x, data, days, dsave, dreturn) {
  
  dayxdf <- data[grab_date <= x & grab_date > (x - days) & expire_date > x]
  
  if (dsave == TRUE) {
    saveRDS(dayxdf, file = paste0(path,"working_data/dayxdf_", days, "_", x, ".rds"), compress = TRUE)
  }
  
  if (dreturn == TRUE) { return(dayxdf)
  }
}


#############################################################################
########## Create the daylist with 30 days validity ---------------------
#############################################################################

dates <- unique(dframe$grab_date)
years <- unique(year(dframe$grab_date))

blocklist.start <- numeric()
blocklist.end <- numeric()


j <- 1
for (i in years) {
  
  if (i == min(years)) {
    blocklist.start[j] <- as.Date(min(dates))
  } else { 
    
  blocklist.start[j] <- as.Date(paste0(years[j],"-01-01"))
  }
  
  if (i == max(years) ) {
    blocklist.end[j] <- as.Date(max(dates))
  } else {
    
    blocklist.end[j] <- as.Date(paste0(years[j],"-12-31"))
  }
  
  j <- j+1

}

as.Date(blocklist.start, origin = "1970-01-01")
as.Date(blocklist.end, origin = "1970-01-01")

# loop over years and create daylists

j <- 1

for (i in years) {
  
dvec <- seq( as.Date(blocklist.start[j], origin = "1970-01-01"), as.Date(blocklist.end[j], origin = "1970-01-01") ,1 ) 

daylist <- mclapply(dvec, dayx, days = 30, dsave = FALSE, dreturn = TRUE, data=dframe, mc.cores = 34)

names(daylist) <- dvec

saveRDS(daylist, file = paste0(path,"daylist_step3_30d_marked_",i, ".rds"),  compress = TRUE)

rm(daylist)

j <- j+1
}



#############################################################################
########## Create the daylist with 20 days validity ---------------------
#############################################################################


dates <- unique(dframe$grab_date)
years <- unique(year(dframe$grab_date))

blocklist.start <- numeric()
blocklist.end <- numeric()

j <- 1
for (i in years) {
  
  if (i == min(years)) {
    blocklist.start[j] <- as.Date(min(dates)+months(1))
  } else { 
    
    blocklist.start[j] <- as.Date(paste0(years[j],"-01-01"))
  }
  
  if (i == max(years) ) {
    blocklist.end[j] <- as.Date(max(dates))
  } else {
    
    blocklist.end[j] <- as.Date(paste0(years[j],"-12-31"))
  }
  
  j <- j+1
  
}


# loop over years and create daylists

j <- 1

for (i in years) {
  
  dvec <- seq( as.Date(blocklist.start[j], origin = "1970-01-01"), as.Date(blocklist.end[j], origin = "1970-01-01") ,1 ) 
  
  daylist <- mclapply(dvec, dayx, days = 20, dsave=FALSE, dreturn = TRUE, data=dframe, mc.cores = 34)
  
  names(daylist) <- dvec
  
  saveRDS(daylist, file = paste0(path,"daylist_step3_20d_marked_",i, ".rds"),  compress = TRUE)
  
  rm(daylist)
  
  j <- j+1
}


#############################################################################
########## Create the daylist with 40 days validity ---------------------
#############################################################################


dates <- unique(dframe$grab_date)
years <- unique(year(dframe$grab_date))

blocklist.start <- numeric()
blocklist.end <- numeric()

j <- 1
for (i in years) {
  
  if (i == min(years)) {
    blocklist.start[j] <- as.Date(min(dates)+months(1))
  } else { 
    
    blocklist.start[j] <- as.Date(paste0(years[j],"-01-01"))
  }
  
  if (i == max(years) ) {
    blocklist.end[j] <- as.Date(max(dates))
  } else {
    
    blocklist.end[j] <- as.Date(paste0(years[j],"-12-31"))
  }
  
  j <- j+1
  
}



# loop over years and create daylists

j <- 1

for (i in years) {
  
  dvec <- seq( as.Date(blocklist.start[j], origin = "1970-01-01"), as.Date(blocklist.end[j], origin = "1970-01-01") ,1 ) 
  
  daylist <- mclapply(dvec, dayx, days = 40, dsave=FALSE, dreturn = TRUE, data=dframe, mc.cores = 34)
  
  names(daylist) <- dvec
  
  saveRDS(daylist, file = paste0(path, "daylist_step3_40d_",i, ".rds"),  compress = TRUE)
  
  rm(daylist)
  
  j <- j+1
}
