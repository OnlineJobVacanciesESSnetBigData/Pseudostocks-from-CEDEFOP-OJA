####################################################################################################
#################Program for calculating pseudo-stocks from CEDEFOP data
#####################################################################################################

# Copyright 2020 DESTATIS
#  
# Licensed under the EUPL, Version 1.2 or – as soon as they will be approved by the European Commission - subsequent versions of the EUPL (the "Licence");
# You may not use this work except in compliance with the Licence.
# You may obtain a copy of the Licence at:
#  *https://joinup.ec.europa.eu/software/page/eupl5
#  
# Unless required by applicable law or agreed to inwriting, software distributed under the Licence is distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the Licence for the specific language governing permissions and limitations under the Licence.

options(scipen = 999)

Sys.setenv(LANG = "en")

rm(list = ls())

path <- "alldata_june20/"


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


#############################################################################
########## Daylist creation function* ---------------------
#############################################################################
#*The daylist is a list structure created from the cedefop dataset. 
# It is a list of all observed days, with each list item containing all job ads which are "active" at this point in time
# A job ad is active for a given day, if it has been posted within a specified number of days earlier and has not yet expired


#define a function for creating the daylist. 
# Options:
# days: the number of days for which an ad is to be counted as active (we use between 20 and 40),
# dsave: If the subset of job ads for each day should be saved to disk
# dreturn: if the full daylist should be returned into the workspace by the function 

dayx <- function(x, data, days, dsave, dreturn) {
  
  dayxdf <- data[grab_date <= x & grab_date > (x - days) & expire_date > x]
  
  
  if (dsave == TRUE) {
    saveRDS(dayxdf, file = paste0(path,"working_data/dayxdf_", days, "_", x, ".rds"), compress = TRUE)
  }
  
  if (dreturn == TRUE) { return(dayxdf)
  }
}


#################################################################
#####Select Subsets of data for use with the function ---------------
#########################################################################

# using the fst package, load only the required columns
# adjust depending on what you intend to analyse and how much memory is available
# here is the bare minimum of variables necessary for calculating pseudostocks

dframe <- read_fst(paste0(path,"OJVsample_step1_redux.fst"), c("general_id", "grab_date", "expire_date" ), as.data.table = TRUE)


#############################################################################
########## Create the daylist with 30 days validity ---------------------
#############################################################################

#create seperate data blocks by year and save seperate daylists for each year
#this is purely done due to memory shortage

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

daylist <- mclapply(dvec, dayx, days = 30, dsave=TRUE, dreturn = TRUE, data=dframe, mc.cores = 8)

names(daylist) <- dvec

saveRDS(daylist, file = paste0(path,"daylist_step3_30d_",i, ".rds"),  compress = TRUE)

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
  
  daylist <- mclapply(dvec, dayx, days = 20, dsave=FALSE, dreturn = TRUE, data=dframe, mc.cores = 8)
  
  names(daylist) <- dvec
  
  saveRDS(daylist, file = paste0("daylist_step3_20d_",i, ".rds"),  compress = TRUE)
  
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
  
  daylist <- mclapply(dvec, dayx, days = 40, dsave=FALSE, dreturn = TRUE, data=dframe, mc.cores = 8)
  
  names(daylist) <- dvec
  
  saveRDS(daylist, file = paste0("daylist_step3_40d_",i, ".rds"),  compress = TRUE)
  
  rm(daylist)
  
  j <- j+1
}


