#######################################################################
################ Program for the initial preparation of CEDEFOP data 
###############################################################

# Setup ------------------------------------------------

# set path according to dataset revision

#revision 9
path <- "Data/alldata_12_2020/"
#revision 8
#path <- "Data/alldata_9_2020/"
#revision 4
#path <- "Data/alldata_3_2020/"
#revision 1
#path <- "Data/alldata_12_2019/"

options(scipen = 999)

library(Hmisc)
library(dplyr)
library(gmodels)
library(data.table)
library(tidyverse)
library(rapport)
library(openxlsx)
library(fst)
library(readxl)

load(paste0(path, "source_quality_assessment.rdata"))

#most recent data:
rmonth <- 12
ryear <- 2020

years <- c(2018, 2019, 2020)
#years <- c(2018, 2019)

for (year in years) { 

  load(paste0(path, "OJVsample_", year, ".rdata"))
  
  #preparation
  setDT(dframe)
  
  # mark deduplication
  dframe$dup <- ifelse(duplicated(dframe$general_id), 1, 0)
  
  # dates
  
  dframe$grab_date <- as.Date(dframe$grab_date, origin = "1970-01-01")
  
  dframe$expire_date <- as.Date(dframe$expire_date, origin = "1970-01-01")
  
  # months
  
  dframe$month <- format(as.Date(dframe$grab_date), "%Y-%m")
  
  # quarter 
  
  dframe <- dframe %>% mutate(qtr = paste0(year(grab_date), "-", "q", quarter(grab_date)))
  
  # difference grab and expire date
  
  dframe$diff <- dframe$expire_date - dframe$grab_date
  
  ###################################################
  #####recode missings to NA============================
  #############################################
  
  empty_as_na <- function(x){
  
    x[!str_detect(x, "")] <- NA
    
    return(x)
  }
  
  setDT(dframe)
  
  dframe <- dframe %>% mutate_at(c("companyname", "city", "idcity", "idregion", "region", "province", "idprovince", "macro_region", "idmacro_region", "idcontract", "contract", "ideducational_level", "educational_level", "idsector", "sector", "idmacro_sector", "macro_sector", "idcategory_sector", "category_sector", "idsalary", "salary", "idworking_hours", "working_hours" , "idexperience", "experience"), empty_as_na)
  
  setDT(dframe)
  
  
  ##############################################################################
  # Geo data cleanup ------------------------------------------------
  #######################################################################
  
  
  # load NUTS table for citystates -------------------
  
  citystates <- read_excel("CitystatesNUTScodes.xls")
  citystates <- citystates[1:3,]
  
  # replace citystates NUTS 2 --------------------
  temp <- unlist(lapply(dframe$idmacro_region, function(x) match(x, citystates$NUTS1_Code)))
  
  temp1 <- as.character(citystates$NUTS2_Code[temp])
  temp2 <- as.character(citystates$NUTS2[temp])
  
  dframe$idregion[!is.na(temp1)] <- temp1[!is.na(temp1)]
  dframe$region[!is.na(temp2)] <- temp2[!is.na(temp2)]
  
  
  # replace citystates NUTS 3 --------------------
  temp <- unlist(lapply(dframe$idregion, function(x) match(x, citystates$NUTS2_Code)))
  
  temp1 <- as.character(citystates$NUTS3_Code[temp])
  temp2 <- as.character(citystates$NUTS3[temp])
  
  dframe$idprovince[!is.na(temp1)] <- temp1[!is.na(temp1)]
  dframe$province[!is.na(temp2)] <- temp2[!is.na(temp2)]
  
  #join source quality assesment
  dframe <- left_join(dframe, sq, by= "source")
  
  colnames(dframe)
  
  ###################################################################
  #####Save data in .fst format
  #################################################
  
  write.fst(dframe, paste0(path,"OJVsample_step1_", year, ".fst"), 100)
  
  rm(temp)
  rm(temp1)
  rm(temp2)
  rm(dframe)
}
