####################################################################################################
#################Program for cleaning and preparing CEDEFOP OJA data
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

# Setup ------------------------------------------------

path <- "alldata_june20/"

options(scipen = 999)

library(Hmisc)
library(dplyr)
#library(networkD3)
# library(lintr)
# library(styler)
library(gmodels)
library(data.table)
#library(sp)
#library(sf)
library(tidyverse)
library(rapport)
library(openxlsx)
library(fst)
library(readxl)

load("OJVsample_q218_q120.rdata")

dframe <- data
rm(data)
setDT(dframe)

# mark deduplication
dframe$dup <- ifelse(duplicated(dframe$general_id), 1, 0)

# convert dates

dframe$grab_date <- as.Date(dframe$grab_date, origin = "1970-01-01")

dframe$expire_date <- as.Date(dframe$expire_date, origin = "1970-01-01")

# add month column

dframe$month <- format(as.Date(dframe$grab_date), "%Y-%m")

# add quarter column 

dframe <- dframe %>% mutate(qtr = paste0(year(grab_date), "-", "q", quarter(grab_date)))


# add difference between grab and expire date

dframe$diff <- dframe$expire_date - dframe$grab_date

###################################################
#####recode missings to NA============================
#############################################

empty_as_na <- function(x){

  x[!str_detect(x, "")] <- NA
  
  return(x)
}


setDT(dframe)

dframe <- dframe %>% mutate_at(c("companyname", "city", "idcity", "province", "idprovince", "macro_region", "idmacro_region", "idcontract", "contract", "ideducational_level", "educational_level", "idsector", "sector", "idmacro_sector", "macro_sector", "idcategory_sector", "category_sector", "idsalary", "salary", "idworking_hours", "working_hours" , "idexperience", "experience"), empty_as_na)

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


colnames(dframe)

###################################################################
# save the various subsets
####################################

#str(dframe)

save(dframe, file=paste0(path,"OJVsample_step1.rdata"), compress = TRUE, compression_level = 6)

write.fst(dframe, paste0(path,"OJVsample_step1.fst"), 100)

# deduplicated subset without superfluous information

dframe <- subset(dframe, dup == 0)

dframe <- dframe[, c(1:49, 51:53)]

savedata <- dframe[, c(1:3, 6, 7, 10:49, 51:52)]

save(savedata, file=paste0(path,"OJVsample_step1_most.rdata"), compress = TRUE, compression_level = 6)

# subset without occupation information

savedata <- dframe[, c(1:10, 19:52)]

save(savedata, file=paste0(path,"OJVsample_step1_noocc.rdata"), compress = TRUE, compression_level = 6)


#small subset without imputed classifications (no occupation, no education, no sector)

savedata <- dframe[, c(1:10, 19:30, 39:49, 51:52)]

save(savedata, file=paste0(path,"OJVsample_step1_noimput.rdata"), compress = TRUE, compression_level = 6)

# subset without geo information

savedata <- dframe[, c(1:18, 29:52)]

save(savedata, file=paste0(path,"OJVsample_step1_nogeo.rdata"), compress = TRUE, compression_level = 6)

