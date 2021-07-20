#######################################################################
################ Program for the cleanup, deduplication and initial filtering of CEDEFOP data 
###############################################################

# Setup ------------------------------------------------

options(scipen = 999)

rm(list = ls())

path <- "Data/alldata_12_2020/"
dir.create( "Results/alldata_12_2020/" , showWarnings = FALSE)
resultspath <- "Results/alldata_12_2020/"

#most recent dataset revision:
rmonth <- 12
ryear <- 2020

gc()

library(Hmisc)
library(ggplot2)
library(readxl)
library(gmodels)
library(data.table)
library(lubridate)
library(stringr)
library(fst)
library(dplyr)
library(openxlsx)
library(foreach)
library(parallel)

# Set multicore
no_cores <- 32

# Initiate cluster
cl <- makeCluster(no_cores)

years <- c(2018, 2019, 2020)
years <- years[years <= ryear]

naframe <- data.frame()
na_after_frame <- data.frame()

for (year in years) { 
  
  dframe <- read.fst(paste0(path,"OJVsample_step1_", year, ".fst"), as.data.table = TRUE)
  
  #index <- sample(dframe$general_id, 1000)
  #dframe <- dframe[general_id %in% index, ]
  
  #share of missings by variable (before deduplication)
  
  na_count <- unlist(mclapply(dframe, function(x) sum(length(which(is.na(x)))), mc.cores=32))
  na_count <- as.data.frame(na_count)
  na_count$variable <- rownames(na_count)
  na_count$total <- nrow(dframe)
  
  temp <- data.frame(na_count, year)
  
  naframe <- rbind(naframe, temp)
  
  # the duplicate observations with the lowest number of missing variables
  dframe$na_count <- rowSums(is.na(dframe))
  
  dframe <- dframe %>% group_by(general_id) %>% arrange(na_count, .by_group = TRUE)
  setDT(dframe)
  
  dframe$dup <- ifelse(duplicated(dframe$general_id), 1, 0)
  
  #check manually for duplicated observations by observables
  
  vdup <- duplicated(subset(dframe, select = c("grab_date", "expire_date", "lang", "idesco_level_4", "esco_level_4", "idesco_level_3", "esco_level_3", "idesco_level_2", "esco_level_2", "idesco_level_1", "esco_level_1", "idprovince", "province", "idregion", "region", "idmacro_region", "macro_region", "idcountry", "country", "idcontract",     "contract", "idsector", "sector", "idmacro_sector", "macro_sector", "idcategory_sector", "category_sector", "sourcecountry","companyname" )))
  
  table(vdup)
  
  dframe$vdup <- vdup
  
   # mark staffing agencies ---------
  
  # load list of typical elements of staffing agencies names:
  keywords <- read_excel("Keywords_staffing_firms.xls" )
  
  lengtha <- sum(!is.na(keywords$combinea))
  lengthb <- sum(!is.na(keywords$combineb))
  lengthc <- sum(!is.na(keywords$identified))
  
  aword <- as.character(keywords$combinea[1:lengtha])
  bword <- as.character(keywords$combineb[1:lengthb])
  clear <- as.character(keywords$identified[1:lengthc])
  
  
  grid <- CJ(aword,bword)
  
  c_1 <- character()
  c_2 <- character()
  for (i in 1:dim(grid)[1]) {
    c_1[i] <- paste(grid[i,1],grid[i,2])
    c_2[i] <- paste0(grid[i,1],grid[i,2])
  }
  
  keys <- c(c_1,c_2,clear)
  
  # removal of rows with companyname in list:
  
  #first step cleaning of company names
  
  dframe$companyname <- unlist(mclapply(dframe$companyname, function(x) gsub("[[:punct:]]|\t|\n|\r|,|;|\\.","",x), mc.cores=32))
  
  dframe$companyname <- tolower(dframe$companyname)
  
  collapsekeys <- paste(keys, collapse = '|')
  
  dframe$staffing <- unlist(mclapply(dframe$companyname, function(x) str_detect(x, collapsekeys), mc.cores=32))
  
  setDT(dframe)
  
  # clean and order company names for LMC --------
  
  dframe$companyname <- tolower(dframe$companyname)
  
  sep <- function(linha) {
    resp <- strsplit(linha," |/|-")
    resp <- unlist(resp)
    resp <- gsub("[[:punct:]]|\t|\n|\r|,|;|\\.","",resp)
    resp <- sort(resp[which(nchar(resp) > 2)])
    paste0(resp,collapse=" ")
  }
  
  ordered <- unlist(mclapply(dframe$companyname, function(x) sep(x), mc.cores=32))
  
  dframe$companyname <- ordered
  
  dframe$companyname[dframe$companyname==""] <- NA
  
  #second run of companyname filtering for staffing agencies after name cleanup
  tmp<- unlist(mclapply(dframe$companyname, function(x) str_detect(x, collapsekeys), mc.cores=32))
  
  dframe[dframe$staffing == FALSE & tmp == TRUE, ]$staffing <- TRUE
  
  setDT(dframe)
  
   #share of missings by variable (AFTER deduplication and sizereduction)
  
  na_count <- unlist(mclapply(dframe, function(x) sum(length(which(is.na(x)))), mc.cores=32))
  na_count <- as.data.frame(na_count)
  na_count$variable <- rownames(na_count)
  na_count$total <- nrow(dframe)
  
  #na after sizereduction
  temp <- data.frame(na_count, year)
  
  na_after_frame <- rbind(naframe, temp)
  
  
  # save data
  
  save(dframe, file=paste0(path, "OJVsample_step1_marked_", year, ".rdata" ), compress = TRUE)
  
  write_fst(dframe, path = paste0(path,"OJVsample_step1_marked_", year, ".fst"), compress = 100)
  
  # save data without interns, duplicates and staffing agencies
  
  dframe <- dframe[dup == 0]
  
  dframe <- dframe[staffing == FALSE | is.na(companyname)]
  
  dframe <- subset(dframe, contract!="Internship" | is.na(contract))
  
  save(dframe, file=paste0(path, "OJVsample_step1_redux_", year, ".rdata" ), compress = TRUE)
  
  write_fst(dframe, path = paste0(path,"OJVsample_step1_redux_", year, ".fst"), compress = 100)
  
}

  
  
#merge and save na counts of all years combined

  naframe$share <- round(naframe$na_count/naframe$total,2)
  
  write.xlsx(naframe, file = paste0(resultspath, "na_byvar_before_dedup.xlsx"), col.names = TRUE, append = FALSE )
  
  na_after_frame$share <- round(na_after_frame$na_count/na_after_frame$total,2)
  write.xlsx(na_after_frame, file = paste0(resultspath, "na_byvar_after_dedup.xlsx"), col.names = TRUE, append = FALSE )
  
  
  rm(list = ls())