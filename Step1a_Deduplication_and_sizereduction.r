####################################################################################################
#################Program for deduplicating, filtering and ammending CEDEFOP OJA data for data quality improvements 
#####################################################################################################


# Setup ------------------------------------------------
path <- "alldata_june20/"

options(scipen = 999)

rm(list = ls())

gc()

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("xlsx")
# install.packages("networkD3")
# install.packages("lubridate")
# install.packages("lintr")
# install.packages("styler")

library(Hmisc)
library(dplyr)
library(ggplot2)
#library(xlsx)
library(readxl)
#ibrary(networkD3)
# library(lintr)
# library(styler)
library(gmodels)
library(data.table)
library(lubridate)
library(stringr)
library(fst)


#load("OJVsample_step1.rdata")
dframe <- read.fst("OJVsample_step1.fst", as.data.table = TRUE)

#index <- sample(1:nrow(dframe), 10000)
#dframe <- dframe[index, ]

setDT(dframe)

#count and save the share of missings by variable (before deduplication)
#this is done for data quality monitoring

na_count <- sapply(dframe, function(x) sum(length(which(is.na(x)))))
na_count <- as.data.frame(na_count)
na_count$variable <- rownames(na_count)
na_count$share <- round(na_count$na_count/nrow(dframe),2)

write.xlsx(na_count, file = "Results/alldata_june20/na_byvar_before_dedup.xlsx", col.names = TRUE, append = FALSE )

#remove observations already marked as duplicate by CEDEFOP
#"duplicate" observations differ in their content
#therefore we keep, from each duplicate group, the observation with the lowest number of missing variables

dframe$na_count <- rowSums(is.na(dframe))

dframe <- dframe %>% group_by(general_id) %>% arrange(na_count, .by_group = TRUE)
setDT(dframe)

dframe$dup <- ifelse(duplicated(dframe$general_id), 1, 0)

dframe <- dframe[dup == 0]

# filter staffing agencies ---------

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

ismiss <- dframe[is.na(companyname)]

dframe <- filter(dframe, !str_detect(companyname, paste(keys, collapse = '|')))

dframe <- bind_rows(dframe, ismiss)

setDT(dframe)

rm(ismiss)


# clean and order company names for LMC index --------

sep <- function(linha) {
  resp <- strsplit(linha," |/|-")
  resp <- unlist(resp)
  resp <- gsub(",|;|\\.","",resp)
  resp <- sort(resp[which(nchar(resp) > 2)])
  paste0(resp,collapse=" ")
}

ordered <- sapply(dframe$companyname, function(x) sep(x))

dframe$companyname <- ordered

# drop internships -----------

dframe <- subset(dframe, contract!="Internship" | is.na(contract))

# by saving the data with the fst package, saving and loading is faster.
# as an additional advantage, the fst package can selectively load only parts of the dataframe, for instance if only certain columns are needed for analysis
write_fst(dframe, path = paste0(path,"OJVsample_step1_redux.fst"), compress = 100)

