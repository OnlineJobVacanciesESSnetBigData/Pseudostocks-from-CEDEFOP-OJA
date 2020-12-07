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

rm(list = ls())

#adapt paths according to your own file structure

path <- "alldata_june20/"
resultspath <- "Results/alldata_june20/"

options(scipen = 999)


library(RColorBrewer)
library(dplyr)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape2)
library(Hmisc)
library(data.table)
library(parallel)
library(fst)


valid <- 20
qmat20 <- readRDS(paste0(resultspath, "qmat_jvs", valid, "day.Rdata"))

valid <- 30
qmat30 <- readRDS(paste0(resultspath, "qmat_jvs", valid, "day.Rdata"))

valid <- 40
qmat40 <- readRDS(paste0(resultspath, "qmat_jvs", valid, "day.Rdata"))

qmat <- subset(qmat20, select = c(-year, -quarter, -JVS_urgent, -JVS))
qmat <- left_join(qmat, subset(qmat30, select = c(-year, -quarter, -JVS_urgent, -JVS)), by = "date")
qmat <- left_join(qmat, subset(qmat40, select = c(-year, -quarter, -JVS_urgent, -JVS)), by = "date")

colnames(qmat) <- c("date", "20_days", "30_days", "40_days")

qmat_long <- reshape2::melt(qmat, id.vars = "date")

qmat_long$value <- round(qmat_long$value)

ggplot(qmat_long, aes(date, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs( x = "quarter", y = "job ads",
        title = "Pseudostocks of CEDEFOP-OJA, by validity period" ) +
  theme(axis.text.x = element_text(angle=45, vjust=0.6), axis.text.y = element_text(angle=45, vjust=0.6), panel.background = element_rect(fill = "white"), panel.grid.major = element_line(size = 0.3, linetype = 'solid',colour = "lightgrey"),panel.border = element_rect(colour = "grey", fill=NA, size=0.3))+
  geom_text(aes(date, value, label = value),   size = 2, position = position_dodge(width = 1), vjust=1.5)+ guides(fill=guide_legend(title="Validity"))

ggsave(paste0(resultspath, "pseudostock_validity_comparison.png"), width = 15, height = 10, units = "cm")
