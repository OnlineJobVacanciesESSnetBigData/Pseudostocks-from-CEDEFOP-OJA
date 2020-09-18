# Overview
R code for calculating and plotting montly and quarterly pseudo-stocks of job ads from the CEDEFOP data for Germany

This code is intended to be run on the EU Dataplatform.

# Input data
 This code uses, as input, CEDEFOP online job vacancy data from the Athena database running on the EU Datalab. Filters are set to include only data from Sources located in Germany. 
 Use the following query to draw all the raw data from the database. Depending on the size of the sample, it might be necessary to split the query into multiple parts.
 ```
 query <- "SELECT * FROM cedefop_datalab.ft_document_en_v4 WHERE ( sourcecountry = 'DE' OR country='DEUTSCHLAND') ORDER BY general_id, source;"
 data <- get_data(query)
 save(data, file="OJVsample.rdata")
 ```
 
# Step by step from data preparation to result graphs and tables

## Data preparation

Use the following two scripts to clean and prepare the dataset for further analysis. 
Adapt the paths in each script to your installation.

- Step1_data_preparation.r
- Step1a_Deduplication_and_sizereduction.r

## Pseudostock calculations
This script calculates which job ads are valid at which point in time and saves this information to a list-object. 

- Step2_Daylist_creation_functions.r

## Pseudostock aggregates and output as graphs and tables
- Step3_pseudostocks_variable_validity.r
- Step3a_pseudostock_index.r

## Compare aggregates for 20, 30 and 40 days of validity 
This is an additional step which compares pseudostocks at variable assumed lengths of validity. It requires multiple passes of the script from Step2, one for each validity period.

- Step3_pseudostock_validity_comparison.r

