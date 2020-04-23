library(tidyverse)
library(usmap)
library(ggplot2)
library(rerddap)
library(maps)

# read CSVs
columns <- c('district', 'fin_sub', 'chargeable_fin_number', 'po_name',
             'unit_name', 'property_address', 'county', 'city', 'state', 'zip', 
             'property_state', 'ownership', 'fdb_id', 'ams_locale',
             'fdb_facility_type', 'fdb_facility_subtype', 
             'building_ownership_description', 'land+description', 
             'space_certified_indicator', 'building_occurrence_date',
             'area_sq_ft')

data_directory <- '/Users/amandeeprathee/work/covid-and-voting/data'
csv_names <- list.files(data_directory)

setwd(data_directory)

data <- read_csv("ak.csv", col_names=columns, skip=4)

for (csv_file in csv_names[2:length(csv_names)]) {
    new_data <- read_csv(csv_file, col_names=columns, skip=4)
    data <- rbind(data, new_data)
}

# plot count and counties
data$building_type <- ifelse(str_detect(data$unit_name, '.*MAIN OFFICE.*'), "post_office", "not_post_office")
table(data$building_type)

data <- data[data$building_type == 'post_office', ]
str(data)

#str(data)
#head(data)
write.csv(data, file="./data_final.csv", quote = FALSE, row.names = FALSE)
