library(readxl)
library(dplyr)

crime_index = read_xlsx("_Index_Crimes_by_County_and_Agency__Beginning_1990.xlsx", sheet=1)

is.na(crime_index)

crime_index[rowSums(is.na(crime_index))==0,]

crime_index[is.na(crime_index)] <- 12

index_by_country = crime_index %>% group_by(crime_index$County)
