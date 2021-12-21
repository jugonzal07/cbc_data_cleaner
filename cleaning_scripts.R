library(stringr)
library(reshape2)


# Parses a CBC CSV file and returns cleaned up data in named list
# -file_name = path to CBC CSV file from:
#    https://netapp.audubon.org/CBCObservation/Historical/ResultsByCount.aspx
# RETURNS
# -cbc_data - a named list with the following elements
# --abbreviation = Abbreviation for CBC circle name
# --circle_name = Name of CBC Circle name
# --latitude = latitude of location
# --longitude = longitude of location
# --long_bird_data = Data frame containing "melted" list of observations. Useful for plotting
# --complete_scientific_df = Dataframe with CBC data using bird's scientific names for columns
# --complete_common_name_df = Dataframe with CBC data using bird's common names for columns
parse_cbc_csv_file <- function(file_name)
{
  raw_df = read.csv(file_name, header = FALSE, stringsAsFactors = FALSE)
  
  # Read in general information
  # -NOTE: General data is always on 2nd row, hard-coded for this purpose
  # -- Order goes Circle Name, Abbreviation, then coordinates separated by "/"
  circle_name = as.character(raw_df[2,1])
  abbreviation = as.character(raw_df[2,2])
  coordinates = unlist(strsplit(as.character(raw_df[2,3]), split = "/"))
  lat = as.numeric(coordinates[1])
  long = as.numeric(coordinates[2])
  
  #--------------------------------------------------------------
  #-- Create named list mapping count year to date (mm/dd/YYYY)--
  #--------------------------------------------------------------
  first_index = which(raw_df[,1] == "CountYear5") + 1
  last_index = which(raw_df[,1] == "CountYear4") - 1
  count = unlist(raw_df[first_index:last_index, ][1])
  date = unlist(raw_df[first_index:last_index, ][2])
  count_year_to_date_mapping = setNames(date, count)
  
  n_counts = length(count_year_to_date_mapping)
  
  #--------------------------------------------------------------
  #------- Weather data (day of Count weather conditions)--------
  #--------------------------------------------------------------
  header_index = which(raw_df[,1] == "CountYear3")
  first_index = header_index + 1
  last_index = which(raw_df[,1] == "CountYear5")-1
  
  # Construct data frame and give appropriate column names
  count_year_info_df = raw_df[first_index:last_index,]
  names(count_year_info_df) = lapply(raw_df[header_index,], as.character)
  
  # Rename columns more appropriately
  names(count_year_info_df)[names(count_year_info_df)=="CountYear3"] = "CountYear" 
  names(count_year_info_df)[names(count_year_info_df)=="LowTemp"] = "LowTemp.F" 
  names(count_year_info_df)[names(count_year_info_df)=="HighTemp"] = "HighTemp.F" 
  
  # Add Dates
  count_year_info_df$Date = as.Date(sapply(count_year_info_df$CountYear, 
                                           function(x) unname(count_year_to_date_mapping[x])), "%m/%d/%Y")
  
  # Move to first column
  count_year_info_df = count_year_info_df[,c(ncol(count_year_info_df), 1:(ncol(count_year_info_df)-1))]
  
  # Change temps to numerics
  count_year_info_df$LowTemp.F = sapply(count_year_info_df$LowTemp.F, 
                                        function(x){as.numeric(gsub(' Fahrenheit', '', x))}, 
                                        USE.NAMES = FALSE)
  
  count_year_info_df$HighTemp.F = sapply(count_year_info_df$HighTemp.F, 
                                         function(x){as.numeric(gsub(' Fahrenheit', '', x))}, 
                                         USE.NAMES = FALSE)
  
  #--------------------------------------------------------------
  #------------------------- Bird Count -------------------------
  #--------------------------------------------------------------
  
  header_index = which(raw_df[,1] == "COM_NAME")
  first_index = header_index + 1
  last_index = which(raw_df[,1] == "CountYear1")-1
  
  # Construct data frame and give appropriate column names
  raw_bird_count_df = raw_df[first_index:last_index,][1:3]
  names(raw_bird_count_df) = c("names", "count_year_data", "bird_count")
  
  # Remove \n characters and split up scientific and common names
  # TODO: Works but man is this ugly
  common_names = unname(sapply(raw_bird_count_df$names, function(x) unlist(strsplit(x, "\\n"))[1]))
  scientific_names = unname(sapply(raw_bird_count_df$names, function(x) unlist(strsplit(x, "\\[|\\]"))[2]))
  
  # Parse out calendar year, number of participants, and total hours
  calendar_year = sapply(raw_bird_count_df$count_year_data, 
                         function(x) unlist(strsplit(x, " \\["))[1],
                         USE.NAMES = FALSE)
  
  participants = as.numeric(sapply(raw_bird_count_df$count_year_data, 
                                   function(x) unlist(strsplit(x, "Participants: |\\n"))[4],
                                   USE.NAMES = FALSE))
  
  total_hrs = as.numeric(sapply(raw_bird_count_df$count_year_data, 
                                function(x) unlist(strsplit(x, "Total Hrs.: "))[2],
                                USE.NAMES = FALSE))
  
  # Append to count_year_info_df, but first reverse order of rows in 
  # count_year_info DF to match bird count data.
  count_year_info_df = count_year_info_df[rev(rownames(count_year_info_df)),]
  
  count_year_info_df$participants = participants[1:nrow(count_year_info_df)]
  count_year_info_df$total_hrs = total_hrs[1:nrow(count_year_info_df)]
  
  # Construct bird count DF, long form
  bird_count_df = as.data.frame(cbind(calendar_year, common_names, scientific_names, bird_count= raw_bird_count_df$bird_count))
  
  # Cast it back together into coherent tables
  bird_count_scientific_df = dcast(bird_count_df, 
                                   calendar_year ~ scientific_names, 
                                   value.var = "bird_count")
  
  bird_count_common_name_df = dcast(bird_count_df, 
                                    calendar_year ~ common_names, 
                                    value.var = "bird_count")
  
  # Merge bird count data with count year data for a complete data frame
  complete_scientific_df = cbind(count_year_info_df, bird_count_scientific_df)
  complete_common_name_df = cbind(count_year_info_df, bird_count_common_name_df)
  
  # Construct return data structure (i.e., a named list)
  cbc_data = list()
  cbc_data$abbreviation = abbreviation
  cbc_data$circle_name = circle_name
  cbc_data$latitude = lat
  cbc_data$longitude = long
  cbc_data$long_bird_data = bird_count_df
  cbc_data$complete_scientific_df = complete_scientific_df
  cbc_data$complete_common_name_df = complete_common_name_df
  
  return(cbc_data)
  
  
}

# Changes NA's and count week values to 0s. Converts counts to numerics as well.
# -complete_df = either a complete_scientific_df or complete_common_name_df from parse_cbc_csv_file
remove_na_values_and_cw<-function(complete_df){
  
  # Get first and last column index of bird data
  first_index = which(colnames(complete_df)== 'calendar_year')+1
  last_index = length(colnames(complete_df))
  
  
  # Clean up NA and blanks
  complete_df[is.na(complete_df)] = 0
  complete_df[,first_index:last_index][complete_df[,first_index:last_index] == ""] = 0
  complete_df[,first_index:last_index][complete_df[,first_index:last_index] == "cw"] = 0
  complete_df[,first_index:last_index] = lapply(complete_df[,first_index:last_index], as.numeric)
  
  return(complete_df)
  
}
