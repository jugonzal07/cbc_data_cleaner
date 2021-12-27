library(stringr)
library(reshape2)
require(ggplot2)

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

# Returns just the bird count columns from a completed dataframe from 
# parse_cbc_csv_file
# -complete_df = either a complete_scientific_df or complete_common_name_df 
#                from parse_cbc_csv_file
get_completed_df_bird_count_columns<-function(complete_df){
  
  # Get first and last column index of bird data
  first_index = which(colnames(complete_df)== 'calendar_year')+1
  last_index = length(colnames(complete_df))
  
  return(complete_df[,first_index:last_index])
}


# Runs a Kendall Tau analysis for ordered time series data to check if there
# is a statistically positive or negative relationship over time. 
# INPUTS:
# -species_count = vector representing a species count with respect to dates in 
#                  "dates" vector (integer)
# -dates = vector of dates associated with species count (Date)
# RETURNS:
# -length 2 vector
# --[1] = Tau statistic
# --[2] = P-value
get_kendall_tau_result_for_species<-function(species_count, dates)
{
  res = cor.test(x = as.numeric(dates), 
                 y = species_count, 
                 method = "kendall", exact = FALSE)
  
  return(c(res$estimate, res$p.value))
}

# Runs a Kendall Tau analysis for ordered time series data to check if there
# is a statistically positive or negative relationship over time for all species
# in a completed DF from parse_cbc_csv_file
# INPUT
# -complete_df = either a complete_scientific_df or complete_common_name_df 
#                from parse_cbc_csv_file
# OUTPUT
# -kendall_tau_df = dataframe with two columns, Tau statistic and p.value. Each
#                   row is a bird species
get_kendall_tau_statistics<-function(complete_df){
  
  # Clean out count week, NA values, and convert to numeric counts
  clean_numeric_df = remove_na_values_and_cw(complete_df)
  
  # Create a dataframe of just the bird counts
  bird_count_df = get_completed_df_bird_count_columns(clean_numeric_df)
  
  # Apply Kendall Tau statistic for each species
  kendall_tau_results = sapply(names(bird_count_df), function(x){
    get_kendall_tau_result_for_species(bird_count_df[[x]], clean_numeric_df$Date)
  })
  
  # Rename rows to the relevant statistics
  row.names(kendall_tau_results) = c("tau", "p.value")
  
  # Convert to dataframe
  kendall_tau_df <- as.data.frame(t(kendall_tau_results))
  
  return(kendall_tau_df)
}


# Saves a time series plot if statistically significant. Sorted into different
# directories and saves Kendall Tau statistics onto plot
# INPUTS:
# -species = species of interest (character)
# -statistic_df = kendall_tau_df from get_kendall_tau_statistics (data.frame)
# -cbc_data = named list from parse_cbc_csv_file (list)
# OUTPUTS:
# -boolean TRUE if p-value is < 0.05. FALSE otherwise
# -PNG file in either "plot_decreasing" or "plot_increasing" directory
save_important_species_trend_plots<-function(species, statistic_df, cbc_data)
{
  
  # Get this species p.value
  p_value = statistic_df$p.value[row.names(statistic_df)==species]
  
  if(is.na(p_value)) return(FALSE)
  
  # Check if null hypothesis can be rejected (p.value <= 0.05)
  if(p_value <= 0.05){
    
    # Parse relevant CBC data
    abbreviation = cbc_data$abbreviation
    circle_name = cbc_data$circle_name
    complete_common_name_df = cbc_data$complete_common_name_df
    
    # Remove count week birds and NA
    # TODO: Inefficient to do this for every species
    clean_numeric_df = remove_na_values_and_cw(complete_common_name_df)
    
    # Check if this species is increasing or decreasing
    tau = statistic_df$tau[row.names(statistic_df)==species]
    is_increasing = tau > 0
    
    # Format p-value
    if(p_value < 0.01){
      p_value = format(p_value, scientific = TRUE, digits = 3)
    } else {
      p_value = round(p_value, 2)
    }
   
    # Plot parameters
    title = paste0(species, " Count")
    subtitle = paste0("CBC Counts for: ", abbreviation, " - ", circle_name)
    max_limit = max(clean_numeric_df[[species]])
    label_text = paste0("Kendall Tau Statistics\n",
                        "tau: ", round(tau,2), ", p-value: ", p_value)
    
    # Put label on top right or top left of plot
    if(is_increasing){
      x_label_pos =  min(clean_numeric_df$Date) + 365*2.5
    }else {
      x_label_pos =  max(clean_numeric_df$Date) - 365*2.5
    }
    
    cbc_plot = ggplot(clean_numeric_df, aes(x=Date, 
                                            y=.data[[species]], 
                                            label = label_text))+
            geom_line(linetype = 'dashed', size = 0.75)+
            geom_point(size = 3.5)+
            geom_label(x=x_label_pos, y=max_limit, label = label_text)+
            labs(title = title, subtitle = subtitle, 
                 x = "CBC Date", y = "Count")+
            scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
            scale_y_continuous(n.breaks = 10, limits = c(0, max_limit*1.1)) +
            theme(plot.title = element_text(size = 15, face="bold", hjust=0.5),
                  plot.subtitle = element_text(size = 12, hjust=0.5))
    
    # Plot save parameters
    save_directory = if(is_increasing) "plots_increasing" else "plots_decreasing"
    plot_name = paste0(gsub(" ", "_", species), "_", abbreviation, ".png")
    plot_name = gsub("/","_", plot_name)
    plot_name = gsub("\\(","", plot_name)
    plot_name = gsub("\\)","", plot_name)
    plot_name = gsub("'","", plot_name)
    
    save_directory = file.path(getwd(), save_directory)
    
    # Creates a directory if one doesnt exist
    dir.create(save_directory, showWarnings = FALSE)
    
    # Save plot
    ggsave(file.path(save_directory, plot_name),
           width = 9, height = 6)
    
    return(TRUE)
    
  }else {
    
    # Cannot reject null hypothesis. Do not plot this species
    return(FALSE)
    
  }
  
}
