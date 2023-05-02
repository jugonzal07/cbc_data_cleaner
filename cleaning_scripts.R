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
    
    
    # Creates a directory for the circle if one doesnt exist
    circle_directory = file.path(getwd(), "circles", abbreviation)
    dir.create(circle_directory, showWarnings = FALSE)
    
    
    # Creates a directory if one doesnt exist
    save_directory = file.path(getwd(), "circles", abbreviation, save_directory)
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


# Takes a list of kendaul tau CSVs and merges either all the increasing or 
# decreasing species
# -p_value_threshold = If p-value below this threshold, the increasing or
#                      decreasing trend is considered significant and captured
# -is_increasing = If true, return statistically significant increasing species 
#                  If false, return statistically significant decreasing species
# RETURNS
# -merged_df - dataframe with following sets of columns:
#              --species = species with either stat significant inc/dec trends
#              --CIRCLES = all remaining columns represent p-values for the
#                          increasing or decreasing species within a given circle
merge_statistically_significant_dfs<-function(p_value_threshold, is_increasing){
  
  # Get list of kendall_tau CSVs in circles directory
  circles_filelist = list.files(path = "./circles", recursive = TRUE, pattern="*_kendall_tau.csv$")
  
  # Initialize data frame with aggregated results
  merged_df = data.frame()
  
  # Create list of increasing DFs. Could not get this working with lapply or sapply
  for (i in (1:length(circles_filelist)))
  {
    
    kend_tau_csv = paste('circles',circles_filelist[i],sep = '/')
    
    # Read in CSV from list
    stat_df = read.csv(kend_tau_csv, header = TRUE, row.names = 1)
    
    # Remove NAs
    stat_df = stat_df[!is.na(stat_df$p.value),]
    
    # Get increasing or decreasing species based on tau value
    if(is_increasing){
      sig_df = stat_df[stat_df$tau>0,]
    }else{
      sig_df = stat_df[stat_df$tau<0,]
    }
    
    # Remove those that don't meet p-value threshold
    sig_df = sig_df[sig_df$p.value <= p_value_threshold,]
    
    # Get circle name
    circle_name = strsplit(as.character(kend_tau_csv), split = "/")[[1]][2]
    
    # Drop all but p-values
    sig_p_value_df = data.frame(sig_df['p.value'])
    
    # Change the column name to circle name
    colnames(sig_p_value_df) = circle_name
    
    # Create new data frame with two columns, one with the circles p-values and the other with the associated species
    sig_p_value_df = data.frame(species = row.names(sig_p_value_df), sig_p_value_df, stringsAsFactors = FALSE) 
    
    # Append this circle to merged dataframe
    if (length(merged_df) == 0){
      
      merged_df = sig_p_value_df
      
    }else{
      merged_df = merge(merged_df, sig_p_value_df,by="species",all=T)
    }
    
  }
  
  # Sort by species and return result
  merged_df = merged_df[order(merged_df$species),]
  return(merged_df)
  
}


# Produces and saves a time-series plot for a given species of bird across a 
# list of circles. Useful for looking at trends across a region.
# INPUTS:
# -species = scientific name or common name for bird of interest (string)
# -use_scientific_name = boolean to determine whether or not the 'species' input
#                        is specified as a common or scientific name (bool)
# -circles = List of 4 character strings representing CBC circles
#            (e.g., c('VARA', 'VATP', 'VAMB', 'VACL', 'VANO')) (vector)
# -output_dir = directory to output plots
# -start_year(OPTIONAL) = start year for checking count. Defaults to 2000
# -end_year(OPTIONAL) = end year for checking count. Defaults to current year.
# OUTPUTS:
# -time series counts for all circles listed in "circles"
generate_species_time_series_plots<-function(species, use_scientific_name, 
                                             circles, output_dir,
                                             start_year = "2000",
                                             end_year = format(Sys.Date(), "%Y"))
{
  # Set CSV pattern name based on the use_scientific_name flag
  pattern = ifelse(use_scientific_name, "*_scientific_names.csv", "*_common_names.csv")
  
  # Set unmerged data frame
  circle_count_df = data.frame()
  
  # Replace any spaces with a "."
  species = gsub("\\s+", ".", species)
  
  # Iterate through each circle in circles vector
  for (circle in circles) {
    
    # Find the CSV for this circle and load it
    cbc_csv_filename = list.files(path = paste0("./circles/",circle), 
                                  pattern = pattern, full.names = TRUE)
    
    circle_df = read.csv(cbc_csv_filename)
    
    # Clean up data, remove NA and cw
    circle_df = remove_na_values_and_cw(circle_df)
    
    # Filter for just the Date and species
    circle_df = circle_df[, c("Date", "CountYear", species)]
    
    # Add circle to this dataset
    circle_df$circ = circle
    
    #Change CountYear to a human readable/understandable year (101 = year 2000)
    circle_df$CountYear = circle_df$CountYear + 1899
    
    # Change name of species column to count and CountYear to CBC_Year
    names(circle_df)[which(names(circle_df) == species)] <- "count"
    names(circle_df)[which(names(circle_df) == "CountYear")] <- "CBC_Year"
    
    # Bind this circle to previous circle data
    circle_count_df = rbind(circle_count_df, circle_df)
    
  }
  
  # convert Date column to Date class and remove NAs
  circle_count_df$Date = as.Date(circle_count_df$Date)
  circle_count_df$CBC_Year = as.Date(paste0(as.character(circle_count_df$CBC_Year), "-12-25"))
  circle_count_df$count[is.na(circle_count_df$count)] = 0
  
  # Change start and end year to numerics and filter by them
  start_year = as.numeric(start_year)
  end_year = as.numeric(end_year)
  
  circle_count_df = circle_count_df[format(circle_count_df$CBC_Year, "%Y") >= start_year,]
  circle_count_df = circle_count_df[format(circle_count_df$CBC_Year, "%Y") <= end_year,]
  
  cleaned_species = gsub("\\."," ", species)
  
  title = paste("Species count:", cleaned_species)
  subtitle = "Across select Christmas Bird Count circles"
  
  # plot using ggplot2
  ggplot(circle_count_df, aes(x = CBC_Year, y = count, color = circ)) +
    geom_line(linetype = 'solid', size = 1)+
    geom_point(size = 2)+
    labs(title = title, subtitle = subtitle, 
         x = "CBC Date", y = "Count", color = "Circle")+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme(plot.title = element_text(size = 15, face="bold", hjust=0.5),
          plot.subtitle = element_text(size = 12, hjust=0.5),
          axis.text.x = element_text(angle = -30, hjust = 0))
  
  # Creates a directory if one doesnt exist
  save_directory = file.path(getwd(), output_dir)
  dir.create(save_directory, showWarnings = FALSE)
  
  plot_name = paste0(species,".count.png")
  
  # Save plot
  ggsave(file.path(save_directory, plot_name),
         width = 9, height = 6)
  
  
  # Aggregate all counts into a common year
  counts_by_year <- aggregate(count ~ CBC_Year, 
                              data = circle_count_df, 
                              FUN = sum)
  
  counts_by_year$circ = "SumTotal"
  counts_by_year$Date = counts_by_year$CBC_Year
  
  # plot using ggplot2
  ggplot(counts_by_year, aes(x = CBC_Year, y = count)) +
    geom_line(linetype = 'solid', size = 1)+
    geom_point(size = 2)+
    labs(title = title, subtitle = subtitle, 
         x = "CBC Date", y = "Count")+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme(plot.title = element_text(size = 15, face="bold", hjust=0.5),
          plot.subtitle = element_text(size = 12, hjust=0.5),
          axis.text.x = element_text(angle = -30, hjust = 0))
  
  plot_name = paste0(species,".sum.total.png")
  
  # Save plot
  ggsave(file.path(save_directory, plot_name),
         width = 9, height = 6)
  
}
