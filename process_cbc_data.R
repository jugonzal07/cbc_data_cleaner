setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('cleaning_scripts.R')

# Data pulled from:
# -https://netapp.audubon.org/CBCObservation/Historical/ResultsByCount.aspx

file_name = "vano_2000_2021.csv" #<-- must be in same directory as this script

# Parse CBC CSV file and compile a clean table by year
cbc_data = parse_cbc_csv_file(file_name)

# Pull out data from cbc_data object
abbreviation = cbc_data$abbreviation
circle_name = cbc_data$circle_name
lat = cbc_data$latitude 
long = cbc_data$longitude 
melted_bird_count_df = cbc_data$long_bird_data
complete_scientific_df = cbc_data$complete_scientific_df 
complete_common_name_df = cbc_data$complete_common_name_df

# Write out files
file_name_prefix = paste(abbreviation,circle_name,lat,long, sep = "_")
file_name_sci = paste0(file_name_prefix, "_scientific_names.csv")
file_name_com = paste0(file_name_prefix, "_common_names.csv")
file_name_tau = paste0(file_name_prefix, "_kendall_tau.csv")

# Creates a directory for the circle if one doesnt exist
circle_directory = file.path(getwd(), "circles", abbreviation)
dir.create(circle_directory, showWarnings = FALSE)

write.csv(complete_scientific_df, file =  file.path(getwd(), "circles", abbreviation, file_name_sci), row.names = FALSE)
write.csv(complete_common_name_df, file = file.path(getwd(), "circles", abbreviation, file_name_com), row.names = FALSE)


#----------------------------STATISTICS-----------------------------------------

# Write out Kendall Tau statistics for every species in data
kendall_tau_df = get_kendall_tau_statistics(complete_common_name_df)
write.csv(kendall_tau_df, file = file.path(getwd(), "circles", abbreviation, file_name_tau), row.names = TRUE)

#-----------------------------PLOTTING------------------------------------------

# Produce plots for each species with p-value < 0.05
changing_species = sapply(row.names(kendall_tau_df), 
                          save_important_species_trend_plots, 
                          statistic_df = kendall_tau_df,
                          cbc_data = cbc_data)

# Do the following when you have multiple circles ran and want to investigate a 
# trends across a list of circles. Example below assumes you have data generated
# for VACL, VAMB, VANO, and VATP for American Kestrel (Falco sparverius).

species = 'Northern Bobwhite'
use_scientific_name = FALSE
circles = c('VACL', 'VAMB', 'VACN', 'VATP', 'VANS')
output_dir = "count_plots"

generate_species_time_series_plots(species, use_scientific_name, circles, output_dir)

#---------------------------AGGREGATING K-TAU STATS-----------------------------

# Do the following when you have multiple circles ran and want to combine their
# results! The code below merges all statistically significant results in the
# circles directory into two CSV files for easy interpretation

# Set value threshold of 0.05 and merge results of all circles in "circles"
# directory
p_value_threshold = 0.05

inc_species = merge_statistically_significant_dfs(p_value_threshold, TRUE)
dec_species = merge_statistically_significant_dfs(p_value_threshold, FALSE)

# Write out results
write.csv(inc_species, 
          file = file.path(getwd(), "statistics", "increasing_species.csv"), 
          row.names = FALSE)
write.csv(dec_species, 
          file = file.path(getwd(), "statistics", "decreasing_species.csv"), 
          row.names = FALSE)


