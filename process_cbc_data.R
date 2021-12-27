setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('cleaning_scripts.R')

# Data pulled from:
# -https://netapp.audubon.org/CBCObservation/Historical/ResultsByCount.aspx

file_name = "vatp_2000_2021.csv" #<-- must be in same directory as this script

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

write.csv(complete_scientific_df, file = file_name_sci, row.names = FALSE)
write.csv(complete_common_name_df, file = file_name_com, row.names = FALSE)


#----------------------------STATISTICS-----------------------------------------

# Write out Kendall Tau statistics for every species in data
kendall_tau_df = get_kendall_tau_statistics(complete_common_name_df)
write.csv(kendall_tau_df, file = file_name_tau, row.names = TRUE)

#-----------------------------PLOTTING------------------------------------------

# Produce plots for each species with p-value < 0.05
changing_species = sapply(row.names(kendall_tau_df), 
                          save_important_species_trend_plots, 
                          statistic_df = kendall_tau_df,
                          cbc_data = cbc_data)


