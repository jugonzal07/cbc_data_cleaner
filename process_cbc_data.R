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

write.csv(complete_scientific_df, file = file_name_sci, row.names = FALSE)
write.csv(complete_common_name_df, file = file_name_com, row.names = FALSE)

#-----------------------------PLOTTING------------------------------------------

require(ggplot2)


# Remove count week birds and NA
plot_df = remove_na_values_and_cw(complete_common_name_df)

# Check bird count over time
species = 'Savannah Sparrow'
subtitle = paste0("CBC Counts for: ", abbreviation, " - ", circle_name)

ggplot(plot_df, aes(x=Date, y=`Savannah Sparrow`))+
  geom_line(linetype = 'dashed', color = 'steelblue', size = 0.75)+
  geom_point(color = 'steelblue', size = 3)+
  geom_text(label = plot_df[[species]], nudge_x = 100, nudge_y = 3, 
            check_overlap = T, size = 3.5)+
  labs(title = paste0(species, " Count"), subtitle = subtitle, 
       x = "CBC Date", y = "Count")+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
  scale_y_continuous(breaks = seq(0,100000,10))+
  theme(plot.title = element_text(size = 15, face="bold", hjust=0.5),
        plot.subtitle = element_text(size = 12, hjust=0.5))

# Box and whisker plots of rarer sparrows
sparrow_list = c("American Tree Sparrow",
                 "Chipping Sparrow",
                 "Fox Sparrow",
                 "Henslow's Sparrow",
                 "Lincoln's Sparrow",
                 "Savannah Sparrow",
                 "Swamp Sparrow",
                 "White-crowned Sparrow")

sparrow_df = plot_df[,colnames(plot_df) %in% sparrow_list]
melted_sparrow_df = melt(sparrow_df, variable.name = "Species", value.name = "Count")

ggplot(melted_sparrow_df, aes(y=Count, x= Species)) +
  geom_boxplot(fill="wheat")+
  labs(title = "Less Common Sparrow Counts", 
       subtitle = paste0("In ", abbreviation, " - ", circle_name),
       x = "Sparrow Species", y = "Annual Count")+
  scale_y_continuous(breaks = seq(0,150, 10), limits = c(0, 150))+
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme_minimal()+
  theme(plot.title = element_text(size = 15, face="bold", hjust=0.5),
        plot.subtitle = element_text(size = 12, hjust=0.5))

