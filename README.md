# Christmas Bird Count Data Cleaner

The R scripts in this repo transforms CSV's from [Audubon's Christmas Bird Count® portal](https://netapp.audubon.org/CBCObservation/Historical/ResultsByCount.aspx) and transforms them into more wieldable tables for data analysis. Per the National Audobo Society's website "*CBC Data are provided by National Audubon Society and through the generous efforts of Bird Studies Canada and countless volunteers across the Western Hemisphere.*" More information can be found at links below:

- www.audubon.org
- www.christmasbirdcount.org
- www.bsc-eoc.org (for Canadian users)

## Features

The main purpose of this repo is the `parse_cbc_csv_file` function found in `cleaning_scripts.R`. This function reads in a CBC CSV files and returns a named list with post processed information for ease of us. This information is in the form of a named list with the following:

- abbreviation = Abbreviation for CBC circle name
- circle_name = Name of CBC Circle name
- latitude = latitude of CBC circle location
- longitude = longitude of CBC circle location
- long_bird_data = Data frame containing "melted" list of observations. Useful for plotting
- complete_scientific_df = Dataframe with CBC data using bird's scientific names for columns
- complete_common_name_df = Dataframe with CBC data using bird's common names for columns


## Example Studies

This repo also includes very basic analyses performed using the post-processed data in R along with the R library, `ggplot2`.
These examples can be found in `process_cbc_data.R`.

https://github.com/jugonzal07/cbc_data_cleaner/blob/main/sample_outputs/savannah_sparrow_count.png

https://github.com/jugonzal07/cbc_data_cleaner/blob/main/sample_outputs/sparrow_count_spread.png


