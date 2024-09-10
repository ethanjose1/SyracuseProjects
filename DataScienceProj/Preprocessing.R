# Load the `arrow` package - used to read parquet datafiles
library(arrow)
library(tidyverse)

#for fread
library(data.table)

#read in the static house data
meta_house_DF <- read.csv(file.choose())

# sample energy usage file and weather data
sample_energy<- read_parquet("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/2023-houseData/8414.parquet")
sample_weather<- read.csv("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/weather/2023-weather-data/G4500010.csv")


#where to get other files
house_file_dir <-"https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/2023-houseData/"
weather_file_dir <- "https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/weather/2023-weather-data"

######################

#generates total energy used for HVAC (rather than all the specific attributes listed).
#then removes those fields from the DF (so that they are not counted twice)
total_electric_HVAC_energy <- function(df){
  df$total_electric_HVAC_energy_used  <-  df$out.electricity.ceiling_fan.energy_consumption +
    df$out.electricity.cooling_fans_pumps.energy_consumption+
    df$out.electricity.cooling.energy_consumption+
    df$out.electricity.freezer.energy_consumption+
    df$out.electricity.heating_fans_pumps.energy_consumption+
    df$out.electricity.heating_hp_bkup.energy_consumption+
    df$out.electricity.heating.energy_consumption+
    df$out.electricity.hot_tub_heater.energy_consumption+
    df$out.electricity.hot_tub_pump.energy_consumption+
    df$out.electricity.hot_water.energy_consumption+
    df$out.electricity.lighting_exterior.energy_consumption+
    df$out.electricity.lighting_garage.energy_consumption+
    df$out.electricity.lighting_interior.energy_consumption+
    df$out.electricity.mech_vent.energy_consumption+
    df$out.electricity.plug_loads.energy_consumption+
    df$out.electricity.pool_heater.energy_consumption+
    df$out.electricity.refrigerator.energy_consumption+
    df$out.electricity.well_pump.energy_consumption
  
  df$out.electricity.ceiling_fan.energy_consumption  <- NULL
  df$out.electricity.cooling_fans_pumps.energy_consumption <- NULL
  df$out.electricity.cooling.energy_consumption <- NULL
  df$out.electricity.freezer.energy_consumption <- NULL
  df$out.electricity.heating_fans_pumps.energy_consumption <- NULL
  df$out.electricity.heating_hp_bkup.energy_consumption <- NULL
  df$out.electricity.heating.energy_consumption <- NULL
  df$out.electricity.hot_tub_heater.energy_consumption <- NULL
  df$out.electricity.hot_tub_pump.energy_consumption <- NULL
  df$out.electricity.hot_water.energy_consumption <- NULL
  df$out.electricity.lighting_exterior.energy_consumption <- NULL
  df$out.electricity.lighting_garage.energy_consumption <- NULL
  df$out.electricity.lighting_interior.energy_consumption <- NULL
  df$out.electricity.mech_vent.energy_consumption <- NULL
  df$out.electricity.plug_loads.energy_consumption <- NULL
  df$out.electricity.pool_heater.energy_consumption <- NULL
  df$out.electricity.refrigerator.energy_consumption <- NULL
  df$out.electricity.well_pump.energy_consumption <- NULL
  
  return(df)
}



#refine the static housing data
summerize_house_data <- function(df) {
  
  #get the details from the time sample
  posix_time <- as.POSIXlt(df$time)
  df$month <- posix_time$mon + 1
  df$day <- posix_time$mday
  df$hour <- posix_time$hour
  
  #we will focus on July
  df <- df[df$month==7,]
  
  #remove anytime with an NA
  df <- df[!is.na(df$month),]
  
  #update the df, so that we have one field for HVAC usage
  df <- total_electric_HVAC_energy(df)
  
  #sort df
  df_summary <- df %>%
    arrange(month, day, hour)
  
  return(df_summary)
}


#get the weather data for a house's location
#and add the data to the house's dataframe
get_summary_weatherDF <- function(aHouse){
  
  county <- aHouse$county[1]
  file_name <- paste0(county, ".csv")
  
  f <- paste0(weather_file_dir, "/", file_name)
  
  #read the file (fread is quick)
  data_table <- fread(f)
  weatherDF <- as.data.frame(data_table)
  
  #parse the times
  posix_time <- as.POSIXlt(weatherDF$date_time)
  weatherDF$month <- posix_time$mon + 1
  weatherDF$day <- posix_time$mday
  weatherDF$hour <- posix_time$hour
  
  #we will focus on July
  weatherDF <- weatherDF[weatherDF$month==7,]
  
  #sort the df
  df_summary <- weatherDF %>%
    arrange(month, day, hour)
  
  #since both were grouped/sorted the same way, we can combine columns
  aHouse$temp <- df_summary$`Dry Bulb Temperature [°C]`
  aHouse$wind <- df_summary$`Wind Speed [m/s]`
  aHouse$horizonal_rad <- df_summary$`Global Horizontal Radiation [W/m2]`
  aHouse$normal_rad <- df_summary$`Direct Normal Radiation [W/m2]`
  aHouse$diffuse_rad <- df_summary$`Diffuse Horizontal Radiation [W/m2]`
  return(aHouse)
}


#my assumption on the value of different types of insulation
#to see how useful is better insulation
annotation <- list(
  'in.insulation_ceiling' = list(
    'None' = 0,
    'Uninsulated' = 0,
    'R-7' = 1,
    'R-13' = 2,
    'R-19' = 2,
    'R-30' = 3,
    'R-38' = 4,
    'R-49' = 5
  ),
  'in.insulation_floor' = list(
    'None' = 0,
    'Uninsulated' = 0,
    'Ceiling R-19' = 2,
    'Ceiling R-13' = 1
  ),
  'in.insulation_foundation_wall' = list(
    'None' = 0,
    'Wall R-15, Exterior' = 3,
    'Wall R-10, Exterior' = 2,
    'Uninsulated' = 0,
    'Wall R-5, Exterior' = 1
  ),
  'in.insulation_rim_joist' = list(
    'None' = 0,
    'R-5, Exterior' = 1,
    'R-10, Exterior' = 2,
    'Uninsulated' = 0,
    'R-15, Exterior' = 3
  ),
  'in.insulation_roof' = list(
    'None' = 0,
    'Unfinished, Uninsulated' = 0,
    'Finished, Uninsulated' = 0,
    'Finished, R-7' = 2,
    'Finished, R-13' = 4,
    'Finished, R-19' = 6,
    'Finished, R-38' = 8,
    'Finished, R-30' = 9,  
    'Finished, R-49' = 10
  ),
  'in.insulation_slab' = list(
    'None' = 0,
    '2ft R5 Under, Horizontal' = 2,
    '2ft R10 Under, Horizontal' = 3,
    '2ft R10 Perimeter, Vertical' = 3,
    'Uninsulated' = 1,
    '2ft R5 Perimeter, Vertical' = 2
  ),
  'in.insulation_wall' = list(
    'None' = 0,
    'Wood Stud, R-11' = 2,
    'CMU, 6-in Hollow, Uninsulated' = 1,
    'CMU, 6-in Hollow, R-7' = 2,
    'CMU, 6-in Hollow, R-11' = 3,
    'CMU, 6-in Hollow, R-15' = 3,
    'Brick, 12-in, 3-wythe, Uninsulated' = 2,
    'Brick, 12-in, 3-wythe, R-15' = 4,
    'Brick, 12-in, 3-wythe, R-19' = 5,
    'CMU, 6-in Hollow, R-19' = 6,
    'Brick, 12-in, 3-wythe, R-11' = 3,
    'Wood Stud, R-15' = 3,
    'Wood Stud, R-19' = 4,
    'Brick, 12-in, 3-wythe, R-7' = 3,
    'Wood Stud, Uninsulated' = 1,
    'Wood Stud, R-7' = 1
  ),
  'in.windows' = list(
    'None' = 0,
    'Double, Clear, Metal, Air, Exterior Clear Storm' = 4,
    'Single, Clear, Metal' = 1,
    'Double, Clear, Metal, Air' = 4,
    'Single, Clear, Metal, Exterior Clear Storm' = 3,
    'Triple, Low-E, Non-metal, Air, L-Gain' = 4,
    'Single, Clear, Non-metal, Exterior Clear Storm' = 2,
    'Single, Clear, Non-metal' = 1,
    'Double, Clear, Non-metal, Air' = 2,
    'Double, Clear, Non-metal, Air, Exterior Clear Storm' = 3,
    'Double, Low-E, Non-metal, Air, M-Gain' = 4
  )
)


#use these columns to check about insulation
columnsToCheck <- names(annotation)
columnsToCheck <- columnsToCheck[ columnsToCheck != "in.insulation_roof"]

get_other_insulation <- function(static_house_df) {
  
  # Create an empty list to store individual data
  insulation_value <- list()
  
  # Iterate over each column to check
  for (col in columnsToCheck) {
    # Get the corresponding annotation value for the column and row value
    insulation_value[[col]] <- annotation[[col]][[static_house_df[[col]]]]
  }
  
  # Calculate the sum of individual data
  sumIndData <- sum(unlist(insulation_value))
  
  return(sumIndData)
}

#maybe just roof insulation is useful
get_roof_insulation <- function(static_house_df){
  
  val <- annotation[["in.insulation_roof"]][[static_house_df[["in.insulation_roof"]]]]
  
  return(val)
}


# Loop through each file
first_time = TRUE

num_to_do <- nrow(meta_house_DF)


#########################
# NOTE: This might take some time
#########################

for (i in 1:num_to_do) {
  row <- meta_house_DF[i,]
  file <- paste0(row$bldg_id, ".parquet")
  
  f = paste0(house_file_dir, file)
  
  number <- row$bldg_id
  
  if(as.integer(i/10) * 10 == i)
    print(paste("house number", number, " iteration: ", i, " out of: ", num_to_do))  
  
  df <- read_parquet(f)
  
  new_df <- summerize_house_data(df)
  
  #get the relevant static info
  this_house_static_data <- meta_house_DF[meta_house_DF$bldg_id==number,]
  
  new_df$bldg_id <- number
  new_df$window_areas <- this_house_static_data$in.window_areas
  new_df$windows <- this_house_static_data$in.windows
  new_df$number_of_stories <- this_house_static_data$in.geometry_stories
  new_df$usage_level <- this_house_static_data$in.usage_level
  
  new_df$income <- this_house_static_data$in.income
  new_df$sqft <- this_house_static_data$in.sqft
  new_df$county <- this_house_static_data$in.county
  new_df$lat <- this_house_static_data$in.weather_file_latitude
  new_df$long <- this_house_static_data$in.weather_file_longitude
  new_df$in.hvac_cooling_efficiency <- this_house_static_data$in.hvac_cooling_efficiency
  
  new_df$roof_insulation <- get_roof_insulation(this_house_static_data)
  new_df$other_insulation <- get_other_insulation(this_house_static_data)
  
  new_df$ducts <- this_house_static_data$in.ducts
  
  #get the weather data, at the day current summary level
  new_df<-get_summary_weatherDF(new_df)
  
  # get the target variable (energy_per_sqft)
  new_df$energy_per_sqft <- new_df$total_electric_HVAC_energy_used / new_df$sqft
  
  if(first_time) {
    first_time=FALSE
    info <- new_df
  } else {
    info <- rbind(info, new_df)
  }
  
}


#############
#change the strings


#update attributes within the DF
convert_strings <- function(data){
  
  #convert income into something useful
  data$income <- as.factor(data$income)
  new_order <- c("<10000", "10000-14999", "15000-19999", "20000-24999", "25000-29999", "30000-34999", "35000-39999",  
                 "40000-44999", "45000-49999", "50000-59999", "60000-69999", "70000-79999", "80000-99999",
                 "100000-119999", "120000-139999",  "140000-159999", "160000-179999",  "180000-199999", "200000+")
  data$income <- factor(data$income, levels = new_order)
  data$income <- as.integer(data$income)
  
  
  #convert ducks
  data$ducts <- as.factor(data$ducts)
  new_order <- c("None", "30% Leakage, Uninsulated",  "0% Leakage, Uninsulated", "10% Leakage, R-8")
  data$ducts <- factor(data$ducts, levels = new_order)
  data$ducts <- as.integer(data$ducts)
  
  
  data$in.hvac_cooling_efficiency <- as.factor(data$in.hvac_cooling_efficiency)
  new_order <- c("None",  "Heat Pump", "AC, SEER 10", "AC, SEER 13",
                 "AC, SEER 15", "AC, SEER 8" ,"Room AC, EER 8.5" , "Room AC, EER 10.7",
                 "Room AC, EER 9.8", "Room AC, EER 12.0")
  data$in.hvac_cooling_efficiency <- factor(data$in.hvac_cooling_efficiency, levels = new_order)
  data$in.hvac_cooling_efficiency <- as.integer(data$in.hvac_cooling_efficiency)
  
  
  data$usage_level <- as.factor(data$usage_level)
  new_order <- c( "Low", "Medium" , "High" )
  data$usage_level <- factor(data$usage_level, levels = new_order)
  data$usage_level <- as.integer(data$usage_level)
  
  data$window_areas <- as.factor(data$window_areas)
  new_order <- c("F6 B6 L6 R6", "F9 B9 L9 R9", "F12 B12 L12 R12",
                 "F15 B15 L15 R15", "F18 B18 L18 R18" , "F30 B30 L30 R30")
  data$window_areas <- factor(data$window_areas, levels = new_order)
  data$window_areas <- as.integer(data$window_areas)
  
  data$windows <- as.factor(data$windows)
  new_order <- c(
    "Single, Clear, Metal",    
    "Single, Clear, Non-metal" ,    
    "Single, Clear, Metal, Exterior Clear Storm",
    "Single, Clear, Non-metal, Exterior Clear Storm",
    "Double, Clear, Metal, Air" ,
    "Double, Clear, Metal, Air, Exterior Clear Storm",
    "Double, Clear, Non-metal, Air" ,    
    "Double, Low-E, Non-metal, Air, M-Gain",      
    "Double, Clear, Non-metal, Air, Exterior Clear Storm",
    "Triple, Low-E, Non-metal, Air, L-Gain")
  data$windows <- factor(data$windows, levels = new_order)
  data$windows <- as.integer(data$windows)
  
  data$county <- gsub("G", "", data$county, ignore.case = TRUE)
  data$county <- as.integer(data$county)
  
  #attributes not useful for predictions
  data$house <- NULL #have building_id
  data$poverty_level <- NULL #have income
  data$max_temp <- NULL #have min_temp
  
  return(data)
}

#now convert strings to integers (via factors)
data <- convert_strings(info)
data <- data[!is.na(data$income),]


### remove irrelevant columns
data<- data[, -c(1:25)]


# save the data
write_parquet(data, "df_for_387_only_July_cleaned.parquet")
write_csv(data, "/Users/ethan/Documents/Data Science/Project/df_for_387_only_July_cleaned.csv")

