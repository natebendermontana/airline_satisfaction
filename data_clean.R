#### Setup / read in data ####
data_path <- here("data", "airline_passenger_satisfaction.csv")
dict_path <- here("data", "data_dictionary.csv")
df <- read.csv(data_path)
dict <- read.csv(dict_path) 

#### EDA ####
# very little missing data, only 393 rows missing arrival.delay data
skimr::skim(df)

# some strong correlations to take note of
#corrgram(sampled_data)
colnames(df)

# filter out small amount of missing data and convert all chr variables to factors
df <- df %>%
  filter(complete.cases(.)) %>% 
  mutate_if(is.character, as.factor)

# List of 1-5 satisfaction survey vars to be converted to factors
selected_vars <- c(
  "Departure.and.Arrival.Time.Convenience", 
  "Ease.of.Online.Booking",
  "Check.in.Service", 
  "Online.Boarding", 
  "Gate.Location",
  "On.board.Service", 
  "Seat.Comfort", 
  "Leg.Room.Service",
  "Cleanliness", 
  "Food.and.Drink", 
  "In.flight.Service",
  "In.flight.Wifi.Service", 
  "In.flight.Entertainment",
  "Baggage.Handling"
)

# Convert selected variables to factors
df <- df %>%
  mutate(across(all_of(selected_vars), as.factor))

# ensure that "Satisfied" is the reference level of the dep var, for ease of model interpretation
df$Satisfaction <- relevel(df$Satisfaction, ref = "Satisfied")

# Write final df to CSV file
clean_df_path <- here("data", "df_clean.csv")
write.csv(df, clean_df_path, row.names = FALSE)

# Display the path to the saved CSV file
cat("Final CSV file saved at:", clean_df_path, "\n")
