
install.packages("tidyverse")
library(readr) # Load the readr library
library(dplyr)

covid_df <- read.csv("covid19.csv") # upload the csv file and create a dataframe(DF)
dim(covid_df) #Find the dimension of the data frame
vector_cols <- colnames(covid_df) #Find the column names
print(vector_cols)

str(vector_cols) # FInd the datastructure

head(vector_cols)
glimpse(vector_cols)

covid_df_all_states <- filter(covid_df, Province_State == "All States")

covid_df_all_states_daily <- select(covid_df_all_states, Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)

covid_df_all_states_daily_sum <- covid_df_all_states_daily %>% group_by(Country_Region) %>%
  summarise(tested = sum(daily_tested),
            positive = sum(daily_positive),
            active = sum (active),
            hospitalized = sum(hospitalizedCurr)) %>%
  arrange(-tested)

print(covid_df_all_states_daily_sum)

covid_top_10 <- head(covid_df_all_states_daily_sum, 10)
