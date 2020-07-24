
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

covid_df_all_states_daily