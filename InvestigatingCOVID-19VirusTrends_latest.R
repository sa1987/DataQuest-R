#####################
# Which countries have had the highest number of positive cases against the number of tests?
# ######################################################################
# Download the covid19.csv CSV file.
# 
# Load this file using the function read_csv() from the readr package.
# Don't forget to load the readr library first using the following command: library(readr).
#         Store the result in the variable named covid_df.
# 
#     Determine the dimension of the dataframe, covid_df by using the function dim()
# 
#     Determine the column names of covid_df using the colnames() function.
#         Store the result in the variable named vector_cols.
#         Display the content of this variable.
#         What data structure the vector_cols variable represents?
# 
#     Display the first few rows of the covid_df dataset using the function head()
# 
#     Display the summary of the covid_df dataset using the function glimpse() from the tibble package.
#         Why is the glimpse() function useful when exploring a new dataset?
# 
# 

##################

library(readr)
library(tidyverse)

#Understanding the Data

covid_df=read_csv("covid19.csv")
dim(covid_df)
vector_cols <- colnames(covid_df)
print(vector_cols)
str(vector_cols)
head(covid_df)
glimpse(covid_df)

#Isolate the data with All_state info, not state wise
covid_df_all_states <- covid_df %>% filter(Province_State == "All States")

#Isolate the columns related to date wise information
covid_df_all_states_daily <- covid_df_all_states %>% select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)

#Extracting the Top Ten Tested Cases Countries - Groupby states and summarize the data

covid_df_all_states_daily_sum <- covid_df_all_states_daily %>% group_by(Country_Region) %>% 
  summarise(
  tested = sum(daily_tested),
  positive = sum(daily_positive),
  active = sum(active),
  hospitalized = sum(hospitalizedCurr)) %>%
  arrange(-tested)
#Print the data  
print(covid_df_all_states_daily_sum)

#Select the top 10
covid_top_10 <- head(covid_df_all_states_daily_sum,10)

#Extract Vectors

countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10[["positive"]]
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10 %>% pull(hospitalized)

#Name the vectors
names(positive_cases) < countries
names(tested_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries


#Divide the positive cases by tested cases
positive_cases/tested_cases

positive_tested_top_3 <- c("United Kingdom" = 0.11, "United States" = 0.10, "Turkey" = 0.08)


#Different method 
###########################################
Positive_tested_percentage <- covid_top_10 %>% mutate(case_percent = positive/tested) %>%
  arrange(-case_percent) %>% head(3) 

Positive_tested_top3  <- Positive_tested %>%
  select(Country_Region, case_percent)


Positive_tested_top3

##################################################

#Continue from line 78
######

#Keeping relevant information


# Creating vectors
united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)

# Creating the matrix covid_mat
covid_mat <- rbind(united_kingdom, united_states, turkey)

# Naming columns
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")

# Displaying the matrix
covid_mat

#Putting all together 

question <- "Which countries have had the highest number of positive cases against the number of tests?"

answer <- c("Positive tested cases" = positive_tested_top_3)

datasets <- list(
  original = covid_df,
  allstates = covid_df_all_states,
  daily = covid_df_all_states_daily,
  top_10 = covid_top_10
)

matrices <- list(covid_mat)
vectors <- list(vector_cols, countries)
data_structure_list <- list("dataframe" = datasets, "matrix" = matrices, "vector" = vectors)
covid_analysis_list <- list(question, answer, data_structure_list)
covid_analysis_list[2]
