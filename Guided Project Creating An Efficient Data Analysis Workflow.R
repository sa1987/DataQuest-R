# In this guided project, we will be acting as a data analyst for a company that sells books for learning programming.
# Your company has produced multiple books, and each has received many reviews.
# Your company wants we to check out the sales data and see if we can extract any useful information from it. 
# We'll walk through this process as we progrses through thee mission.


# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# How big is the dataset?
#   Recall we can use the dim() function to see how many rows and columns are in a tibble.
# You should take a note of how many columns there are and how many rows there are.
# Write some notes to ourself to keep track of what we see in the dataset.
# What are the column names?
#   Recall we can use the colnames() function to return each of the column names of a tibble in a vector.
# What do each of the columns seem to represent?
#   What are the types of each of the columns? Sometimes you may find that data will look one way, but is actually disguised as another. As mentioned above, a common example of this is numbers that are actually strings in the data!
#   You can use the typeof() function to learn what the type is for each column.
# This would be a good application of a for loop. Since we have the column names from colnames(), we can loop through these and check the types of each of the column. Recall that you can access a column in a tibble with double bracket syntax tibble_data[["column_name"]] where tibble_data is a tibble and column_name is a string with the column name. Combining this with typeof() will give you back the type of the column itself.
# What are the unique values are present in each of the columns?
#   The If we're dealing with numbers, it's good to get a sense of how high and how low the values go. If we're dealing with strings, it's good to see all of the different possible values that are there.
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

library(tidyverse)

reviews <- read.csv("book_reviews.csv")
dim(reviews)
colnames(reviews)
for (c in colnames(reviews)){
  print(typeof(reviews[[c]]))
}

# str(reviews) -> this will also work . But more details

for (c in colnames(reviews)){
  print(paste0("Unique value in the Column- " , c ,":"))
  print(unique(reviews[[c]]))
}

#Remove missing data rows

new_reviews = reviews %>% 
  filter(!is.na(review))
     
dim=(new_reviews)

updated_reviews <- new_reviews %>% mutate(
  state = case_when(
  state == "TX" ~ "Texas",
  state == "FL" ~ "Florida",
  state == "NY" ~ "New York",
  state == "CA" ~ "California",
  TRUE ~ state
)

)

analyze_review <- updated_reviews %>% 
  mutate(
    review_num = case_when(
      review == "Poor" ~ 1,
      review == "Fair" ~ 2,
      review == "Good" ~ 3,
      review == "Great" ~ 4,
      review == "Excellent" ~ 5
    ),
    is_high_review = if_else(review_num >= 4, TRUE, FALSE)
  )
consolidated_data <- count(analyze_review, vars = book)
names(consolidated_data) <- c("Books", "Sales")
for (boo in analyze_review$book){
  if ((!(boo %in% consolidated_data[1]))) {
    consolidated_data$Price[consolidated_data$Books == boo] <- analyze_review$price[analyze_review$book == boo]
  }
  
}

#Calulate thr profit by calucating the income

calc_profit <- consolidated_data %>% mutate(
  Profit = (Sales * Price)
) %>% arrange(-Profit)