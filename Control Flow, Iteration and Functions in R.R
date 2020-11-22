millimeter_measurements <- c(10000, 54900, 94312, 49185, 2100)
v_length <- integer(length(millimeter_measurements))
meter_measurements <- c()
indices <- 1:length(millimeter_measurements)

for (i in indices){
  meter_measurements[i] <- millimeter_measurements[i]/1000
}

meter_measurements

prices <- c(138.19, 82.72, 118.97, 77.55, 62.28, 89.71, 97.36, 91.98, 113.45, 73.93)
i <- 1
is_below_70 <- FALSE
while(!is_below_70){
  is_below_70 <- prices[i] < 70
  i = i+1
  print(prices[i])
}
buy_price <- prices[i-1]   
buy_price



first_vec <- c(1, 5, 4, 2, 3, 7, 6)
second_vec <- c(9, 2, 1, 8, 3, 4, 5, 6, 10, 7, 12, 11)
third_vec <- c(8, 3, 5, 1, 7, 1, 10)
find_longer_vector <- function(vec_one, vec_two) {
  
  if (length(vec_one) > length(vec_two)) {
    return("First")
  } else if (length(vec_one) < length(vec_two)) {
    return("Second")
  } else {
    return("Equal Length")
  }
}

first_vs_second <- find_longer_vector(first_vec, second_vec)
first_vs_third <- find_longer_vector(first_vec, third_vec)

is_divisible <- function(a,b){
  status <- if_else(a%%b==0, TRUE , FALSE)
  return(status)
}
div_5731_by_11 <- is_divisible(5731, 11)


subtract_all <- function(start, ...) {
  current_num <- start
  
  for (num in list(...)) { 
    current_num <- current_num - num
  }
  
  return(current_num)
}

first_subtraction <- subtract_all(10, 1, 2, 3)
second_subtraction <- subtract_all(100, 71, 22)
