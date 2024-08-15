# nested loop example ----
file_prefix <- c("temp", "ph", "salinity")
file_suffix <- c(1, 2, 3, 4, 5)

for (i in 1:length(file_prefix)){
  for (j in 1:length(file_suffix)){

print(paste0(file_prefix[i], "_", file_suffix[j]))   
  }
}
 
# another nested loop example ----
odds <- c(1, 3, 5)
evens <- c(2, 4, 6, 8)

for (i in seq_along(odds)) {
  for (j in seq_along(evens)){
    print(odds[i] * evens[j])
  }
}

# a simple function ----
birddog_sum <- function(bird, dog) {
  pets <- bird + dog
  return(pets)
}

x <- birddog_sum(bird = 2, dog = 5)


double_it <- function(x) {
  print(2 * x)
}

double_it(4)
double_it(1:4)

exclaim_age <- function(age) {
  print(paste("I am", age, "years old!"))
}

exclaim_age(age = 10)

find_max <- function(val1, val2) {
  if(val1 >val2) {
    return(val1)
  } else if (val2 > val1) {
    return(val2)
  }
}

find_max(7, 3)

# code warm up ----
# in progress
quarter_splits <- c(1.0, 1.1, 1.2, 1.1, 1.4, 1.5, 1.6, 1.4)
splits <- c("1st", "2nd", "3rd", "4th", "5th", "6th")
my_splits <- splits 
%>% dplyr::across(splits)

for (i in seq_along(quarter_splits)) {
  two_splits <- quarter_splits[i] + quarter_splits [i +1]
  } for (j in seq_along(splits)) {
  print(paste0("This is my" ,  [j], "split" , two_splits))
}

# a function with conditions ----

animal_age <- function(animal, age) {
  
  if(!animal %in% c("dog", "goat")) {
    stop("Oops! animal must be a dog or a goat")
  }
  if (is.numeric(age) == FALSE) {
    stop("The age must be a number greater than 0")
  }
  if (age <= 0){
    stop ("Age must be greater than 0")
  }  
  if (animal == "dog") {
    print(age * 7)
  } else if (animal == "goat") {
    print(age * 4.7)
  }
}

animal_age("dog", 8)
animal_age("cow", 2)
animal_age("dog", "yellow")

dog_choice <- data.frame(dog_name = c("Khora", "Teddy", "Waffle", "Banjo"),
                         food = c("everything", "salmon"                          , "pancakces", "chicken"))
library(tidyverse)

dog_menu <- function(name) {
  my_sub <- dog_choice %>%
    dplyr::filter(dog_name == name)
  
  print(paste("My name is", my_sub$dog_name, "and I like to eat", my_sub$food))
}

dog_menu("Khora")
dog_menu("Waffle")
dog_menu("Banjo")
dog_menu("Teddy")


# warning and error messages
calc_windpower <- function(rho, radius, windspeed) {
  
  if (windspeed > 130) {
    warning("wow, that's fast! are you sure?")
  }
  
  if (rho > 1.225) {
    warning("that air density is suspicious. are you sure?")
  }
  if (radius <0) {
    stop("rotor radius must be a positive value (meters)")
  }
  print(0.3 * rho * pi * (radius^2) * (windspeed^3))
}

calc_windpower(rho = 2, radius = -1, windspeed = 50)
