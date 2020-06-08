## Created: Thu Jun 04 16:10:29 2020
## Updated:
## Purpose of script: 
## To help Irma practice whatever short codes
##
## -----------------------
## Notes:
## This is primarily for tidyverse: dplyr, stringr and regular expression
##
##
## -----------------------
## Objects read:
## 
## Object written: Irma Wang
## 
## ----------------------

library(tidyverse)
library(ggplot2)

install.packages("styler")
install.packages("lintr")


# SorensonDataTeamTraining20200604 ----------------------------------------
# Sreeja's code of using case_when function

df <- iris %>% 
  janitor::clean_names() %>% 
  filter(species == "setosa") %>% 
  mutate(Petal_length_desc = case_when(petal_length >= 1.6 ~ "long",
                                       petal_length > 1.3 & petal_length < 1.6 ~ "medium", 
                                       TRUE ~ "short"
  ))

# case_when is tidy compare to ifelse function

mynewvector <- case_when(
  condition1 ~ value1,
  condition2 ~ value2,
  condition3 ~ value3,
  TRUE ~ valueForEverythingElse
)

# possible example with census data

mydata <- mutate(
  division = case_when(
    State %in% Northeast ~ "Northeast",
    State %in% Midwest ~ "Midwest",
    State %in% South ~ "South",
    State %in% West ~ "West",
    TRUE ~ "Other"
  )
)

# practice using rowwise function before mutate-------------------------

iris %>% rowwise() %>%
  janitor::clean_names() %>% 
  mutate(sepal_area = sepal_length * sepal_width)

# learn purrr::map functions
map(.x, .y, ...)
# above code means apply .x(variables) to .y(function). '...' in case 
# other inputs needs to be given to .y 

# some example of how map vectorize data frames, create a list contains 2 sets of lists
df_list <- list(iris = head(iris, n = 2),
                mtcars = head(mtcars, n = 3))
df_list
# nrow tells me NULL because it dosen't work for 2 dfs
nrow(df_list)

# use purrr map
df_list %>% map(nrow)

# change the output to a df
df_list %>% map_int(nrow)

# learn pmap: for every tuple in the .l(where l means list of lists,
# tuple is formed from each element from differnt lists), apply .f
pmap(.l, .f, ...)
pmap(df, .f)

## practice pmap

iris %>% 
  group_by(Species) %>% 
  summarize(pl_avg = mean(Petal.Length, pw_avg = mean(Petal.Width)))
iris %>% 
  group_by(Species) %>% 
  summarize(pl_qul = quantile(Petal.Length, c(0.25, 0.5, 0.75)))

# change quantile to list
iris %>% 
  group_by(Species) %>% 
  summarize(pl_qul = list(quantile(Petal.Length, c(0.25, 0.5, 0.75))))

# apply map
iris %>% 
  group_by(Species) %>% 
  summarize(pl_qul = list(quantile(Petal.Length, c(0.25, 0.5, 0.75)))) %>%
  mutate(pl_qul = map(pl_qul, enframe, name = "quantile")) %>% 
  unnest() %>% 
  mutate(quantile = factor(quantile))

iris %>% 
  group_by(Species) %>% 
  summarize(pl_qul = list(quantile(Petal.Length, c(0.25, 0.5, 0.75)))) %>%
  mutate(pl_qul = map(pl_qul, enframe, name = "quantile")) %>% 
  unnest(cols = c(pl_qul))

# stringr practice----------------------------------------------------
strings <- c(
  "310 733 8965",
  "310-293-8753",
  "Work: 310-499-7527; Home: 310.355.3678/310-880-4789",
  "3109185662"
)
 
phone <- "([2-9][0-9]{2})[. -]*([0-9]{3})[. -]*([0-9]{4})"

# find the pattern in strings
str_locate(strings, phone)
str_locate_all(strings, phone)

# extract pattern from strings
str_extract(strings, phone)
str_extract_all(strings, phone)
str_extract_all(strings, phone, simplify = TRUE)

# match pattern from strings separtes pattern
str_match(strings, phone)

# replace strings with other characters
str_replace(strings, phone, "xxx-xxx-xxxx")
str_replace_all(strings, phone, "xxx-xxx-xxxx")

# replace strings with other number
str_replace(strings, "([2-9][0-9]{2})[. -]*([0-9]{3})","444-555")

# practice with emails

email <- c(
  "aaron@ucla.edu",
  "pwang@ucla.edu",
  "aha@gmail.com",
  "lin11@hotmail.com",
  "Aaron@ucla.edu",
  "kk_tt@yahoo.com"
)

pattern <- "@"

# I want to separate emails by name and domain
str_split(email, pattern, simplify = TRUE)

# I want to look for things in those email string
# such as "aaron"

str_detect(email, "aaron")
email[str_detect(str_to_lower(email),"aaron")]

# use pipe to find
email %>% str_to_lower() %>% 
  str_view("aaron") 

# look for numbers in string

str_detect(email,"[:digit:]{2}")
email[str_detect(email,"[:digit:]{2}")]

email %>% str_view("[:digit:]{2}") 

# look for special character in string
email[email %>% str_split("@", simplify = T) %>%
          `colnames<-`(c("user_name", "domain")) %>% 
          as_tibble() %>% 
          pull(user_name) %>% 
          str_detect("[:punct:]")] 

email[email %>% str_split("@", simplify = T) %>%
        `colnames<-`(c("user_name", "domain")) %>% 
        as.data.frame() %>% 
        pull(user_name) %>% 
        str_detect("[:punct:]")] 
# pull function takes out vectotized column from a df or table

email[email %>% str_split("@", simplify = T) %>% 
  `colnames<-`  (c("user_name","domain")) %>% 
  as.data.frame() %>% 
  .$user_name %>% 
  str_detect("[:punct:]")]

# str_glue function, {} calculates the data

str_glue("this {mean(cars$speed)} ", .sep = ":", " is the mpg mean")

lll <- mtcars %>% rownames_to_column("car_brand") 
str_glue_data(lll, "The car model {lll$car_brand} has {lll$hp} hp")

# I could also change rowname to a column and pipe everything into one pipe
mtcars %>% rownames_to_column("car_brand") %>% 
  str_glue_data("The car model {car_brand} has {hp} hp")

# or, I can use rowname function direction
mtcars %>% str_glue_data("{rownames(.)} has {hp} hp")



######################################################################
