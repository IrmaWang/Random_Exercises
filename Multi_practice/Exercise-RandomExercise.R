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

# practice using rowwise function before mutate

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


######################################################################