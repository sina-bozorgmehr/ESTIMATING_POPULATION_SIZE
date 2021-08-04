library(tidyverse)
library(rio)      # to import the excel file
library(modelr)   # to create the linear regression model


# the url link to the Census Estimates data
url <- "https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx"


df <- import(url, skip = 3, ) %>%     # import the data into a tibble and skipping the first three lines
  as_tibble() %>%
  filter(...1 == ".California") %>%   # excluding all the rows except the one for California
  select(!c(2,3)) %>%                 # selecting all columns but the 2nd and 3rd columns
  t() %>%                             # transposing the tibble
  print()

df %<>%
  as.data.frame() %>%                 # converting the tibble to data frame
  add_rownames(var = "Year") %>%      # creating a new column out of row names
  print()

df <- df[-1,]                         # removing the first row to get rid of the original column names

names(df)[2] <- "Population"          # renaming the 2nd column to Population


# Converting columns' values to integers
df$Year <- as.integer(df$Year)
df$Population <- as.integer(df$Population)


# creating a linear regression model
model <- df %>%
  select(
    Population,
    Year) %>%
  lm()
#Tabulating the statistical description of the linear regression model
summary(model)

# creating a grid for ggplot
grid <- df %>%
  data_grid(Year) %>%
  add_predictions(model)

# visualization
ggplot(df, aes(Year)) +
  geom_point(aes(y = Population), size = 4) +
  geom_line(aes(y = pred), data = grid, color = "red", size = 2) +
  scale_x_continuous(name = "Year", breaks = seq(2010,2019,1)) +
  scale_y_continuous(breaks = seq(36000000,40000000, 500000))
  

# predicting the state population in the next 5 years
prediction <- predict(model, list(Year = c(2020, 2021, 2022, 2023, 2024)))
names(prediction) <- c(2020, 2021, 2022, 2023, 2024)
prediction



