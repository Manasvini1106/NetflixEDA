## LOAD LIBRARIES
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)

## LOAD DATASET

initial <- read_csv("/Users/manasvininittala/Downloads/netflix_titles.csv")
colnames(initial)
str(initial)

## CHECK NULLS
any_nulls <- any(is.na(initial))
print(any_nulls)
nulls_per_column <- colSums(is.na(initial))
print(nulls_per_column)

## CHECK DUPLICATES
num_duplicates <- initial %>%
  duplicated() %>%
  sum()
print(num_duplicates)

## There are no duplicate values and the null values are the most in director,cast and country. We will not be using director and cast for EDA so we can drop them.
stage_one = select(initial,-director,-cast)
colnames(stage_one)
nulls_per_column = colSums(is.na(stage_one))
print(nulls_per_column)

## Next in the country column we have 831 nulls. Let us see the frequency of countries.
freq_table <- stage_one %>%
  group_by(country) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))
print(freq_table)

## United States has the most frequency. We will populate the nulls with Unites States
stage_two = stage_one %>%
  mutate(country = replace(country, is.na(country), "United States"))

freq_table <- stage_two %>%
  group_by(country) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))
print(freq_table)

nulls_per_column = colSums(is.na(stage_two))
print(nulls_per_column)

## Date added doesnt contribute much to our EDA. So we can drop it
stage_three = select(stage_two,-date_added)
colnames(stage_three)

nulls_per_column = colSums(is.na(stage_three))
print(nulls_per_column)

freq_table <- stage_three %>%
  group_by(rating) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))
print(freq_table)

## In the frequency table we can see that 66 min, 74 min and 84 min are included in the rating. They should be the missing 3 null values in duration. Let's filter and check it out.
desired_ratings <- c("66 min", "74 min", "84 min")
filtered_data <- filter(stage_three, rating %in% desired_ratings)
print(filtered_data)

stage_four <- stage_three %>%
  mutate(duration = ifelse(is.na(duration), rating, duration))

stage_four <- stage_four %>%
  mutate(rating = replace(rating, rating %in% desired_ratings, NA))

nulls_per_column <- colSums(is.na(stage_four))
print(nulls_per_column)

## So now we only have nulls in the rating section, lets populate the nulls with the most frequent rating
stage_five = stage_four %>%
  mutate(rating = replace(rating, is.na(rating), "TV-MA"))

freq_table <- stage_five %>%
  group_by(rating) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))
print(freq_table)

nulls_per_column <- colSums(is.na(stage_five))
print(nulls_per_column)

## Now that we have removed nulls and duplicates, let us drop columns which are not useful. The description column is not useful.
stage_six = select(stage_five,-description)
colnames(stage_six)
nulls_per_column <- colSums(is.na(stage_six))
print(nulls_per_column)

summary(stage_six)  # Provides summary statistics for numerical variables
str(stage_six)      # Shows data frame structure, including data types


## So now my data is clean and available for further analysis

final_data <- stage_six
summary(final_data)
write.csv(final_data, file = "final_netflix_data.csv")

## Let's start Exploratory Data Analysis
head(final_data)

## Plot for types
data_count = final_data %>% count(type)

ggplot(data_count, mapping = aes(x = type, y = n, fill = type)) +
  geom_bar(stat = "identity") + 
  labs(title = "Count of Types", x = "Type", y = "Count") + 
  theme_classic()  

## Plot for ratings
data_counts <- final_data %>% count(rating)

ggplot(data_counts, aes(x = rating, y = n, fill = rating)) +
  geom_bar(stat = "identity") +  
  labs(title = "Count of Ratings", x = "Rating", y = "Frequency") +
  theme(axis.text.x = element_text(angle=30))

## Plot for movies by year
data_counts <- final_data %>% count(release_year) 

ggplot(data_counts, aes(x = release_year, y = n,fill = release_year)) +
  geom_bar(stat = "identity") +  
  labs(title = "Count of Movies by Release Year", x = "Release Year", y = "Frequency") +  # Set plot labels
  theme(axis.text.x = element_text(angle=30))

## Plot for type and rating
ggplot(final_data, aes(x = rating, y = type,color = type)) +
  geom_point() +  
  labs(title = "Rating vs. Type", x = "Rating", y = "Type") +
  theme(axis.text.x = element_text(angle=30))

## Count of occurrences for each rating, grouped by type
data_counts <- final_data %>%
  group_by(type) %>%
  count(rating)  

## Create count plot with hue
ggplot(data_counts, aes(x = rating, y = n, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "Count of Ratings by Type", x = "Rating", y = "Frequency") +  
  theme_classic()   






