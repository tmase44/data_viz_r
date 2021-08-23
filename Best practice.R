# BEST PRACTICES----

# Chapter 1: proportion data
# Chapter 2: point data
# Chapter 3: single distribution data
# Chapter 4: multiple distribution data

library(tidyverse)
library(waffle)
install.packages("Rttf2pt1")
who_disease <- read.csv("who_disease.csv")
head(who_disease)

# first looks
ggplot(who_disease, aes(region)) +
  geom_bar() # volume by region

# exploration, subset some data (AMR)

amr_region <- who_disease %>% 
  filter(region == 'AMR')
ggplot(amr_region,aes(year,cases,color=disease))+
  geom_point(alpha=0.5)

#PROPORTIONAL DATA----

#pie----
# good for simple representation of volume/share

  # mutate
who_disease %>% 
  mutate(region=ifelse(region%in%c('EUR','AFR'), region, 'Other')) %>%
  #then plot
  ggplot(aes(1,fill=region))+
  geom_bar(color="white")+
  coord_polar(theta="y")+
  theme_void()

# waffle----
  # similar to pie but more precise

# filter data
obs_by_region<-who_disease %>% 
  group_by(region) %>% 
  summarise(num_obs = n()) %>% 
  mutate(percent = round(num_obs/sum(num_obs)*100))

#make an array
percent_by_region<-obs_by_region$percent
names(percent_by_region)<-obs_by_region$region # names the array regions

# send array to the waffle
waffle::waffle(percent_by_region,rows=5)

#pie2----
#Wrangle data into form we want. 
disease_counts <- who_disease %>%
  mutate(disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other')) %>%
  group_by(disease) %>%
  summarise(total_cases = sum(cases))

ggplot(disease_counts, aes(x = 1, y = total_cases, fill = disease)) +
  geom_col()+
  coord_polar(theta="y")+
  theme_void()+ # without this, chart is messy
  ggtitle('Proportion of diseases')

#waffle2

disease_counts <- who_disease %>%
  group_by(disease) %>%
  summarise(total_cases = sum(cases)) %>% 
  mutate(percent = round(total_cases/sum(total_cases)*100))

# Create an array of rounded percentages for diseases.
case_counts <- disease_counts$percent
# Name the percentage array with disease_counts$disease
names(case_counts)<-disease_counts$disease

# Pass case_counts vector to the waffle function to plot
waffle::waffle(case_counts)

# pies only work with a single distribution. With multiple distribution..
  # bars are better

# stacked bar----

who_disease %>% 
  filter(region=='SEAR') %>% 
  ggplot(aes(countryCode,cases,fill=disease))+
           geom_col(position='fill') # fill ensures bars fill to 100%, othereise would be different heights
# BEST PRACTICE
  # use few classes (no more thn 3), and never use one stack in isolation
  # when comparing different wholes to each other

disease_counts <- who_disease %>%
  mutate(disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other')) %>%
  group_by(disease, year) %>% # note the addition of year to the grouping.
  summarise(total_cases = sum(cases))

# add the mapping of year to the x axis. 
ggplot(disease_counts, aes(year,total_cases, fill = disease)) +
  geom_col(position='fill')

# ordering the stack for readability

disease_counts <- who_disease %>%
  mutate(
    disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other') %>% 
      factor(levels = c('measles', 'other', 'mumps')) # change factor levels to desired ordering
  ) %>%
  group_by(disease, year) %>%
  summarise(total_cases = sum(cases)) 

# plot
ggplot(disease_counts, aes(year,total_cases, fill = disease)) +
  geom_col(position = 'fill')

# categorical x axis---
  # the above chart, there are no pre-1999 records for mumps, making earlier data meaningless

# Filter to on or later than 1999
disease_counts <- who_disease %>%
  filter(year>='1999') %>% 
  mutate(disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other')) %>%
  group_by(disease, region) %>%
  summarise(total_cases = sum(cases))

ggplot(disease_counts, aes(region,total_cases,fill=disease)) +
  geom_col(position='fill')




  