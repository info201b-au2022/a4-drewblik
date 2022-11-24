library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(maps)
library(usdata)
library(mapproj)
library(patchwork)
library(stringr)
library(knitr)
library(lintr)

# The functions might be useful for A4
source("../source/a4-helpers.R")

incarceration_trends <- read_csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>

# Total population of WA in 2018
WA_2018_pop <- incarceration_trends %>%
  filter(year == max(year)) %>%
  filter(state == "WA", na.rm = TRUE) %>%
  pull(total_pop_15to64)
WA_2018_pop <- sum(WA_2018_pop)

# Total aapi population in WA in 2018
total_aapi_pop_WA <- incarceration_trends %>%
  filter(year == max(year)) %>%
  filter(state == "WA", na.rm = TRUE) %>%
  pull(aapi_pop_15to64)
total_aapi_pop_WA <- sum(total_aapi_pop_WA)

# Proportion of aapi people in WA in 2018
aapi_WA_prop_percent <- 100 * (total_aapi_pop_WA / WA_2018_pop)

# Total black population in WA in 2018
total_black_pop_WA <- incarceration_trends %>%
  filter(year == max(year)) %>%
  filter(state == "WA") %>%
  pull(black_pop_15to64)
total_black_pop_WA <- sum(total_black_pop_WA)

# Proportion of black people in WA in 2018
black_WA_prop_percent <- 100 * (total_black_pop_WA / WA_2018_pop)

# Total latinx population in WA in 2018
total_latinx_pop_WA <- incarceration_trends %>%
  filter(year == max(year)) %>%
  filter(state == "WA") %>%
  pull(latinx_pop_15to64)
total_latinx_pop_WA <- sum(total_latinx_pop_WA)

# Proportion of latinx people in WA in 2018
latinx_WA_prop_percent <- 100 * (total_latinx_pop_WA / WA_2018_pop)

# Total native population in WA in 2018
total_native_pop_WA <- incarceration_trends %>%
  filter(year == max(year)) %>%
  filter(state == "WA") %>%
  pull(native_pop_15to64)
total_native_pop_WA <- sum(total_native_pop_WA)

# Proportion of native people in WA in 2018
native_WA_prop_percent <- 100 * (total_native_pop_WA / WA_2018_pop)

# Total white population in WA in 2018
total_white_pop_WA <- incarceration_trends %>%
  filter(year == max(year)) %>%
  filter(state == "WA") %>%
  pull(white_pop_15to64)
total_white_pop_WA <- sum(total_white_pop_WA)

# Proportion of white people in WA in 2018
white_WA_prop_percent <- 100 * (total_white_pop_WA / WA_2018_pop)

# Total jail population in WA in 2018
WA_jail_2018 <- incarceration_trends %>%
  filter(year == max(year)) %>%
  filter(state == "WA") %>%
  pull(total_jail_pop)
WA_jail_2018 <- sum(WA_jail_2018)

# Total population of aapi people in jail in WA in 2018
total_aapi_jail_WA <- incarceration_trends %>%
  filter(year == max(year)) %>%
  filter(state == "WA") %>%
  pull(aapi_jail_pop)
total_aapi_jail_WA <- sum(total_aapi_jail_WA)

# Proportion of aapi people in jail in WA in 2018
aapi_WA_jail_prop_percent <- 100 * (total_aapi_jail_WA / WA_jail_2018)

# Total population of black people in jail in WA in 2018
total_black_jail_WA <- incarceration_trends %>%
  filter(year == max(year)) %>%
  filter(state == "WA") %>%
  pull(black_jail_pop)
total_black_jail_WA <- sum(total_black_jail_WA)

# Proportion of black people in jail in WA in 2018
black_WA_jail_prop_percent <- 100 * (total_black_jail_WA / WA_jail_2018)

# Total population of latinx people in jail in WA in 2018
total_latinx_jail_WA <- incarceration_trends %>%
  filter(year == max(year)) %>%
  filter(state == "WA") %>%
  pull(latinx_jail_pop)
total_latinx_jail_WA <- sum(total_latinx_jail_WA)

# Proportion of latinx people in jail in WA in 2018
latinx_WA_jail_prop_percent <- 100 * (total_latinx_jail_WA / WA_jail_2018)

# Total population of native people in jail in WA in 2018
total_native_jail_WA <- incarceration_trends %>%
  filter(year == max(year)) %>%
  filter(state == "WA") %>%
  pull(native_jail_pop)
total_native_jail_WA <- sum(total_native_jail_WA)

# Proportion of native people in jail in WA in 2018
native_WA_jail_prop_percent <- 100 * (total_native_jail_WA / WA_jail_2018)

# Total population of white people in jail in WA in 2018
total_white_jail_WA <- incarceration_trends %>%
  filter(year == max(year)) %>%
  filter(state == "WA") %>%
  pull(white_jail_pop)
total_white_jail_WA <- sum(total_white_jail_WA)

# Proportion of white people in jail in WA in 2018
white_WA_jail_prop_percent <- 100 * (total_white_jail_WA / WA_jail_2018)



#----------------------------------------------------------------------------#



## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
get_year_jail_pop <- function() {
  year_jail_pop <- incarceration_trends %>% 
    group_by(year) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
return(year_jail_pop)   
}

test <- get_year_jail_pop()

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  data <- get_year_jail_pop()
  ggplot(data) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    labs(
      title = "Increase in Jail Population in the U.S. (1970-2018)", # plot title
      x = "Year", # x-axis label
      y = "Total Jail Population", # y-axis label
    )
} 
plot_jail_pop_for_us()
#----------------------------------------------------------------------------#



## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

get_jail_pop_by_states <- function(states){
  temp <- incarceration_trends %>% 
    filter(state %in% states) %>%
    select(year, state, total_jail_pop) %>% 
    group_by(state, year) %>% 
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
return(temp)
}

plot_jail_pop_by_states <- function(states)  {
  data <- get_jail_pop_by_states(states)
  ggplot(data) +
    geom_line(mapping = aes(x = year, y = total_jail_pop, color = state)) +
    labs(
      title = "Jail Population by States (1970-2018)", # plot title
      x = "Year", # x-axis label
      y = "Total Jail Population", # y-axis label
    )
} 

plot_jail_pop_by_states(c("WA", "CA", "NY", "AL", "GA"))

#----------------------------------------------------------------------------#




## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas

# Question: How does population density affect the proportion of black people in prison?

get_density_proportion_data <- function(){
  
  density_proportion_data <- na.omit(incarceration_trends)
  
  
  
  density_proportion_data <- density_proportion_data %>% 
  mutate(pop_density = total_pop / land_area) %>% 
  mutate(prop_black_in_jail = black_jail_pop / black_pop_15to64) %>%
    
  filter(year == 2000) %>% 
  filter(pop_density < max(pop_density)) %>% 
  filter(pop_density < max(pop_density)) %>% 
  filter(pop_density < max(pop_density)) %>% 
  filter(prop_black_in_jail < max(prop_black_in_jail)) %>% 
  select(pop_density, prop_black_in_jail)
  
  
  
return(density_proportion_data)  
}

test3 <- get_density_proportion_data()


plot_density_proportion <- function(){
  density_data <- get_density_proportion_data()
  
  ggplot(data = density_data) +
  geom_point(mapping = aes(x = pop_density, y = prop_black_in_jail)) +
  geom_smooth(mapping = aes(x = pop_density, y = prop_black_in_jail)) +
    labs(
      title = "Proportion of Black People in Jail vs Population Density", # plot title
      x = "Population Density (# People / Sq. Miles)", # x-axis label
      y = "Proportion of Black People in Jail", # y-axis label
    )
}

plot_density_proportion()
#----------------------------------------------------------------------------#








## Section 6  ---- 
#----------------------------------------------------------------------------#
get_map_data <- function(){
  # Map: Black jail population over total jail population ratio each state in 1990
  incarceration_trends$black_prop <- (incarceration_trends$black_jail_pop / 
                                              incarceration_trends$black_pop_15to64)
  incarceration_trends$jail_prop_black <- (incarceration_trends$black_jail_pop / 
                                                   incarceration_trends$total_jail_pop)
  
  mod_data <- incarceration_trends %>%
    filter(year == 2000) %>%
    select(state, county_name, year, fips, total_jail_pop, black_pop_15to64, 
           black_jail_pop, black_prop, jail_prop_black) 
  
  state_shape <- map_data("state") %>%
    rename(state = region)
  state_shape <- state_shape %>%
    mutate(state = state2abbr(state))
  
  map_data <- state_shape %>%
    left_join(mod_data, by = "state")
return(map_data)
}

plot_black_prop_map <- function(){
  blank_theme <- theme_bw() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    )
  temp_map_data <- get_map_data()
ggplot(temp_map_data) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = jail_prop_black),
      color = "black", size = 0.3
    ) + 
    coord_map() + 
    scale_fill_continuous(limits = c(0, max(temp_map_data$jail_prop_black)), 
                          na.value = "grey", low = "white", high = "red") +
    blank_theme + 
  labs(
    title = "Proportion of Imprisioned Black People Over Total Jail Populations in 2000", # plot title
    x = "Year", # x-axis label
    legend = "Black Jail Proportion"
  )

}

plot_black_prop_map()
#----------------------------------------------------------------------------#

## Load data frame ---- 


