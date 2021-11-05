### Team Assignment ###

## Clearing the environment ## 

rm(list = ls())

## Loading the packages ##

library(tidyverse)
library(modelsummary)
library(ggplot2)

## Loading the data ##

df <- read.csv(url("https://raw.githubusercontent.com/CheemaHaaris/Coding1/master/da1-datacollection.csv"))

## Inspecting the data ##

summary(df)

head(df)

# issues in addresses (Special character) - to be dealt with


## Checking for duplicates ## 

unique(df$id)

## Data Cleaning and Filtering

df <- df %>% select( !c( address ) )
df

# We dropped the address as we are interested in working at the district level rather than the address within the district.


## Changing var types to binary ##

df$sunday <- ifelse(df$sunday_open == 'Yes', 1, 0)
df$chain <- ifelse(df$chain_membership == 'Yes',1,0)  
  
df <- df %>% select( !c( sunday_open ) )
df <- df %>% select( !c( chain_membership ) )


# Problem: calculating operating hours  


## Descriptive Statistics ##

datasummary(df$p_coke + df$p_snickers ~ Mean + Median + SD + Min + Max + P25 + P75 + N, data = df )

datasummary_skim(df)  
  
  
## Distribution of main variables ## 

# Setting the theme #

theme_custom <- function( base_size = 11, base_family = "") {
  theme_bw() %+replace% 
    theme(
      
      plot.background   = element_rect(fill = "grey92", colour = "grey92") ,
      
      panel.grid.major  = element_line(color = "white"),
      
      panel.background  = element_rect(fill = "grey92", colour = "grey92"),
      
      panel.border      = element_rect(linetype = "solid", fill = NA), 
      
      axis.line         = element_line(color = "black", size = 1),
      
      axis.ticks        = element_line(color = "red4", size = 2),
      
      axis.text         = element_text(color = "black"),
      
      axis.ticks.length.y = unit(.25, "cm"),
      
      axis.ticks.length.x = unit(.25, "cm"),
      
      axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
      
    )
}

# Choice of graph - A histogram was used as we are analyzing the distribution of a single numeric variable

## Unconditioned distribution of price for each product ##

# Dist of price of coke

ggplot( df , aes( x = p_coke ) ) +
  geom_histogram( aes( y = ..density.. * 100 ) , fill = 'navyblue') +
    labs( title = 'Distribution of the price of coke' , x = 'Price of Coke (500ml)' , y = 'Relative Frequency (%) ') +
      theme_custom()

# Dist of price of snickers

ggplot( df , aes( x = p_snickers ) ) +
  geom_histogram( aes( y = ..density.. * 100 ) , fill = 'red') +
  labs( title = 'Distribution of the price of snickers' , x = 'Price of Snickers (50g)' , y = 'Relative Frequency (%) ') +
    theme_custom()


## Price Distribution conditioned on district ##

# Dist of price of coke

ggplot( df , aes( x = p_coke ) ) +
  geom_histogram( aes( y = ..density.. * 100 ) , fill = 'navyblue') +
  labs( title = 'Inner vs Outer district distribution of the price of coke' , x = 'Price of Coke (500ml)' , y = 'Relative Frequency (%) ') +
    facet_wrap(df$district) +
      theme_custom()

# Dist of price of snickers

ggplot( df , aes( x = p_snickers ) ) +
  geom_histogram( aes( y = ..density.. * 100 ) , fill = 'red') +
  labs( title = 'Inner vs Outer district distribution of the price of snickers' , x = 'Price of Snickers (50g)' , y = 'Relative Frequency (%) ') +
    facet_wrap(df$district) +
      theme_custom()



## Price Distribution conditioned on the number of cashiers ##

# Price of coke
ggplot( df , aes( x = p_coke ) ) +
  geom_histogram( aes( y = ..density.. * 100 ) , fill = 'navyblue') +
    labs( title = 'Price distribution of coke conditioned on the number of cashiers' , x = 'Price of Coke (500ml)' , y = 'Relative Frequency (%) ') +
      facet_wrap(df$no_cashier) +
        theme_custom()

# Price of snickers
ggplot( df , aes( x = p_snickers ) ) +
  geom_histogram( aes( y = ..density.. * 100 ) , fill = 'red') +
    labs( title = 'Price distribution of snickers conditioned on the number of cashiers' , x = 'Price of Snickers (50g)' , y = 'Relative Frequency (%) ') +
      facet_wrap(df$no_cashier) +
        theme_custom()

# *we do not have similar number of observations for each category - would not suggest using this


## Price Distribution conditioned on the membership of a chain ##

# Price of coke
ggplot( df , aes( x = p_coke ) ) +
  geom_histogram( aes( y = ..density.. * 100 ) , fill = 'navyblue') +
  labs( title = 'Price distribution of coke conditioned on membership of a chain' , x = 'Price of Coke (500ml)' , y = 'Relative Frequency (%) ') +
    facet_wrap(df$chain) +
      theme_custom()

# Price of snickers
ggplot( df , aes( x = p_snickers ) ) +
  geom_histogram( aes( y = ..density.. * 100 ) , fill = 'red') +
  labs( title = 'Price distribution of coke conditioned on membership of a chain' , x = 'Price of Snickers (50g)' , y = 'Relative Frequency (%) ') +
    facet_wrap(df$chain) + 
      theme_custom() 
        
## need help with adding a legend for the facet_wrap