# PROJECT R ~(Maryam, Oszkar, Fatima, Hassan)
# Price differential of products based on districts


rm(list = ls())
 

library(ggthemes)
library(modelsummary)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplR)
library(dplyr)
install.packages("pacman")
library(pacman)
install.packages("devtools")
library(devtools)

data <- read_csv( "https://raw.githubusercontent.com/maryamkhan1120/Coding-1-Hong-Kong/main/HongKong%20_Data.csv")



glimpse(data)

#cleaning data 
#1 altering col-name
colnames(data) <- c("ID","Store_name","Address","District","Price_coke","Price_orbit","Distance","Cashier")
data
#2 removing ID col
data$ID <- NULL
data
#3 factoring of data 
data$District <- factor(data$District)
is.factor(data$District)


# Skim of data
datasummary_skim(data)
datasummary(Price_coke + Price_orbit ~ Mean + Median + Max + Min , data= data)



# range function
range_df <- function( x )
{ 
  max( x , na.rm = T ) - min( x , na.rm = T ) 
}

?P100



# quartile fuction 
q5 <- function(x){
  quantile( x , c(0.05))
}

#data_summary
a <- datasummary(District * Price_coke  ~ Mean + Median + Max + Min + SD  + P25 + N + range_df + P75 + q5 , data= data )
a
b <- datasummary(District * Price_orbit~ Mean + Median + Max + Min + SD  + N + P25 + P75 + q5 + range_df , data= data)
b
c <- datasummary(District * Price_orbit + District * Price_coke ~ Mean + Median + Max + Min + SD  + N + P25 + P75 + q5 + range_df  , data= data)
c


data1 <- data %>% 
  select(District,Price_coke,Price_orbit) %>% 
  group_by(District) 
data1



# ggplot price coke 
ggplot (data = data1 , aes( x= Price_coke, fill= District)) + 
  geom_histogram(aes(y=..density..),color= "white",binwidth = 50) +
  geom_density(aes(y=..density..), color ="black",binwidth=50, alpha=0.5)+
  facet_grid(~District)+ 
  xlab("Coke Price") +
  ylab("Frequency of Price")+
  annotate( "text" , x = mean( data$Price_coke , na.rm = T ) + 54 , y = 0.013 , label = 'Mean' , color = 'purple')+
  geom_segment( aes(x = mean( `Price_coke` , na.rm = T ), y = 0, 
                    xend = mean( `Price_coke`, na.rm = T ) , yend = 0.015) , color = 'darkgreen', size = 1,
                arrow = arrow(length = unit(0.15, "cm")) )+
  scale_y_continuous(labels = scales::percent)+
  ggtitle ("Distribution of Coke (Binned with 50 HUF)")+
  labs(x='Price of Coke', y= "Kernel Density Percentage")+
  theme_gdocs()+
  theme(plot.title=element_text(size= 10,hjust=0.5))
  
  

  

#ggplot orbit 
ggplot (data = data1 , aes( x= Price_orbit, fill= District)) + 
  geom_histogram(aes(y=..density..),color= "white", binwidth = 30) +
  geom_density(aes(y=..density..), color ="black", binwidth=30, alpha=0.5)+
  facet_grid(~District)+ 
  xlab("Coke Price") +
  ylab("Frequency of Price")+
  annotate( "text" , x = mean( data$Price_orbit , na.rm = T ) + 54 , y = 0.013 , label = 'Mean' , color = 'purple')+
  geom_segment( aes(x = mean( `Price_orbit` , na.rm = T ), y = 0, 
                    xend = mean( `Price_orbit`, na.rm = T ) , yend = 0.015) , color = 'darkgreen', size = 1,
                arrow = arrow(length = unit(0.15, "cm")) )+
  scale_y_continuous(labels = scales::percent)+
  ggtitle ("Distribution of Orbit (Binned with 30 HUF)")+
  labs(x='Price of Orbit', y= "Kernel Density Percentage")+
  theme_gdocs()+
  theme(plot.title=element_text(size= 12,hjust=0.5))






