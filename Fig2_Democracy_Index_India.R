
# Libraries
library(ggplot2)
library(ggthemes)
library(dplyr)

# Variables
partyColor <- c(Congress = "#D7D9CE",  BJP = "#119DA4")
titleColor <- "#444444"
footerColor <- "#888888" 
displayResolution <- 600
lineColor <- "#0C7489"
pointColor <- "#040404" 


# set working directory and read the file
setwd("/Users/sanjaynoronha/Desktop/R/Github/R_charts")
df <- read.csv("Fig2_Democracy_Index_India.csv") 

# print the file to see the contents
df

# ggplot the data
p <- ggplot(df, aes(x=year, y= rank)) +
  geom_rect(data = df, aes(xmin = year, xmax = year_end, fill = party), ymin =-Inf, ymax = Inf, alpha = 0.2) +
  geom_text(aes(label = rank), vjust = - 1.6) +
  geom_line(colour = lineColor) +
  geom_point(size = 2.75, colour = pointColor) +
  scale_y_reverse(limits = c(60, 20)) + #Ref: https://r-graphics.org/recipe-axes-reverse
  scale_x_continuous(  breaks    =c(2006, 2008, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
                       , labels = c(2006, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 2021)) +
  scale_fill_manual(values = partyColor) + 
  
  # this is our base theme
  theme_fivethirtyeight() +
  
  # customize the base theme
  labs(
    title = "India, democracy index rank over the years"
    , subtitle = "India's rank among 167 countries. Data unavailable in 2007, 9"
    , caption = c("Instagram: @plotShaala", "Source: EIU" )
  ) +
  
  theme(
    plot.caption = element_text(hjust=c(1, 0), colour = footerColor)
    , plot.title = element_text(colour = titleColor, hjust = 0.5)
    , plot.subtitle = element_text(colour = titleColor, hjust = 0.5)
  ) +
  
  theme(
    panel.grid.major.y =  element_blank()
    , panel.grid.major.x =  element_line(size = 0.15)
    , panel.grid.minor.x =  element_blank()
    , panel.grid.minor.y = element_blank()
  ) +
  
  theme(
    axis.ticks = element_blank()
  ) +
  
  theme(
    legend.title = element_blank()
    , legend.position="bottom"
    , legend.background = element_blank()
  ) +
  
  theme(axis.title = element_text(size = 10)) + ylab('rank') + xlab('year')



# save plot in high resolution
ggsave(plot = p,  width = 6.0, height = 5.2 ,dpi = displayResolution, filename = "Fig2_Democracy_Index_India.png")




