
# Reference
# https://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot


# Libraries
library(ggplot2)
library(ggthemes)
library(dplyr)


# Variables
partyColor <- c(BJP = "#B2B7C0",  Congress ="#D7D7D7")
personColor <- c(MD = "#EC111A", MA = "#00588D", GA = "#5DA4DF" )
titleColor <- "#EC111A"
displayResolution <- 600


# set working directory and read the file
setwd("/Users/sanjaynoronha/Desktop/R")
df <- read.csv("Fig3_India_Billiionaires.csv")

# print the file to see the contents
df

p <- ggplot(df, aes(x=year, y= worth)) +
  # position the text as appropriate
  geom_text(data = filter(df, name == "Gautam Adani"),aes(label = worth, color = code), vjust = 1.9, hjust = 1.5) +
  geom_text(data = filter(df, name != "Gautam Adani"),aes(label = worth, color = code), vjust = -1.0, hjust = 1.2) +
  
  #line and points
  geom_line(aes(color = code)) +
  geom_point(aes(color = code), size = 2.3) +
  
  # draw the background denoting the political party
  geom_rect(data = df, aes(xmin = year, xmax = year_max, fill = party), ymin =-Inf, ymax = Inf, alpha = 0.1) +
  xlim(2008, 2022) +
  
  # re-label the x axis
  scale_x_continuous(  breaks    =c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)
                       , labels = c(2008, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 2021, 2022)) +
  
  # background color to be manually configured but not present in the legend
  scale_fill_manual(values = partyColor,  guide = "none") + 
  
  # colors for people and labels to be renamed
  scale_colour_manual(values= personColor, labels = c(MD= "Median worth - Top 10 Indian billionaires",
  MA="Mukesh Ambani", GA="Gautam Adani"))+
  
  # annotation for the name of political party 
  annotate("text", x = 2008, y = 90, label ="Congress", hjust = 0, size = 6, fontface = "bold")+
  annotate("text", x = 2014, y = 90, label ="BJP", hjust = 0, size = 6, fontface = "bold")+
  
  #manually configuring the y axis range
  ylim(0, 100) +

  
  # this is our base theme
  theme_clean() +
  
  # customize the base theme
  labs(
    title = "Stairway to Heaven"
   , subtitle = "Yearly worth ($billion) - Mukesh Ambani, Gautam Adani and the median worth\nof India's top 10 billionaires"
    , caption = c("Instagram: @plotShaala", "Source: Forbes" )
  ) +
  
  theme(
    plot.caption = element_text(hjust=c(1, 0))
    , plot.title = element_text(colour = titleColor, size = 18)
    , plot.subtitle  = element_text(size = 12)
  ) +
  
  theme(
    panel.grid.major.x =   element_line( size = 0.1, color = "gray60")
    , panel.grid.major.y =  element_blank()
    , panel.grid.minor.x =  element_blank()
    , panel.grid.minor.y = element_blank()
  ) +
  
  theme(
    axis.ticks = element_blank()
  ) +
  
  theme(
    legend.title = element_blank()
    , legend.position= "bottom"
    , legend.background = element_blank()
  ) 



# save plot in high resolution
ggsave(plot = p,  width = 7.2, height = 6.4 ,dpi = displayResolution, filename = "Fig3_Billionaires_India.png")