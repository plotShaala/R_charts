library(ggplot2)
library(ggthemes)
library(dplyr)

# Variable
countryColor <- c(China = "#02C39A", India = "#05648A") 
titleColor <- "#444444"
subTitleColor <- "#444444"  
labelFiller <- "#D4DC41"  
displayResolution <- 600


# set working directory and read the file
setwd("<details of file path>")
df <- read.csv("Fig1_WorldPollutedCities.csv")
df <- df %>% arrange(desc(ranking))


# This function causes the city to be ordered
# but this starts from the bottom of the axis
df$city  <- factor(
  df$city 
  ,levels =  df$city
)

df


# draw a horizontal lollipop chart
# Horizontal version
p <- ggplot(df, aes(x=value, y=city)) +
    geom_segment( aes(x=0, xend= value, y= city, yend= city, color= country) ) +
    geom_point( aes(color= country), size = 3.25) +
    annotate("label", x = 120 , y =  df$city,  label = df$value,  size = 2.5, fontface = "bold", fill = labelFiller, colour = "#444444") +
    scale_color_manual(values = countryColor) + 
  
theme_fivethirtyeight() +

# THEME SECTION
  
labs(
  x = NULL
  , y = NULL
  , title = "Top 15 of the world's most polluted cities"
  , subtitle = "2020, micrograms per cubic metre"
  , caption = c("Instagram: @plotShaala", "Source: www.iqair.com" )
) +
  
  theme(
    plot.caption = element_text(hjust=c(1, 0), colour = subTitleColor)
    , plot.title = element_text(colour = titleColor, hjust = 0.5)
    , plot.subtitle = element_text(colour = subTitleColor, hjust = 0.5)
  ) +
  
  theme(
    panel.grid.major.x =   element_blank()
    , panel.grid.major.y =  element_line(size = 0.2)
    , panel.grid.minor.x =  element_blank()
    , panel.grid.minor.y = element_blank()
  ) +
  
  theme(
    axis.ticks = element_blank()
    , axis.text.x = element_blank() 
    , axis.title.x=element_text(colour="#444444")
    , axis.title.y=element_text(colour="#444444")
  ) +
  
  theme(
    legend.title = element_blank()
    , legend.position="bottom"
    , legend.background = element_blank()
  ) 
  

ggsave(plot = p, width = 6.4, height = 5.6,  dpi = displayResolution, filename = "Fig1_WorldPollutedCities.png")
