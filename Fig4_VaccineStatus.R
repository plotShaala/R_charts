library(ggplot2)
library(ggthemes)
library(dplyr)

# Arrow reference : https://www.r-graph-gallery.com/233-add-annotations-on-ggplot2-chart.html

# Variables
typeColor <- c(State = "#05648A", "Union Territory" = "#02C39A") 
titleColor <- "#444444"
footerColor <- "#888888"  
labelFiller <- "#D4DC41"  
displayResolution <- 600


# set working directory and read the file
setwd("/Users/sanjaynoronha/Desktop/R/Github/R_charts")
df <- read.csv("Fig4_VaccineStatus.csv")
df <- df %>% arrange(desc(Percent))


# This function causes the State to be ordered
# but this starts from the bottom of the axis
df$State  <- factor(
  df$State 
  ,levels =  df$State
)

# draw a horizontal lollipop chart
# Horizontal version
p <- ggplot(df, aes(x=Percent, y=State)) +
  geom_segment( aes(x=0, xend= Percent, y= State, yend= State, color= Type), size = 0.3) +
  geom_point( aes(color= Type, size = Total)) +
  geom_text(aes(label = paste(ceiling(Percent), "%", sep = '')),  hjust = - 0.3, size = 5) +
  annotate("label", x = 105 , y =  df$State,  label = paste(round(df$Total/1000000,2), 'm', sep = ''),  size = 4,fontface = "bold", fill = labelFiller, colour = "#444444") +
  scale_colour_manual(values= typeColor) +
  scale_x_continuous(position = "top", limits = c(0,120))  +
  guides(size = "none")  +      #To turn off size legend
  annotate("segment", x = 90, xend = 95, y = "Manipur", yend = "Nagaland", colour = titleColor, size=2, alpha=0.6, arrow=arrow()) +
  geom_label( aes( x=82, y="Puducherry", label="Total\nvaccinations"), fill = labelFiller ,fontface = "bold", color= titleColor, size=5 , angle=45 ) +
  
  
# plotShaala theme
theme_fivethirtyeight() +

# customize the base theme
labs(
  title = "India COVID vaccination status"
  , subtitle = "States and Union Territories as of 6 MAR 2022"
  , caption = c("Instagram: @plotShaala", "Source: COWIN, Vaccinate India" )
) +
  
  
  theme(
    plot.caption = element_text(hjust=c(1, 0), colour = footerColor, size = 16)
    , plot.title = element_text(colour = titleColor, hjust = 0.6, size = 20)
    , plot.subtitle = element_text(colour = titleColor, hjust = 0.5, size = 17, face = "bold")
  ) +
  
  theme(
    panel.grid.major.y =  element_line(size = 0.15)
    , panel.grid.major.x =  element_blank()
    , panel.grid.minor.x =  element_blank()
    , panel.grid.minor.y = element_blank()
  ) +
  
  theme(
    axis.ticks = element_blank()
    , axis.text.y = element_text(size = 14)
    , axis.text.x = element_blank()
  ) +
  
  theme(
    legend.title = element_blank()
    , legend.text = element_text(size = 18)
    , legend.position= "bottom"
    
    , legend.background = element_blank()
  ) +
  
  theme(axis.title.x = element_text(size = 16)) + 
  xlab('% of population fully vaccinated (received both doses)') + 
  ylab(NULL) 


# save plot in high resolution
ggsave(plot = p, width = 10,  height = 11 ,units = "in",  dpi = displayResolution, filename = "Fig4_VaccineStatus.png")
  


