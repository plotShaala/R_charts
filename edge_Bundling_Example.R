#References : 
# https://r-graph-gallery.com/309-intro-to-hierarchical-edge-bundling.html
# https://r-graph-gallery.com/310-custom-hierarchical-edge-bundling.html

# to add labels to the nodes
# https://r-graph-gallery.com/311-add-labels-to-hierarchical-edge-bundling.html

# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)

displayResolution <- 600


#  Set seed if required
#set.seed(1234)

# set working directory and read the file
setwd("/Users/sanjaynoronha/Desktop/R/Github/R_charts")

# origin & level 1
d1 <- data.frame(from="origin", to=paste("group", seq(1,4), sep=""))

# level 2
d2 <- data.frame(from=rep(d1$to, each = 10), to=paste("subgroup", seq(1,40), sep="_"))
# combine all rows
hierarchy <- rbind(d1, d2)


# connection from one leaf to another
# along with the weight of the connection
all_leaves <- paste("subgroup", seq(1,40), sep="_")

connect <- rbind( 
  data.frame( from=sample(all_leaves, 200, replace=T) , to=sample(all_leaves, 200, replace=T))
  )

# am giving a weight to each connection
connect$value <- runif(nrow(connect), min = 0, max = 1)

# Create a vertices data.frame
# This data frame has meta data about the vertices such as 
# value of node, parent, id, angle etc
vertices  <-  data.frame(
  name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) 
  , value = runif(45)

) 

# Let's add a column with the group of each name. It will be useful later to color points
# Take each vertex and get the index of the corresponding row in the hierarchy$to row
# Then from that index, get the hierarchy$from value
vertices$group  <-  hierarchy$from[ match( vertices$name, hierarchy$to ) ]

# Next step: computing the label features that will be displayed all around the circle, next to the nodes:
# angle → vertical on top and botton, horizontal on the side, and so on.
# flip it → labels on the left hand side must be 180° flipped to be readable
# alignment → if labels are flipped, they must be right aligned

#Let's add information concerning the label we are going to add: angle, 
# horizontal adjustement and potential flip
#calculate the ANGLE of the labels
vertices$id <- NA


# match() :  returns a vector of the positions of (first) matches of its first argument in its second
# is.na() : if the value is NA the is.na() function return the value of true, otherwise, return to a value of false.
# which() : Give the TRUE indices of a logical object, allowing for array indices.

# get the index of the leaves in the vertices data frame
myleaves <- which(is.na( match(vertices$name, hierarchy$from) ))
nleaves <- length(myleaves)

# only number the leaves
# Note the good use of 'myleaves'
vertices$id[ myleaves] <- seq(1:nleaves)

vertices$angle <- 90 - 360 * vertices$id / nleaves

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust <- ifelse( vertices$angle < -90, 1, 0)

# flip angle BY to make them readable
vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)

# Create a graph object
# This is not a data viz
mygraph <- graph_from_data_frame( hierarchy, vertices=vertices )


# The connection object must refer to the ids of the leaves:
# get the indexes of the from and to objects from the vertices object
from  <-  match( connect$from, vertices$name)
to  <-  match( connect$to, vertices$name)

# Basic usual argument
# This is a data viz
p <- ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
  geom_node_point(aes(filter = leaf, colour=group, size=value, alpha=0.2)) + # the leaf only shows leaves
  #scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(0.1,10) ) +
  # the tension affects the lines i.e. straight or curved
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", tension = 0.4)  +
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name, angle = angle, hjust=hjust), size=2.5, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) 

# save plot in high resolution
ggsave(plot = p,  width = 8, height = 8 ,filename = "edge_Bundling_Example.png")
