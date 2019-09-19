library(ggraph)
vignette("Layouts")
# Check also https://www.data-imaginist.com/2017/ggraph-introduction-layouts/
library(tidygraph)

set_graph_style(plot_margin = margin(1,1,1,1))
graph <- as_tbl_graph(highschool)
?highschool
highschool[highschool$from==26,]
highschool[highschool$from==32,]

# Not specifying the layout - defaults to "auto"
ggraph(graph) + 
  geom_edge_link(aes(colour = factor(year))) + 
  geom_node_point()


## trying my own nodes
foodweb <- data.frame(
  "predator" = c("Lamprey","Lamprey","Lamprey","AtSalmon","AtSalmon","LT",  "LT",  "LT",  "LT",     "Cisco","Cisco","Cisco","Cisco","Whitefish","Whitefish", "Trout Perch","ALW", "ALW", "ALW", "Smelt", "Smelt", "Smelt","Sculpin",           "Sculpin",  "MYS", "MYS", "ZOO"),
  "prey"     = c("Burbot", "LT",     "AtSalmon","ALW",     "Smelt",   "MYS","ALW", "Smelt","Sculpin","ALW","Smelt","ZOO","BcCp",   "BcCp","Benthic Invertebrates", "ZOO"  , "MYS", "ZOO", "BcCp", "MYS", "ZOO", "BcCp",    "Benthic Invertebrates", "MYS","PHY", "Detritus","PHY")
)

foodweb$value <- rep(1, nrow(foodweb))
foodweb$level <- rep(NA, nrow(foodweb))
for (i in 1:nrow(foodweb)) {
  if (foodweb$predator[i] %in% c("Detritus")) foodweb$level[i] = 0.5
  if (foodweb$predator[i] %in% c("PHY","Benthic Invertebrates")) foodweb$level[i] = 1
  if (foodweb$predator[i] %in% c("ZOO","MYS")) foodweb$level[i] = 2
  if (foodweb$predator[i] %in% c("BcCp")) foodweb$level[i] = 3
  if (foodweb$predator[i] %in% c("Sculpin","Trout Perch", "Alewife","Smelt")) foodweb$level[i] = 4
  if (foodweb$predator[i] %in% c("Burbot","AtSalmon", "LT","Cisco","Whitefish")) foodweb$level[i] = 5
  if (foodweb$predator[i] %in% c("Lamprey")) foodweb$level[i] = 6
}

graph <- as_tbl_graph(foodweb)
ggraph(graph) + 
  geom_edge_link() + 
  geom_node_point() +
  geom_node_label(aes(label = names(graph[1])), repel = TRUE)

# testing different layouts
igraph_layouts <- c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
                    'randomly', 'fr', 'kk', 'drl', 'lgl')
myplots <- list()
for (i in seq_along(igraph_layouts)) {
  #myplots[[i]] <-
    ggraph(graph, layout=igraph_layouts[i]) + 
    geom_edge_link() + 
    geom_node_point() +
    geom_node_label(aes(label = names(graph[1])), repel = TRUE) +
    ggtitle(paste0("Layout: ",igraph_layouts[i])) +
    theme_void() 
}
library(gridExtra)
do.call(grid.arrange, myplots)

# In conclusion:
# Once I'll have the outputs table from EwE, I'll be able to export the dataframe,
#   melt it with the package reshape, and compute this graph.

# Other potential package:
# ggnet2 https://briatte.github.io/ggnet/


library(tweenr)
library(igraph)
igraph_layouts <- c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
                    'randomly', 'fr', 'kk', 'drl', 'lgl')
igraph_layouts <- sample(igraph_layouts)
graph <- graph_from_data_frame(highschool)
V(graph)$degree <- degree(graph)
layouts <- lapply(igraph_layouts, create_layout, graph = graph)
layouts_tween <- tween_states(c(layouts, layouts[1]), tweenlength = 1, 
                              statelength = 1, ease = 'cubic-in-out', 
                              nframes = length(igraph_layouts) * 16 + 8)
title_transp <- tween_t(c(0, 1, 0, 0, 0), 16, 'cubic-in-out')[[1]]
for (i in seq_len(length(igraph_layouts) * 16)) {
  tmp_layout <- layouts_tween[layouts_tween$.frame == i, ]
  layout <- igraph_layouts[ceiling(i / 16)]
  title_alpha <- title_transp[i %% 16]
  p <- ggraph(graph, 'manual', node.position = tmp_layout) + 
    geom_edge_fan(aes(alpha = ..index.., colour = factor(year)), n = 15) +
    geom_node_point(aes(size = degree)) + 
    scale_edge_color_brewer(palette = 'Dark2') + 
    ggtitle(paste0('Layout: ', layout)) + 
    theme_void() + 
    theme(legend.position = 'none', 
          plot.title = element_text(colour = alpha('black', title_alpha)))
  plot(p)
  
}




# igraph
## A simple example with a couple of actors
## The typical case is that these tables are read in from files....
actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)

## The opposite operation
as_data_frame(g, what="vertices")
as_data_frame(g, what="edges")
