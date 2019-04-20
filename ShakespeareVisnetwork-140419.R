library("visNetwork") 
library(RColorBrewer)
library(igraph)
library(plyr)
library(dplyr)
library(tidyverse)

# Need to setwd programatically because the visSave function cant do relative paths otherwise
# Get color palettes
# display.brewer.all()
greys <- brewer.pal(8, "Greys")
greyPalette <- colorRampPalette(c(greys[2], greys[8]), alpha=TRUE)
set2 <- brewer.pal(8, "Set2")
set1 <- brewer.pal(8, "Set1")
M_COLOR <- set2[3]
F_COLOR <- set2[4]
U_COLOR <- greys[4]
gender_to_color <- data.frame(gender=c("M", "F", "U"), color=c(M_COLOR, F_COLOR,U_COLOR), stringsAsFactors = FALSE)

MARRIAGE_COLOR <- set1[1]
type_to_color <- data.frame(type=c("Marriage", "Interaction"), color=c(MARRIAGE_COLOR, U_COLOR),stringsAsFactors = FALSE)


# Get data
all_lines <- read.csv("data/Shakespeare_data.csv", na.strings=c(""," ","NA"), stringsAsFactors = FALSE)
player_atts_data <- read.csv("data/players.csv", na.strings=c(""," ","NA"), stringsAsFactors = FALSE)
player_atts_data <- subset(player_atts_data, select = -c(1))

# Check group sizes
# all_lines %>% group_by(Play) %>% group_size()

# Filter by play name to get lines only from 6 plays, and remove the "All" character chorus
six_plays <- all_lines[all_lines$Play %in% c("Much Ado about nothing", "The Tempest", "Romeo and Juliet", "Hamlet", "macbeth", "Merchant of Venice"), ] %>% filter(!(Player %in% c("ALL", "All")))

# Rename values so it looks cleaner
six_plays$Play <- mapvalues(six_plays$Play, from=c("Much Ado about nothing", "macbeth"), to=c("Much Ado About Nothing", "Macbeth"), warn_missing = TRUE)

# Check group sizes
# six_plays %>% group_by(Play) %>% group_size()

# Split ActSceneLine into three cols 
six_plays <- six_plays %>% drop_na(ActSceneLine) %>% separate(ActSceneLine, c("Act", "Scene", "Line"))
# View(six_plays)

# Group by Play/Act/Scene, distinct set of players in each scene. Remove scene 0 since that will usually only have one member. 
players_per_scene <- six_plays %>% group_by(Play, Act, Scene) %>% distinct(Player)


# Get lines per player, gender, etc to be mapped later
player_atts <- six_plays %>% count(Play, Player, name="lines")
player_atts <- merge(player_atts, player_atts_data, by.x=c("Play", "Player"), by.y=c("Play", "Player"), all.x=TRUE)

# Replace NA gender with "U"
player_atts$Gender[is.na(player_atts$Gender)] <- c("U")


add_attributes <- function(nodes, edges, player_atts){
  added <- character(nrow(nodes)) # Better to preassign.
  nodes$married <- "Interaction"
  for (i in seq(nrow(player_atts))){
    x <- player_atts[i,]$Player
    y <- player_atts[i,]$Married
    if (!is.na(y) && !(y %in% added)) {
      edges <- rbind(edges, data.frame(from=c(x), to=c(y), value=c(1), type="Marriage", stringsAsFactors = FALSE))
      added <- append(added, x)
      nodes[nodes$id==x | nodes$id==y, "married"] <- "Marriage"
    }
  }
  return(list(nodes=nodes, edges=edges))
}


process_play <- function(play, color){
  # Get color ramp palette
  palette <- brewer.pal(9, color)
  palette <-colorRampPalette(c(palette[2], palette[8]), alpha=TRUE)
  
  # Get player_atts and players_per_scene for this one play
  players_per_scene <- players_per_scene %>% filter(Play == play)
  player_atts <- player_atts %>% filter(Play==play)
  #View(one_play)
  # We need to constantly set strings as factors as false, otherwise we lose the string data
  edges <- data.frame(from=character(), to=character(), value=integer(), group=character(), stringsAsFactors = FALSE)
  compute_interactions <- function(grp, key) {
    # We need to leave out scenes with only 1 member because we can't generate interactions from that
    if (length(grp$Player)< 2) {
      return()
    }
    # Generate all pairs
    pairs <- combn(grp$Player, 2)
    # Increment pair value if they exist, else add new pair
    for (i in 1: ncol(pairs)){
      x <- pairs[1, i]
      y <- pairs[2, i]
      # We need to check whether it currently exists as (x,y) or (y,x) in edges
      a <- subset(edges, from==x & to==y)
      b <- subset(edges, from==y & to==x)
      if (length(rownames(a)) > 0 | length(rownames(b)) > 0) {
        if (length(rownames(a)) > 0) {
          b <- a
        }
        #Add new weight. 
        # Note we need to use <<- to ensure side effects are real.  
        edges[rownames(b)[1],"value"] <<- edges[rownames(b)[1],"value"] + 1
      } else {
        edges <<- rbind(edges, data.frame(from=c(x), to=c(y), value=c(1), type="Interaction", stringsAsFactors = FALSE))
      }
    }
  }
  
  # Add interaction edges
  players_per_scene %>% group_walk(compute_interactions)
  
  # Get unique players list. Construct from edges so we already leave out those with zero interactions. 
  players <- unique(c(edges$from, edges$to))
  View(players)
  
  # Create nodes df
  nodes <- data.frame(id=players, label=players)
  
  # Add marriage edges 
  data <- add_attributes(nodes, edges, player_atts)
  edges <- data$edges
  nodes <- data$nodes
  
  # Set node size by degree (number of connected members)
  g <-graph_from_data_frame(edges, directed = FALSE)
  degrees <- degree(g)
  nodes$value <- degrees[match(nodes$id, names(degrees))]
  
  # Set node color intensity by total number of lines in play
  # players_per_scene %>% filter(Play == play)
  nodes$lines <- player_atts$lines[match(nodes$id, player_atts$Player)]
  MAX_LINES= max(nodes$lines)
  nodes$color_by_lines <- palette(MAX_LINES)[nodes$lines]
  nodes$color <- nodes$color_by_lines
  
  # For each node, get total weight of all edges 
  total_weight <- strength(g, weights=edges$value)
  nodes$total_weight <- total_weight[match(nodes$id, names(total_weight))]
  # MAX_NODE_WEIGHT = max(nodes$total_weight)
  # nodes$color <- palette(MAX_NODE_WEIGHT)[nodes$total_weight]
  
  # Set group by gender
  nodes$gender <- player_atts$Gender[match(nodes$id, player_atts$Player)]
  
  # Get node color by gender
  nodes$color_by_gender <- gender_to_color$color[match(nodes$gender, gender_to_color$gender)]
  
  # Get node color by marriage
  nodes$color_by_marriage <- type_to_color$color[match(nodes$married, type_to_color$type)]
  
  
  # Set edge color and intensity by edge weight (value)
  MAX_EDGE_VALUE = max(edges$value)
  edges$color_by_weight <- palette(MAX_EDGE_VALUE)[edges$value]
  edges$color_by_gender <- greyPalette(MAX_EDGE_VALUE)[edges$value]
  edges$color_by_marriage <- type_to_color$color[match(edges$type, type_to_color$type)]
  
  edges$color <- edges$color_by_weight

  # Set edge value by type of interaction
  edges$value_by_interaction <- edges$value
  edges$value_by_marriage <- edges$value
  isMarried <- edges$type == "Marriage"
  edges$value_by_marriage[isMarried] <- 10
  
  
  NUM_PLAYERS <- nrow(nodes)
  NUM_EDGES <- nrow(edges)
  AVERAGE_NUM_EDGES <- mean(degrees)
  AVERAGE_EDGE_WEIGHT <- mean(edges$value)
  AVERAGE_NODE_WEIGHT <- mean(nodes$total_weight)
  # CONNECTIVITY? 
  
  stats <- list(NUM_PLAYERS=NUM_PLAYERS, NUM_EDGES=NUM_EDGES,AVERAGE_NUM_EDGES=AVERAGE_NUM_EDGES,AVERAGE_EDGE_WEIGHT=AVERAGE_EDGE_WEIGHT, AVERAGE_NODE_WEIGHT=AVERAGE_NODE_WEIGHT)

  # insert tooltip text for nodes and edges
  nodes$title <- paste("<b>", nodes$label, "</b>",
                       "<br><b>Number of Lines:</b>", nodes$lines,
                       "<br><b>Number of Edges:</b>", nodes$value, 
                       "<br><b>Total Edge Weight:</b>", nodes$total_weight
                       )
  edges$title <- paste("<b>", edges$from, " and ", edges$to, "<b>", 
                       "<br><b>Type:</b>", edges$type,
                       "<br><b>Weight:</b>", edges$value)
  
  return(list(nodes=nodes, edges=edges, stats=stats))
}


# Draw graph
drawNetwork <- function(play, nodes, edges, stats, layout, color) {
  palette <- brewer.pal(8, color)
  statText <- paste(paste("<b>", names(stats), "</b>", sep=""), format(stats, digits=2), sep = ": ", collapse = " | ")
  statFooter <- list(text=statText, style=statsStyle)
  g <- visNetwork(nodes, edges, 
                  width="100%",
                  footer=statFooter) %>% 
    visNodes(shadow = list(enabled = TRUE, 
                           size = 15),
             font=list(color=greys[7], face='Roboto'),
             scaling=list(max=60,
                          min=10,
                          label=list(enabled=TRUE,
                                     drawThreshold=10,
                                     maxVisible=35,
                                     max=35,
                                     min=10
                                     ))) %>% 
    visEdges(smooth=FALSE, 
             scaling=list(max=10, min=1),
             selectionWidth=0) %>%
    visIgraphLayout(layout=layout, randomSeed=1) %>%
    visOptions(highlightNearest=list(enabled=TRUE)) %>%
    visInteraction(navigationButtons=TRUE, 
                   tooltipDelay=150,
                   tooltipStyle=toolTipStyle)
   # visSave(g, paste(play, "-network.html", sep=""), selfcontained = TRUE, background = "white")
  return(g)
}

toolTipStyle <- "position: fixed; 
                                 padding: 10px; 
                                 white-space: nowrap; 
                                 font-family: Helvetica Neue,Helvetica,Arial,sans-serif;
                                 font-size: 14px; 
                                 background-color: rgb(255, 255, 255);                                                border-radius: 5px; 
                                 color: rgb(86, 86, 86); 
                                 box-shadow: rgba(0, 0, 0, 0.3) 5px 5px 20px;"

footerHtmlShiny <-paste("<b>Edge Weight:</b> Represents number of scenes two characters appear together in. <br>Mapped to edge size and color intensity.<br><br>",
                              "<b>Number of Lines:</b> Represents total number of lines in play. <br>Mapped to node color intensity.<br><br>",
                              "<b>Number of Edges:</b> Number of edges. Represents number of other characters that they interact with (i.e. appear in the same scene with at least once). <br>Mapped to node size.<br><br>",
                              "<b>Total Edge Weight:</b> Sum of weights of all edges. Represents total number of times they interact with other characters (i.e. number of times they appear in the same scene with each character).<br><br>", sep=""
)

statsStyle <- "    
    min-height: 20px;
    padding: 19px;
    margin-bottom: 20px !important;"


boxStyleShiny <- "    
    min-height: 20px;
    padding: 19px;
    margin-top: 40px;
    margin-bottom: 20px;
    background-color: #f5f5f5;
    border: 1px solid #e3e3e3;
    border-radius: 4px;"

