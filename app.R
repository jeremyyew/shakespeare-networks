# setwd('~/Dropbox/AMTH160 Structure of Networks/Project')
source("ShakespeareVisnetwork-140419.R")
library(shiny)
library(visNetwork)
library(shinythemes)

playlist <- c("Much Ado About Nothing","The Tempest","Romeo and Juliet", "Hamlet", "Macbeth", "Merchant of Venice")
colors <- c("Reds", "Blues", "Purples", "Oranges", "Greens", "YlOrBr")
data <-vector("list", 6)
graphs <-vector("list", 6)

server <- function(input, output) {
  lapply(seq(length(playlist)), function(i) {
    print(playlist[i])
    # Process data
    data[[i]] <<- process_play(playlist[i], colors[i])
    # Draw
    graphs[[i]] <<- drawNetwork(playlist[i], data[[i]]$nodes, data[[i]]$edges, data[[i]]$stats, "layout_with_lgl", colors[i])
    
    output[[playlist[i]]] <- renderVisNetwork({
      return(graphs[[i]])
    })
    
    # SHOW MARRIAGES
    observe({
      if (input$showMarriages){
        data[[i]]$nodes$color <- data[[i]]$nodes$color_by_marriage
        data[[i]]$edges$color <- data[[i]]$edges$color_by_marriage
        data[[i]]$edges$value <- data[[i]]$edges$value_by_marriage
      } else {
        data[[i]]$nodes$color <- data[[i]]$nodes$color_by_lines
        data[[i]]$edges$color <- data[[i]]$edges$color_by_weight
        data[[i]]$edges$value <- data[[i]]$edges$value_by_interaction
      }
      visNetworkProxy(playlist[i]) %>% 
        visUpdateNodes(data[[i]]$nodes) %>%
        visUpdateEdges(data[[i]]$edges) 
    })
    
    # SHOW GENDER
    observe({
      if (input$showGender){
        data[[i]]$nodes$color <- data[[i]]$nodes$color_by_gender
        data[[i]]$edges$color <- data[[i]]$edges$color_by_gender
      } else {
        data[[i]]$nodes$color <- data[[i]]$nodes$color_by_lines
        data[[i]]$edges$color <- data[[i]]$edges$color_by_weight
      }
      visNetworkProxy(playlist[i]) %>%
        visUpdateNodes(data[[i]]$nodes) %>%
        visUpdateEdges(data[[i]]$edges)
    })
  })
}

ui <- fluidPage(
  theme=shinytheme("spacelab"),
  titlePanel("Shakespeare's Plays: Network visualization of interactions"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("showMarriages", "Show marriages", value = FALSE),
      checkboxInput("showGender", "Show gender", value = FALSE),
      div(
        p("Potential data to calculate:"),
        p("Total number of edges from female to female characters"),
        p("Total number of edges from male to male characters"),
        p("Average number of edges for male characters:"),
        p("Average number of edges for female characters:"),
        p("Average sum of edge weights for male characters:"),
        p("Average sum of edge weights for female characters:"),
        p("Average edge weight for male-female interactions:"),
        p("Average edge weight for male-male interactions:"),
        p("Average edge weight for female-female interactions:"),
        p("Average number of edges from female to female characters"),
        p("Average number of edges from female to male characters")
        )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(playlist[1], visNetworkOutput(playlist[1])),
        tabPanel(playlist[2], visNetworkOutput(playlist[2])),
        tabPanel(playlist[3], visNetworkOutput(playlist[3])),
        tabPanel(playlist[4], visNetworkOutput(playlist[4])),
        tabPanel(playlist[5], visNetworkOutput(playlist[5])),
        tabPanel(playlist[6], visNetworkOutput(playlist[6]))
      ),
      br(),
      br(),
      div(HTML(footerHtmlShiny), 
          style=boxStyleShiny)
      )
))


shinyApp(ui = ui, server = server)

