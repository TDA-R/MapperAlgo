library(shiny)
library(networkD3)
library(igraph)
library(ggplot2)
source('R/EdgeVertices.R')
source('R/ConvertLevelsets.R')
source('R/Cover.R')
source('R/Cluster.R')
source('R/SimplicialComplex.R')
source('R/MapperAlgo.R')

make_noisy_circle <- function(radius, num_points, noise_sd = 0.05) {
  theta <- runif(num_points, 0, 2 * pi)
  x <- radius * cos(theta) + rnorm(num_points, sd = noise_sd)
  y <- radius * sin(theta) + rnorm(num_points, sd = noise_sd)
  data.frame(x = x, y = y)
}

noisy_inner_circle <- make_noisy_circle(radius = 1, num_points = 1000)
noisy_outer_circle <- make_noisy_circle(radius = 2, num_points = 1000)

circle_data <- rbind(
  data.frame(circle = "inner", noisy_inner_circle),
  data.frame(circle = "outer",noisy_outer_circle)
)

ui <- fluidPage(
  titlePanel("Mapper Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "clustering_method",
        "Clustering Method:",
        choices = c("dbscan", "hierarchical", "kmeans", "pam"),
        selected = "dbscan"
      ),
      uiOutput("method_params_ui"),  # 動態參數 UI
      sliderInput("intervals", "Number of intervals:", min = 2, max = 10, value = 4),
      sliderInput("overlap", "Percent overlap:", min = 10, max = 90, value = 50, step = 5)
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("circlePlot")),
        column(6, forceNetworkOutput("mapperPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  # 動態生成方法參數的 UI
  output$method_params_ui <- renderUI({
    switch(input$clustering_method,
           "dbscan" = tagList(
             numericInput("eps", "DBSCAN eps:", value = 0.5, step = 0.1),
             numericInput("minPts", "DBSCAN minPts:", value = 5)
           ),
           "kmeans" = numericInput("num_clusters", "Number of Clusters (kmeans):", value = 2, min = 1),
           "pam" = numericInput("num_clusters", "Number of Clusters (PAM):", value = 2, min = 1),
           "hierarchical" = tagList(
             selectInput("hclust_method", "Linkage Method:", 
                         choices = c("ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), 
                         selected = "ward.D2"),
             numericInput("num_bins_when_clustering", "Number of bins to use when clustering.", value = 10, min = 1)
           )
    )
  })
  
  # Plot for circle_data
  output$circlePlot <- renderPlot({
    ggplot(circle_data) +
      geom_point(aes(x = x, y = y, color = circle)) +
      theme_minimal() +
      labs(title = "Circle Data", x = "X", y = "Y", color = "Group")
  })
  
  # Mapper Algorithm and Force Network
  output$mapperPlot <- renderForceNetwork({
    # 根據選擇的模型設置參數
    method_params <- switch(
      input$clustering_method,
      "dbscan" = list(eps = input$eps, minPts = input$minPts),
      "kmeans" = list(max_kmeans_clusters = input$num_clusters),
      "pam" = list(num_clusters = input$num_clusters),
      "hierarchical" = list(num_bins_when_clustering = input$num_bins_when_clustering, method = input$hclust_method),
      stop("Unknown clustering method")
    )
    
    time_taken <- system.time({
      Mapper <- MapperAlgo(
        filter_values = circle_data[, 2:3],
        intervals = input$intervals,
        percent_overlap = input$overlap,
        methods = input$clustering_method,
        method_params = method_params
      )
    })
    
    Graph <- graph.adjacency(Mapper$adjacency, mode = "undirected")
    l <- length(V(Graph))
    
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    # Majority vote for each vertex
    var.maj.vertex <- c()
    circle_groups <- as.character(circle_data$circle)
    for (i in 1:l) {
      points.in.vertex <- Mapper$points_in_vertex[[i]]
      Mode.in.vertex <- Mode(circle_groups[points.in.vertex])
      var.maj.vertex <- c(var.maj.vertex, as.character(Mode.in.vertex))
    }
    
    # Node data
    vertex.size <- sapply(1:l, function(i) length(Mapper$points_in_vertex[[i]]))
    MapperNodes <- mapperVertices(Mapper, 1:nrow(circle_data))
    MapperNodes$Group <- as.factor(var.maj.vertex)
    MapperNodes$Nodesize <- vertex.size
    
    # Edge data
    MapperLinks <- mapperEdges(Mapper)
    
    # Generate Force Network
    forceNetwork(
      Nodes = MapperNodes,
      Links = MapperLinks,
      Source = "Linksource",
      Target = "Linktarget",
      Value = "Linkvalue",
      NodeID = "Nodename",
      Nodesize = "Nodesize",
      Group = "Group",
      opacity = 1,
      zoom = TRUE,
      radiusCalculation = JS("Math.sqrt(d.nodesize)"),
      colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
      linkDistance = 30,
      charge = -10,
      legend = TRUE
    )
  })
}

shinyApp(ui, server)
