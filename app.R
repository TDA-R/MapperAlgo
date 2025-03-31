library(shiny)
library(networkD3)
library(igraph)
library(ggplot2)

library(parallel)
library(foreach)
library(doParallel)

source('R/EdgeVertices.R')
source('R/ConvertLevelsets.R')
source('R/Cover.R')
source('R/Cluster.R')
source('R/SimplicialComplex.R')
source('R/MapperAlgo.R')
source('R/Plotter.R')

make_noisy_circle <- function(radius, num_points, noise_sd = 0.05) {
  theta <- runif(num_points, 0, 2 * pi)
  x <- radius * cos(theta) + rnorm(num_points, sd = noise_sd)
  y <- radius * sin(theta) + rnorm(num_points, sd = noise_sd)
  data.frame(x = x, y = y)
}

# Generate noisy circle data
noisy_inner_circle <- make_noisy_circle(radius = 1, num_points = 1000)
noisy_outer_circle <- make_noisy_circle(radius = 2, num_points = 1000)

circle_data <- rbind(
  data.frame(dataset = "circle", circle = "inner", noisy_inner_circle),
  data.frame(dataset = "circle", circle = "outer", noisy_outer_circle)
)

# Load Iris dataset
iris_data <- iris
iris_data$dataset <- "iris"

# UI
ui <- fluidPage(
  titlePanel("Mapper Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "data_choice",
        "Choose Dataset:",
        choices = c("Circle Data" = "circle", "Iris Data" = "iris"),
        selected = "circle"
      ),
      selectInput(
        "clustering_method",
        "Clustering Method:",
        choices = c("dbscan", "hierarchical", "kmeans", "pam"),
        selected = "dbscan"
      ),
      selectInput(
        "cover_type",
        "Cover Type:",
        choices = c("stride", "extension"),
        selected = "extension"
      ),
      uiOutput("method_params_ui"),  # dynamic parameter UI
      sliderInput("intervals", "Number of intervals:", min = 2, max = 10, value = 4),
      sliderInput("overlap", "Percent overlap:", min = 10, max = 90, value = 50, step = 5)
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("dataPlot")),
        column(6, forceNetworkOutput("mapperPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive dataset based on user choice
  selected_data <- reactive({
    if (input$data_choice == "circle") {
      return(circle_data)
    } else {
      return(iris_data)
    }
  })
  
  output$dataTable <- renderTable({
    head(selected_data(), 10) 
  })
  
  # Dynamic UI for method parameters
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
             numericInput("num_bins_when_clustering", "Number of bins to use when clustering:", value = 10, min = 1)
           )
    )
  })
  
  # Plot for selected data
  output$dataPlot <- renderPlot({
    data <- selected_data()
    if (input$data_choice == "circle") {
      ggplot(data) +
        geom_point(aes(x = x, y = y, color = circle)) +
        theme_minimal() +
        labs(title = "Circle Data", x = "X", y = "Y", color = "Group")
    } else {
      ggplot(data) +
        geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
        theme_minimal() +
        labs(title = "Iris Data", x = "Sepal Length", y = "Sepal Width", color = "Species")
    }
  })
  
  # Mapper Algorithm and Force Network
  output$mapperPlot <- renderForceNetwork({
    req(input$clustering_method)
    showNotification(paste("Computing Mapper with", input$clustering_method), 
                     duration = NULL, type = "message", id="mapper_computing")
    
    # Get selected dataset
    data <- selected_data()
    
    # Choose filter function (different for each dataset)
    filter_values <- if (input$data_choice == "circle") {
      data[, c("x", "y")]
    } else {
      data[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
    }
    
    method_params <- switch(
      input$clustering_method,
      "dbscan" = list(eps = input$eps, minPts = input$minPts),
      "kmeans" = list(max_kmeans_clusters = input$num_clusters),
      "pam" = list(num_clusters = input$num_clusters),
      "hierarchical" = list(num_bins_when_clustering = input$num_bins_when_clustering, method = input$hclust_method),
      stop("Unknown clustering method")
    )
    
    # Compute Mapper
    time_taken <- system.time({
      Mapper <- MapperAlgo(
        filter_values = filter_values,
        intervals = input$intervals,
        percent_overlap = input$overlap,
        methods = input$clustering_method,
        method_params = method_params,
        cover_type = input$cover_type,
        num_cores = 2
      )
    })
    
    req(Mapper)
    removeNotification("mapper_computing")
    
    MapperPlotter(Mapper, circle_data$circle, circle_data)
  })
}

shinyApp(ui, server)

