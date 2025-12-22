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
  
  # Header
  fluidRow(
    column(
      width = 3,
      tags$img(src = "https://raw.githubusercontent.com/kennywang112/kennywang112/ef626c2efc001b6aee6d2237f97a95531e31e154/name.png",
               style = "max-width:100%; height:auto;"
               ),
    ),
    column(
      width = 9,
      tags$h3("Topological Data Analysis: Mapper Algorithm"),
      tags$p(HTML("This Shiny app visualizes the Mapper algorithm for topological data analysis built by 
      <a href='https://kennywang112.github.io/Profile/'>Chi-Chien Wang</a>.<br/>
      You can explore synthetic circle data or the classic Iris dataset with different clustering methods and cover strategies.<br/>
      More details on the Mapper algorithm can be found in the 
                  <a href='https://github.com/kennywang112/MapperAlgo'>Package</a>."
      ))
      
    )
  ),
  tags$hr(),
  
  # Main layout
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

      radioButtons(
        "interval_mode", "Interval specification:",
        choices = c("Number of intervals" = "count",
                    "Interval width" = "width"),
        selected = "count", inline = TRUE
      ),
      # 只有在選 count 時顯示
      conditionalPanel(
        condition = "input.interval_mode == 'count'",
        sliderInput("intervals", "Number of intervals:",
                    min = 2, max = 10, value = 4)
      ),
      # 只有在選 width 時顯示
      conditionalPanel(
        condition = "input.interval_mode == 'width'",
        numericInput("interval_width", "Interval width:",
                    value = 0.5, min = 0, step = 0.1)
      ),


      uiOutput("method_params_ui"),  # dynamic parameter UI
      # sliderInput("intervals", "Number of intervals:", min = 2, max = 10, value = 4),
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
    
    # Placeholder for Mapper Graph
    if (input$clustering_method == "dbscan") {
      validate(
        need(!is.null(input$eps), "eps must be provided"),
        need(!is.null(input$minPts) && input$minPts >= 1, "minPts must be a single integer >= 1")
      )
    }
    
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

    method_params <- isolate({
      switch(
        input$clustering_method,
        "dbscan" = list(
          eps = input$eps,
          minPts = as.integer(input$minPts)
        ),
        "kmeans" = list(max_kmeans_clusters = as.integer(input$num_clusters)),
        "pam" = list(num_clusters = as.integer(input$num_clusters)),
        "hierarchical" = list(
          num_bins_when_clustering = as.integer(input$num_bins_when_clustering),
          method = input$hclust_method
        ),
        stop("Unknown clustering method")
      )
    })
    
    # Compute Mapper
    result <- tryCatch({
  
      label_column <- if (input$data_choice == "circle") {
        circle_data$circle
      } else {
        iris_data$Species
      }

      intervals_val <- if (input$interval_mode == "count") input$intervals else NULL
      interval_width_val <- if (input$interval_mode == "width") input$interval_width else NULL
      
      Mapper <- MapperAlgo(
        filter_values = filter_values,
        intervals = intervals_val,
        interval_width = interval_width_val,
        percent_overlap = input$overlap,
        methods = input$clustering_method,
        method_params = method_params,
        cover_type = input$cover_type,
        num_cores = 2
      )
      
      label_column <- if (input$data_choice == "circle") {
        data$circle  # 改為 selected_data() 結果的 label
      } else {
        data$Species
      }

      removeNotification("mapper_computing")
      MapperPlotter(Mapper, label_column, data)
    }, error = function(e) {
      removeNotification("mapper_computing")
      showNotification(paste("Error running Mapper:", e$message), type = "error", duration = 5)
      NULL
    })
    result
  })
}

shinyApp(ui, server)

