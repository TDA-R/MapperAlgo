library(networkD3)
library(igraph)
library(ggplot2)
library(dplyr)

library(parallel)
library(foreach)
library(doParallel)

source('R/EdgeVertices.R')
source('R/ConvertLevelsets.R')
source('R/Cover.R')
source('R/Cluster.R')
source('R/SimplicialComplex.R')
source('R/MapperAlgo.R')
source('inst/example/ExampleData.R')

pca_result <- reader(dataset_name = 'mnist')

time_record_1 <- c()
time_record_2 <- c()
for (core in 1:2) {
  for (sub in seq(5000, 10000, by = 1000)) {
    lens <- pca_result$x[,1:2]
    lens <- lens[sample(1:nrow(lens), sub),]

    time_taken <- system.time({
      Mapper <- MapperAlgo(
        filter_values = lens,
        intervals = 4,
        percent_overlap = 50,
        methods = "hierarchical",
        method_params = list(num_bins_when_clustering = 10, method = 'ward.D2'),
        num_cores = core
      )
    })

    elapsed_time <- time_taken["elapsed"]

    if (core == 1) {
      time_record_1 <- c(time_record_1, elapsed_time)
    } else {
      time_record_2 <- c(time_record_2, elapsed_time)
    }
  }
}

time_record_df_long <- data.frame(
  samples = seq(5000, 10000, by = 1000),
  core_1 = time_record_1,
  core_2 = time_record_2
) %>%
  # Convert to long format
  pivot_longer(cols = c(core_1, core_2), names_to = "Cores", values_to = "Time")
time_record_df_long
ggplot(time_record_df_long, aes(x = samples, y = Time, color = Cores)) +
  geom_line() +
  geom_point() +
  labs(title = "Time taken for different number of samples and cores (2 dimensions)",
       x = "Number of samples",
       y = "Time taken (s)",
       color = "Cores") +
  theme_minimal()

time_record_1 <- c()
time_record_2 <- c()
for (core in 1:2) {
  for (pc in 2:6) {
    print(pc)

    lens <- pca_result$x[,1:pc]
    lens <- lens[sample(1:nrow(lens), 5000),]

    time_taken <- system.time({
      Mapper <- MapperAlgo(
        data = lens,
        filter_values = lens,
        intervals = 4,
        percent_overlap = 50,
        methods = "hierarchical",
        method_params = list(num_bins_when_clustering = 10, method = 'ward.D2'),
        num_cores = core
      )
    })

    elapsed_time <- time_taken["elapsed"]

    if (core == 1) {
      time_record_1 <- c(time_record_1, elapsed_time)
    } else {
      time_record_2 <- c(time_record_2, elapsed_time)
    }
  }
}

time_record_df_long <- data.frame(
  pc = seq(2,6,1),
  core_1 = time_record_1,
  core_2 = time_record_2
) %>%
  # Convert to long format
  pivot_longer(cols = c(core_1, core_2), names_to = "Cores", values_to = "Time")
time_record_df_long
ggplot(time_record_df_long, aes(x = pc, y = Time, color = Cores)) +
  geom_line() +
  geom_point() +
  labs(title = "Time taken for different number of PCs and cores (5000 samples)",
       x = "Number of PCs",
       y = "Time taken (s)",
       color = "Cores") +
  theme_minimal()




