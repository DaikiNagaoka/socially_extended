# VaDER clustering ----
library(reticulate)

# Hyperparameters determined via grid search
k <- 7 # number of clusters
learning_rate <- 1e-3
batch_size <- 128
n_layer <- 2 # number of hidden layers
n_hidden1 <- 32 # nodes in the first layer
n_hidden2 <- 8  # nodes in the second layer

# L is a list of data tables
# L$X contains scores for each participant, timepoint, and variable
# L$W represents whether the value is missing (0) or not (1)
# Dimensions of L$X and L$W are [participants, timepoints, variables]
X_train <- L$X
W_train <- L$W

# VaDER code is available at https://github.com/johanndejong/VaDER
VADER <- reticulate::import_from_path("vader", path = "../VaDER/")$VADER
save_path <- file.path("vader", "vader.ckpt")

# Initialize VaDER model
vader <- VADER(
  X_train = X_train,
  save_path = save_path,
  n_hidden = lapply(c(n_hidden1, n_hidden2), as.integer),
  k = as.integer(k),
  learning_rate = learning_rate,
  batch_size = as.integer(batch_size),
  output_activation = NULL,
  alpha = 1.0,
  seed = 1L, # Perform clustering with 1000 different seeds
  recurrent = TRUE,
  W_train = W_train
)

# Pre-fit phase with 100 epochs
system.time(vader$pre_fit(n_epoch = as.integer(100), verbose = TRUE))

# Fit phase with 100 epochs
system.time(vader$fit(n_epoch = as.integer(100), verbose = TRUE))

# Obtain clustering results
yhat <- vader$cluster(X_train, W_train)


# Consensus clustering ----
library(dplyr)
library(ConsensusClusterPlus)

n_samples <- nrow(L$X) # Number of participants
co_occurrence_list <- list() # Initialize list for co-occurrence matrices
dir_path <- "../results" # Directory containing clustering results
clustering_files <- list.files(dir_path, pattern = "*\\.csv") # Fetch clustering result files

# Create a matrix representing the probability of participants belonging to the same cluster
for (file_name in clustering_files) {
  file_path <- file.path(dir_path, file_name)
  clustering_result <- read.csv(file_path, sep = ";", row.names = 1)
  
  co_occurrence_matrix <- matrix(0, nrow = n_samples, ncol = n_samples)
  
  valid_cluster <- TRUE
  for (cluster in unique(clustering_result$x)) {
    same_cluster_indices <- which(clustering_result$x == cluster)
    
    # Ensure each cluster contains a minimum number of participants
    num_threshold <- 80 # At least 80 participants required
    if (length(same_cluster_indices) < num_threshold) {
      valid_cluster <- FALSE
      break
    }
    
    # Update co-occurrence matrix
    co_occurrence_matrix[same_cluster_indices, same_cluster_indices] <- 1
  }
  
  # Append to the list if valid
  if (valid_cluster) {
    co_occurrence_list[[length(co_occurrence_list) + 1]] <- co_occurrence_matrix
  }
}

# Calculate the average co-occurrence matrix
average_co_occurrence_matrix <- Reduce("+", co_occurrence_list) / length(co_occurrence_list)

# Convert co-occurrence probability to distance
distance_matrix <- 1 - average_co_occurrence_matrix

# Perform consensus clustering using "ConsensusClusterPlus"
result <- ConsensusClusterPlus(distance_matrix,
                               maxK = k, # number of clusters
                               pItem = 1,
                               pFeature = 1,
                               reps = 1,
                               distance = "euclidean",
                               seed = 123,
                               clusterAlg = "hc" # hierarchical clustering
)

# Extract consensus cluster labels
cluster_consensus <- result[[k]]$consensusClass
