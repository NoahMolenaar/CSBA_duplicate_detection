#install.packages("jsonlite")
#install.packages("dplyr")
#install.packages("stringi")
#install.packages("stringdist")
#install.packages("tokenizers")
library(stringi)
library(jsonlite)
library(dplyr)
library(stringdist)
library(tokenizers)
library(readxl)


# Load the JSON data
data <- fromJSON("~/OneDrive/Noah laptop/Erasmus Universiteit Rotterdam/Master jaar 1 (2023-2024)/Blok 2/Computer Science for Business Analytics/TVs-all-merged.json")
webshops <- c("amazon.com", "newegg.com", "bestbuy.com", "thenerds.net")
webshops_list <- as.list(webshops)


sameShop <- function(product_i, product_j) {
  return(product_list[[product_i]][[1]]==product_list[[product_j]][[1]])
}

brand <- function(product) {
  # Extract the title of the specified product
  product_title <- product_list[[product]][[3]]
  
  # Find common strings between television_brands and product_title
  common_strings <- intersect(unlist(television_brands), unlist(strsplit(product_title, " ")))
  
  # If there are common strings, return the first one, otherwise return NULL
  if (length(common_strings) > 0) {
    return(common_strings[1])
  } else {
    return(NULL)
  }
}

diffBrand <- function(product_i, product_j) {
  if (!is.null(brand(product_i)) && !is.null(brand(product_j)) && brand(product_i) != brand(product_j)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

kv <- function(product) {
  #featuresMap <- data[[product]]$featuresMap
  featuresMap <- product_list[[product]][[5]]
  pairs_list <- as.list(featuresMap)
  return(pairs_list)
}

q3 <- 3
calcSim <- function(s1, s2) {
  # Function to calculate the q-gram distance between two strings
  qGramDistance <- stringdist(s1,s2,method='qgram',q=q3)
  
  # Calculate q-gram distance
  distance <- qGramDistance
  n1 <- length(tokenize_character_shingles(s1, n = 3, n_min = 3, strip_non_alphanum = FALSE)[[1]])
  n2 <- length(tokenize_character_shingles(s2, n = 3, n_min = 3, strip_non_alphanum = FALSE)[[1]])
  
  # Calculate q-gram similarity value
  similarity_value <- (n1 + n2 - distance) / (n1 + n2)
  
  return(similarity_value)
}

exMW <- function(nmk) {
  nmk <- lapply(nmk, function(x) gsub('["”]', "inch", x))
  nmk <- lapply(nmk, function(x) gsub('-inch', "inch", x))
  nmk <- lapply(nmk, function(x) gsub(' hz', 'hz', x))
  nmk <- lapply(nmk, function(x) gsub(' - ', ' ', x))
  nmk <- lapply(nmk, function(x) gsub(' / ', ' ', x))
  nmk <- lapply(nmk, function(x) gsub('[()]', '', x))
  nmk <- lapply(nmk, function(x) gsub(' & ', '', x))
  
  model_nmk <- unique(unlist(stri_extract_all_regex(as.character(nmk),"[a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*")))
  model_nmk <- model_nmk[!sapply(model_nmk, is.na)]
  return(model_nmk)
}

mw <- function(list1,list2){
  
  # Calculate the intersection and union sizes
  intersection_size <- length(intersect(list1, list2))
  union_size <- length(union(list1, list2))
  
  # Calculate the Jaccard similarity coefficient
  jaccard_similarity <- intersection_size / union_size
  
  # Convert to percentage
  similarity_percentage <- jaccard_similarity #* 100
  
  return(similarity_percentage)
}

TMWMSim <- function(pi,pj,alpha,beta){
  #tmw_i <- exMW(data[[pi]]$title)
  #tmw_j <- exMW(data[[pj]]$title)
  tmw_i <- exMW(product_list[[pi]][[3]])
  tmw_j <- exMW(product_list[[pj]][[3]])
  
  calcCosineSim <- length(intersect(tmw_i, tmw_j))/(sqrt(length(tmw_i))*sqrt(length(tmw_j)))
  
  if(calcCosineSim<=beta){
    return(-1)
  }
  if(calcCosineSim>=alpha){
    return(1)
  }
  return(calcCosineSim)
}

binary_duplicate <- numeric(length(product_list))
for (i in 1:(length(product_list)-1)){
  for (j in (i+1):length(product_list)){
    if (product_list[[i]][[4]]==product_list[[j]][[4]]){
      binary_duplicate[i]<-1
      binary_duplicate[j]<-1
    }
  }
}

Pe_Re_MSM <- function(clusters, product_list) {
  true_positives <- 0
  false_positives <- 0
  false_negatives <- 0
  
  for (cluster in clusters) {
    cluster_size <- length(cluster)
    
    # Compare each pair of products within the cluster
    if(cluster_size==1)
    {
      if(binary_duplicate[cluster]==1){
        false_negatives <- false_negatives + 1
      }
    }
    else{
      for (i in 1:(cluster_size - 1)) {
        for (j in (i + 1):cluster_size) {
          # Check if the pair is a true duplicate based on model_id
          if (product_list[[cluster[i]]][[4]] == product_list[[cluster[j]]][[4]]) {
            true_positives <- true_positives + 1
          } else {
            false_positives <- false_positives + 1
          }
        }
      }
    }
  }
  return_TP_FP_FN <- c(true_positives, false_positives, false_negatives)
  return(return_TP_FP_FN)
}

PC_PQ_MSM <- function(clusters, duplicates_sample, product_list) {
  comparisons <- sum(sapply(clusters, function(pair) choose(length(pair), 2)))
  duplicates_MSM_found <- 0
  
  for (clust in clusters) {
    cluster_size <- length(clust)
    
    if (cluster_size > 1) {
      for (i in 1:(cluster_size - 1)) {
        for (j in (i + 1):cluster_size) {
          if (product_list[[clust[i]]][[4]] == product_list[[clust[j]]][[4]]) {
            duplicates_MSM_found <- duplicates_MSM_found + 1
          }
        }
      }
    }
  }
  PC <- duplicates_MSM_found / duplicates_sample
  PQ <- duplicates_MSM_found / comparisons
  return(list(PC = PC,PQ = PQ))
}


F1MSM <- function(PQ_msm, PC_msm) {
  F1_msm <- (2 * PQ_msm * PC_msm) / (PQ_msm + PC_msm)
  return(F1_msm)
}


isInf <- function(clust1, clust2, distmat) {
  for (i in clust1) {
    for (j in clust2) {
      i_char <- as.character(i)
      j_char <- as.character(j)
      
      if (i < j && is.infinite(distmat[i_char, j_char])) {
        return(TRUE)
      } else if (i > j && is.infinite(distmat[j_char, i_char])) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}


# Function to calculate the dissimilarity between two clusters using single linkage
Dissim <- function(clust1, clust2, distmat) {
  minDis <- Inf
  for (i in clust1){
    for (j in clust2){
      i_char <- as.character(i)
      j_char <- as.character(j)
      if (distmat[i_char, j_char]<minDis){
        minDis <- distmat[i_char, j_char]
      }
    }
  }
  return(minDis)
}



product_list <- list()
for (product in 1:length(data)){
  if (length(data[[product]]$shop)==1){
    #product_list <- c(product_list, list(data[[product]]))
    shop_list <- list()
    fMap <- list()
    shop_list <- c(shop_list, data[[product]]$shop)
    shop_list <- c(shop_list, data[[product]]$url)
    shop_list <- c(shop_list, data[[product]]$title)
    shop_list <- c(shop_list, data[[product]]$modelID)
    fMap <- c(fMap, data[[product]]$featuresMap)
    shop_list <- c(shop_list, list(fMap))
    product_list <-c(product_list,list(shop_list))
  }
  else{
    for (shop in 1:length(data[[product]]$shop)){
      shop_list <- list()
      fMap <- list()
      shop_list <- c(shop_list, data[[product]]$shop[shop])
      shop_list <- c(shop_list, data[[product]]$url[shop])
      shop_list <- c(shop_list, data[[product]]$title[shop])
      shop_list <- c(shop_list, data[[product]]$modelID[shop])
      fMap <- c(fMap, data[[product]]$featuresMap[shop, !is.na(data[[product]]$featuresMap[shop,])])
      shop_list <- c(shop_list, list(fMap))
      product_list <-c(product_list,list(shop_list))
    }
  }
}


# Extract the keys (modelIDs)
model_ids <- names(data)

# Initialize an empty list to store data frames
dfs <- list()

# Loop through each modelID and convert the associated data to a data frame
for (model_id in model_ids) {
  model_data <- data[[model_id]]
  
  # Convert to a data frame, ensuring unique row names
  df <- as.data.frame(model_data, row.names = NULL)
  
  # Add a column for model_id
  df$model_id <- model_id
  
  dfs[[model_id]] <- df
}

# Combine all data frames into one using dplyr
combined_df <- bind_rows(dfs)
new_combined_df <- combined_df[, -which(names(combined_df) == "featuresMap")]
new_combined_df[] <- lapply(new_combined_df, tolower)

#Data cleaning
new_combined_df$title <- gsub('["”]', 'inch', new_combined_df$title)
new_combined_df$title <- gsub('-inch', 'inch', new_combined_df$title)
new_combined_df$title <- gsub('-inches', 'inch', new_combined_df$title)
new_combined_df$title <- gsub(' inches', 'inch', new_combined_df$title)
new_combined_df$title <- gsub('hertz', 'hz', new_combined_df$title)
new_combined_df$title <- gsub(' hz', 'hz', new_combined_df$title)
new_combined_df$title <- gsub('[()]', '', new_combined_df$title)

model_words <- unique(unlist(stri_extract_all_regex(
  as.character(new_combined_df$title),
  "[a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*"
)))


# Convert to a set (using a named vector for easy lookup)
unique_model_words_set <- setNames(rep(TRUE, length(model_words)), model_words)

television_brands <- read_excel("Library/CloudStorage/OneDrive-Persoonlijk/Noah laptop/Erasmus Universiteit Rotterdam/Master jaar 1 (2023-2024)/Blok 2/Computer Science for Business Analytics/television brands.xlsx")
television_brands <- lapply(television_brands, tolower)


model_words <- c(model_words, unlist(television_brands))

binary_matrix <- sapply(model_words, function(word) {
  as.integer(stri_detect(new_combined_df$title, fixed = word))
})


# Convert to a data frame
binary_df <- data.frame(binary_matrix)
binary_trans <- t(binary_df)


# Number of bootstrap iterations
num_bootstraps <- 5
epsilons<- 0.68

# Values of bands to try
band_values <- 40 #c(25, 40, 50, 80, 100, 125, 200, 400)
duplicates_sample_best <- 0

# Initialize a data frame to store results
results_df <- data.frame(Bands = numeric(), F1_lsh = numeric())


fraction_comparisons_matrix <- matrix(0, nrow = length(band_values), ncol = num_bootstraps)
PC_lsh_matrix <- matrix(0, nrow = length(band_values), ncol = num_bootstraps)
PQ_lsh_matrix <- matrix(0, nrow = length(band_values), ncol = num_bootstraps)

PC_msm_matrix <- matrix(0, nrow = length(epsilons), ncol = num_bootstraps)
PQ_msm_matrix <- matrix(0, nrow = length(epsilons), ncol = num_bootstraps)
F1_msm_matrix <- matrix(0, nrow = length(epsilons), ncol = num_bootstraps)
PeRe_msm_matrix <- matrix(0, nrow = length(epsilons), ncol = num_bootstraps)
Pe_msm_matrix <- matrix(0, nrow = length(epsilons), ncol = num_bootstraps)
Re_msm_matrix <- matrix(0, nrow = length(epsilons), ncol = num_bootstraps)


F1_lsh_best_matrix <- matrix(0, nrow = num_bootstraps, ncol = 3)


# Perform bootstrapping for each value of bands
for (bootstrap in 1:num_bootstraps) {
  set.seed(123+bootstrap)
  sampled_indices <- sample(ncol(binary_trans), size = round(0.63 * ncol(binary_trans)), replace = FALSE)
  all_indices <- matrix(1:1624, nrow = 1624, ncol = 1)
  sampled_indices_oos <- all_indices[!(all_indices %in% sampled_indices)]
  # Sample approximately 63% of the original data with replacement
  sampled_data <- binary_trans[, sampled_indices_oos, drop = FALSE]
  colnames(sampled_data) <- sampled_indices_oos
  
  sampled_product_list <- product_list[sampled_indices_oos]
  
  # Initialize a vector to store F1_lsh values for each value of bands
  f1_values <- numeric()
  
  best_overview_matrix <- matrix(0, nrow = ncol(sampled_data), ncol = ncol(sampled_data))
  F1_lsh_best <- 0
  
  # Perform LSH for each value of bands
  for (bands in band_values) {
    permutations = 800
    set.seed(123)
    
    sig_matrix <-matrix(NA, nrow = permutations, ncol = ncol(sampled_data))
    colnames(sig_matrix) <- colnames(sampled_data)
    # Perform permutations and record the first 1 row for each column
    for (i in 1:permutations) {
      binary_trans_perm <-
        sampled_data[sample(nrow(sampled_data)), , drop = FALSE]
      first_1_rows <-
        apply(binary_trans_perm, 2, function(col)
          which(col == 1)[1])
      sig_matrix[i,] <- first_1_rows
    }
    
    rows = nrow(sig_matrix) / bands
    t <- (1 / bands) ^ (1 / rows)
    
    # Initialize buckets
    buckets <- list()
    
    # Divide the signature matrix into bands and hash vectors to buckets
    for (band in 1:bands) {set.seed(123+band)
      start_row <- (band - 1) * rows + 1
      end_row <- band * rows
      band_matrix <- sig_matrix[start_row:end_row, ]
      
      # Hash each vector in the band to a bucket
      band_buckets <- sapply(1:ncol(band_matrix), function(col) {
        hash_value <- paste(band_matrix[, col], collapse = "")
        return(hash_value)
      })
      
      # Store the band buckets
      buckets[[band]] <- band_buckets
    }
    
    candidate_pairs <- list()
    for (band in 1:bands) {
      unique_buckets <- unique(buckets[[band]])
      
      # For each unique bucket in the band, find columns (products) that fall into the same bucket
      for (bucket in unique_buckets) {
        col_indices <- which(buckets[[band]] == bucket)
        
        # If there are at least two columns in the same bucket, consider them as candidate pairs
        if (length(col_indices) > 1) {
          # Get the column names corresponding to col_indices
          
          candidate_pairs[[length(candidate_pairs) + 1]] <-
            list(Band = band,
                 Bucket = bucket,
                 Products = col_indices)
        }
      }
    }
    
    # Initialize matrix
    overview_matrix <- matrix(0,
                              nrow = ncol(sampled_data),
                              ncol = ncol(sampled_data))
    rownames(overview_matrix) <- colnames(overview_matrix) <- sampled_indices_oos
    
    
    
    # Populate matrix based on candidate pairs
    for (pair_list in 1:length(candidate_pairs)) {
      for (product1 in 1:length(candidate_pairs[[pair_list]]$Products)) {
        for (product2 in 1:length(candidate_pairs[[pair_list]]$Products)) {
          if (product1 != product2)
          {
            overview_matrix[candidate_pairs[[pair_list]]$Products[product1], candidate_pairs[[pair_list]]$Products[product2]] <- 1
          }
        }
      }
    }
    
    # Set diagonal elements back to 0
    diag(overview_matrix) <- 0
    
    # Set lower triangle elements to 0
    overview_matrix[lower.tri(overview_matrix)] <- 0
    
    # Initialize counter
    duplicates_lsh_found <- 0
    
    # Get the dimensions of the matrix
    n <- nrow(overview_matrix)
    
    # Iterate over the upper triangular part of the matrix
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        if (overview_matrix[i, j] == 1) {
          # Convert row and column names to integers
          ii <- as.integer(rownames(overview_matrix)[i])
          jj <- as.integer(colnames(overview_matrix)[j])
          
          # Check if the fourth elements are equal and increment counter if true
          if (product_list[[ii]][[4]] == product_list[[jj]][[4]]) {
            duplicates_lsh_found <- duplicates_lsh_found + 1
          }
        }
      }
    }
    
    duplicates_sample <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        # Check if the fourth elements are equal and increment counter if true
        if (product_list[[i]][[4]] == product_list[[j]][[4]]) {
          duplicates_sample <- duplicates_sample + 1
        }
      }
    }
    
    count_elements_above_diagonal <- function(matrix) {
      count <- 0
      n <- nrow(matrix)
      
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          # Check if the element is above the diagonal
          count <- count + 1
        }
      }
      
      return(count)
    }
    comparisons_lsh <- sum(overview_matrix)
    fraction_comparisons <- comparisons_lsh/count_elements_above_diagonal(overview_matrix)
    #"The fraction of comparisons is defined as the candidate duplicate-pairs found by LSH divided by the total number
    #of possible comparisons"
    
    PQ_lsh <- duplicates_lsh_found / comparisons_lsh
    duplicates_total <- nrow(combined_df) - length(data) 
    PC_lsh <- duplicates_lsh_found / duplicates_sample
    F1_lsh <- (2 * PQ_lsh * PC_lsh) / (PQ_lsh + PC_lsh)
    #print(bootstrap)
    #print(band)
    #print(PQ_lsh)
    #print(PC_lsh)
    #print(F1_lsh)
    
    # Store F1_lsh for this value of bands
    f1_values <- c(f1_values, F1_lsh)
    
    band_index <- which(band_values == bands)
    fraction_comparisons_matrix[band_index, bootstrap] <- fraction_comparisons
    PC_lsh_matrix[band_index, bootstrap] <- PC_lsh
    PQ_lsh_matrix[band_index, bootstrap] <- PQ_lsh
    
    if(F1_lsh>F1_lsh_best){
      F1_lsh_best <- F1_lsh
      best_overview_matrix <- overview_matrix
      F1_lsh_best_matrix[bootstrap,] <- c(F1_lsh_best, bands, permutations)
      duplicates_sample_best <- duplicates_sample
    }
  }
  
  # Find the optimal value of t with the highest F1_lsh for this bootstrap iteration
  #optimal_t <- band_values[which.max(f1_values)]
  #optimal_f1 <- max(f1_values)
  
  # Store the results in the data frame
  #results_df <- rbind(results_df, data.frame(Bands = optimal_t, F1_lsh = optimal_f1))
  
  n <- nrow(best_overview_matrix)
  dist <- n*n
  
  alpha <- 0.602
  beta <- 0
  gamma <- 0.756
  mu <- 0.650
  
  distmat <- matrix(Inf, n, n)
  colnames(distmat) <- sampled_indices_oos
  rownames(distmat) <- sampled_indices_oos
  colnames(distmat) <- as.character(colnames(distmat))
  rownames(distmat) <- as.character(rownames(distmat))
  
  #MSM
  for (webshop in webshops_list){
    for (producti in 1:nrow(best_overview_matrix)){ #combined_df kan evt vervangen worden door product_list
      #print(producti)
      if (product_list[[as.integer(rownames(best_overview_matrix)[producti])]][[1]]==webshop){
        for (productj in 1:nrow(best_overview_matrix)){
          if(best_overview_matrix[producti, productj]==1) {
            if (product_list[[as.integer(rownames(best_overview_matrix)[productj])]][[1]] != webshop){
              if (sameShop(producti,productj)||diffBrand(producti,productj)){
                #distmat[producti,productj] <- Inf
                #distmat[productj,producti] <- Inf
              }
              else {
                sim <- 0
                avgSim <- 0
                m <- 0
                w <- 0
                nmk_i <- kv(as.integer(rownames(best_overview_matrix)[producti]))
                nmk_j <- kv(as.integer(rownames(best_overview_matrix)[productj]))
                for (pair_i in 1:length(kv(as.integer(rownames(best_overview_matrix)[producti])))){
                  for (pair_j in 1:length(kv(as.integer(rownames(best_overview_matrix)[productj])))){
                    keySim <- calcSim(names(product_list[[as.integer(rownames(best_overview_matrix)[producti])]][[5]])[pair_i],names(product_list[[as.integer(rownames(best_overview_matrix)[productj])]][[5]])[pair_j])
                    if (keySim > gamma){
                      valueSim <- calcSim(product_list[[as.integer(rownames(best_overview_matrix)[producti])]][[5]][pair_i],product_list[[as.integer(rownames(best_overview_matrix)[productj])]][[5]][pair_j])
                      weight <- keySim
                      sim <- sim + weight * valueSim
                      m <- m + 1
                      w <- w + weight
                      nmk_i[names( kv(as.integer(rownames(best_overview_matrix)[producti]))[pair_i])] <- NULL # hier nog goed naar kijken, want index verschuift
                      nmk_j[names( kv(as.integer(rownames(best_overview_matrix)[productj]))[pair_j])] <- NULL
                    }
                  }
                }
                if (w>0){
                  avgSim <- sim/w
                }
                mwPerc <- mw(exMW(nmk_i),exMW(nmk_j))
                titleSim=TMWMSim(as.integer(rownames(best_overview_matrix)[producti]), as.integer(rownames(best_overview_matrix)[productj]), alpha, beta)
                minFeatures <- min(length(product_list[[as.integer(rownames(best_overview_matrix)[producti])]][[5]]),length(product_list[[as.integer(rownames(best_overview_matrix)[productj])]][[5]]))
                if (titleSim==-1) {
                  theta1 <- m/minFeatures
                  theta2 <- 1 - theta1
                  hSim <- theta1*avgSim + theta2*mwPerc
                } else {
                  theta1 <- (1 - mu)*(m/minFeatures)
                  theta2 <- 1 - mu - theta1
                  hSim <- theta1*avgSim + theta2*mwPerc + mu*titleSim
                }
                distmat[producti,productj] <- 1 - hSim
              }
            }
          }
        }
      }
    }
  }
  
  clusters <- as.list(sampled_indices_oos)
  F1_msm <- list()
  
  for(epsilon in epsilons){
    while (length(clusters) > 1) {
      minDissimilarity <- Inf
      mergeClusters <- c(0, 0)
      for (i in 1:(length(clusters)-1)) {
        for (j in (i + 1):length(clusters)) {
          if (!isInf(clusters[[i]], clusters[[j]], distmat)) {
            dissimilarity <- Dissim(clusters[[i]], clusters[[j]], distmat)
            if (dissimilarity < minDissimilarity) {
              minDissimilarity <- dissimilarity
              mergeClusters <- c(i, j)
              #print(i)
              #print(j)
              #print(dissimilarity)
              #print(minDissimilarity)
            }
          }
        }
      }
      if (minDissimilarity > epsilon) {
        break
      }
      #print(length(clusters))
      
      # Merge the two closest clusters
      newCluster <- c(clusters[[mergeClusters[1]]], clusters[[mergeClusters[2]]])
      clusters <- append(clusters[-mergeClusters], list(newCluster))
    }
    
    PQ_msm <- PC_PQ_MSM(clusters, duplicates_sample_best, product_list)$PQ
    PC_msm <- PC_PQ_MSM(clusters, duplicates_sample_best, product_list)$PC
    f1_msm_eps <- F1MSM(PQ_msm, PC_msm)
    
    PC_msm_matrix[which(epsilons==epsilon), bootstrap] <- PC_msm
    PQ_msm_matrix[which(epsilons==epsilon), bootstrap] <- PQ_msm
    F1_msm_matrix[which(epsilons==epsilon), bootstrap] <-f1_msm_eps
    
    
    
    TPFPFN<-Pe_Re_MSM(clusters, product_list)
    TPValue <-TPFPFN[1]
    FPValue <-TPFPFN[2]
    FNValue <-TPFPFN[3]
    PrecisionValue <- TPValue/(TPValue+FPValue)
    RecallValue <- TPValue/(TPValue+FNValue)
    PeRe_msm_matrix[which(epsilons==epsilon), bootstrap] <- 2*PrecisionValue*RecallValue/(PrecisionValue+RecallValue)
    Pe_msm_matrix[which(epsilons==epsilon), bootstrap] <- PrecisionValue
    Re_msm_matrix[which(epsilons==epsilon), bootstrap] <- RecallValue
  }
}