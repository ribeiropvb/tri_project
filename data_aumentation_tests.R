
if (!require(mirt)) {
  install.packages("mirt")
}
library(tidyverse)
library(mirt)

install.packages("randomNames")
set.seed(20230202)
df <- 1:10 |>
  purrr::map(\(x) sample(0:5, 25, replace = T)) |>
  dplyr::bind_cols() |>
  magrittr::set_colnames(paste0('Q',1:10))

dplyr::bind_cols(
 sample(1:6, 25, replace = T) |> dplyr::as_tibble() |>
    magrittr::set_colnames('Departamento'),
  df
) %>% write.csv('irt.csv')


model_grm <- mirt(df, itemtype = 'graded')

# Estimate the model parameters
fit_grm <- fscores(model_grm)

# Display the estimated item parameters (discrimination and thresholds)
item_parameters <- coef(model_grm)
for (i in 1:(length(item_parameters)-1)) {
  cat("Item", i, ":\n")
  cat("Discrimination (a):", item_parameters[[i]][1,'a1'], "\n")
  cat("Thresholds (b1, b2, b3, ...):", item_parameters[[i]][1, -1], "\n")
  cat("\n")
}

# You can also make item response predictions using the fitted model if needed
plot(model_grm)
plot(model_grm, type = 'trace')
plot(model_grm, type = 'itemscore')










new_irt <- read_csv("new_irt.csv") %>% dplyr::select(-Departamento) %>%
  mutate_all(as.integer)

model_new_grm <- mirt(new_irt, itemtype = 'graded')

# Estimate the model parameters
fit_new_grm <- fscores(model_new_grm)

# Display the estimated item parameters (discrimination and thresholds)
item_parameters <- coef(model_new_grm)
for (i in 1:(length(item_parameters)-1)) {
  cat("Item", i, ":\n")
  cat("Discrimination (a):", item_parameters[[i]][1,'a1'], "\n")
  cat("Thresholds (b1, b2, b3, ...):", item_parameters[[i]][1, -1], "\n")
  cat("\n")
}

# You can also make item response predictions using the fitted model if needed

plot(model_new_grm)
plot(model_new_grm, type = 'trace')
plot(model_new_grm, type = 'itemscore')





# Load the necessary libraries
library(boot)

# Create an empty list to store augmented datasets
augmented_datasets <- list()

# Number of bootstrap samples
num_bootstrap_samples <- 1000

# Perform bootstrap data augmentation
for (i in 1:num_bootstrap_samples) {
  # Generate a bootstrap sample by resampling with replacement
  bootstrap_indices <- sample(1:nrow(df), replace = TRUE)
  bootstrap_sample <- df[bootstrap_indices, ]
  
  # Store the bootstrap sample in the list of augmented datasets
  augmented_datasets[[i]] <- bootstrap_sample
}

# Access one of the augmented datasets (for example, the first one)
first_augmented_dataset <- do.call(rbind, augmented_datasets)
rownames(first_augmented_dataset) <- NULL
first_augmented_dataset

model_boot_grm <- mirt(first_augmented_dataset, itemtype = 'graded')

# Estimate the model parameters
fit_boot_grm <- fscores(model_boot_grm)

# Display the estimated item parameters (discrimination and thresholds)
item_parameters <- coef(model_boot_grm)
for (i in 1:(length(item_parameters)-1)) {
  cat("Item", i, ":\n")
  cat("Discrimination (a):", item_parameters[[i]][1,'a1'], "\n")
  cat("Thresholds (b1, b2, b3, ...):", item_parameters[[i]][1, -1], "\n")
  cat("\n")
}

# You can also make item response predictions using the fitted model if needed

plot(model_boot_grm)
plot(model_boot_grm, type = 'trace')
plot(model_boot_grm, type = 'itemscore')
