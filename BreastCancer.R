
################################################################################
## Environment SetUp 
################################################################################


## Set some options
options(
  encoding = "UTF-8",
  scipen=1000
)


## Download required package if necessary
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(xfun)) install.packages("xfun", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(klaR)) install.packages("klaR", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")


## Load required packages
library(tidyverse)
library(data.table)
library(scales)
library(xfun)
library(knitr)
library(RColorBrewer)
library(caret)
library(klaR)
library(randomForest)
library(ggcorrplot)
library(kableExtra)
library(caTools)





################################################################################
## Getting Data: load  Wisconsin Diagnostic Breast Cancer dataset (WDBC)
################################################################################

url_to_wdbc_data <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
wdbc_data <- fread(url_to_wdbc_data) %>% as_tibble()


## WDBC dataset description from the url below,
## ten real-valued features computed for each cell nucleus
## http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc_data.names
ten_real_features <- c(
  "radius",
  "texture",
  "perimeter",
  "area",
  "smoothness",
  "compactness",
  "concavity",
  "concave points",
  "symmetry",
  "fractal dimension"
)

feature_description <- tibble(
  Feature = str_to_title(ten_real_features),
  Description = c(
    "Mean of distances from center to points on the perimeter for each cell nucleus",
    "Standard deviation of gray-scale values",
    "Perimeter length of each cell nucleus",
    "Area of each cell nucleus",
    "Local variation in radius lengths",
    "Calculated using the formula `perimeter^2 / area - 1.0'",
    "Severity of concave portions of the contour",
    "Number of concave portions of the contour",
    "Symmetry of the nucleus as measured by length differences between lines perpendicular to the major axis and the cell boundary",
    "Based on `coastline approximation' - 1"
  )
)

all_columns_names <- c(
  "id_number", "diagnosis",
  paste0(str_replace_all(string = ten_real_features, pattern = " ", replacement = "_"), "_mean"),
  paste0(str_replace_all(string = ten_real_features, pattern = " ", replacement = "_"), "_se"),
  paste0(str_replace_all(string = ten_real_features, pattern = " ", replacement = "_"), "_worst")
)

names(wdbc_data) <- all_columns_names


# glimpse(wdbc_data)
# any(is.na(wdbc_data))


################################################################################
## Useful functions
################################################################################


#####################################
## Print tabular data
#####################################

.print_tabular_data <- function(df, caption) {
  df %>%
    kable(
      caption = caption, 
      align = 'll', 
      booktabs = T,
      format = "latex", 
      linesep = ""
    ) %>%
    kable_styling(
      full_width = FALSE, 
      position = "center", 
      latex_options = c("scale_down", "hold_position")
    ) -> tab
  
  return(tab)
}



#####################################
## Statistics for features
#####################################

.descriptive_statistics <- function(suffix = "_mean$", df = tibble()) {
  df_chunc <- df %>%
    dplyr::select(all_of(names(df)[str_detect(string = names(df), pattern = suffix)]))
  
  stats_matrix <- do.call(cbind, lapply(df_chunc, summary))
  
  stats_df <- tibble(` ` = row.names(stats_matrix)) %>%
    bind_cols(as_tibble(stats_matrix))
  
  return(stats_df)
}





################################################################################
## Exploratory Data Analysis (EDA)
################################################################################



#####################################
## Distribution of Diagnosis
#####################################

distribution_of_diagnosis <- wdbc_data %>%
  group_by(diagnosis) %>%
  summarise(.groups = 'drop', cnt = n()) %>%
  ungroup() %>%
  mutate(total_cnt = sum(cnt)) %>%
  ggplot(aes(x = "", y = cnt, fill = diagnosis)) +
  geom_col(color = "black") +
  geom_label(
    aes(label = paste0(cnt, '\n', label_percent()(cnt / total_cnt))),
    position = position_stack(vjust = 0.5),
    show.legend = FALSE
  ) +
  coord_polar(theta = "y") +
  theme(
    plot.title = element_text(hjust = 0.5, color="#010101", size = rel(1.0)),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "top"
  )



#####################################
## Histogram for each feature
#####################################

feature_histogram <- wdbc_data %>%
  dplyr::select(-c(id_number)) %>%
  gather("feature", "value", -c(diagnosis)) %>%
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 30) +
  xlab("Feature values") +
  ylab("Count") +
  theme(legend.position = "top",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        legend.title=element_blank()) +
  scale_fill_discrete(labels = c("Benign", "Malignant")) +
  facet_wrap(~ feature, scales = "free", ncol = 3)



#####################################
## Density plots for each feature 
## by diagnosis
#####################################

feature_density_plots <- wdbc_data %>% 
  dplyr::select(-id_number) %>%
  gather("feature", "value", -diagnosis) %>%
  ggplot(aes(value, fill = diagnosis)) +
  geom_density(alpha = 0.5) +
  xlab("Feature values") +
  ylab("Density") +
  theme(legend.position = "top",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        legend.title=element_blank()) +
  scale_fill_discrete(labels = c("Benign", "Malignant")) +
  facet_wrap(~ feature, scales = "free", ncol = 3)



#####################################
## Correlation Heatmap
#####################################

## Compute a correlation matrix
featutes_names <- all_columns_names[!(all_columns_names %in% c("id_number", "diagnosis"))]
corr <- round(cor(wdbc_data %>% dplyr::select(all_of(featutes_names))), 1) # , method = "pearson"
corr <- corr[featutes_names , featutes_names]


## Visualize the correlation matrix
correlation_heatmap <- ggcorrplot(corr, hc.order = TRUE, legend.title = "Corr (r)") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )


################################################################################
# Data wrangling
################################################################################



#####################################
## Label encoding
#####################################

ordered_col_names <- c('diagnosis')
for(item in names(wdbc_data)) {
  if (!(item %in% c('id_number', 'diagnosis'))) {
    ordered_col_names <- c(ordered_col_names, item)
  }
}

wdbc <- wdbc_data %>%
  dplyr::select(all_of(ordered_col_names)) %>%
  mutate(diagnosis = factor(diagnosis))



#####################################
## Split WDBC dataset into 
## training(80%) and testing(20%) sets
#####################################

## Set seed for reproductibility
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(200)                         ## if using R 3.5 or earlier, use `set.seed(200)`
} else {
  print("not using R 3.5 or earlier")
  set.seed(200, sample.kind="Rounding") ## else, `set.seed(200, sample.kind="Rounding")`
}

test_index <- createDataPartition(wdbc$diagnosis, times = 1, p = 0.2, list = FALSE)
test_set <- wdbc[test_index,]
train_set <- wdbc[-test_index,]

## Check
nrow(wdbc) - nrow(train_set) - nrow(test_set)



#####################################
## Feature scaling
#####################################

## Scale train set
## When we center and scale a variable in the training data using the mean and 
## sd of that variable calculated on the training data, we are essentially 
## creating a brand-new variable. Then we train our models on that brand new 
## variable.
## To use that new variable to predict for the validation and/or test datasets, 
## we have to create the same variable in those data sets, 
## by subtracting the same mean and dividing by the same sd calculated from 
## the training set.

train_means <- train_set %>% dplyr::select(-c(diagnosis)) %>% summarise_all(mean)
train_sds <- train_set %>% dplyr::select(-c(diagnosis)) %>% summarise_all(sd)

col_names <- names(train_means)
train_cs <- train_set
test_cs <- test_set
for (col_name in col_names) {
  train_col_mean <- train_means[col_name] %>% pull()
  train_col_sd <- train_sds[col_name] %>% pull()
  
  ## Center & Scale (_cs) Training and Testing Data
  train_cs[,col_name] <- (train_cs[,col_name] - train_col_mean) / train_col_sd
  test_cs[,col_name] <- (test_cs[,col_name] - train_col_mean) / train_col_sd
}


## Checks
train_cs %>% summarise_if(is.numeric, mean)
train_cs %>% summarise_if(is.numeric, sd)

## Store train and test data in a list
## Convert tibble to data frame for some functions (varImp for example) 
## to work properly
data <- list(
  train_x = train_set %>% dplyr::select(-c(diagnosis)) %>% as.data.frame(),
  train_x_cs = train_cs %>% dplyr::select(-c(diagnosis)) %>% as.data.frame(),
  train_y = train_set$diagnosis,
  #
  test_x = test_set %>% dplyr::select(-c(diagnosis)) %>% as.data.frame(),
  test_x_cs = test_cs %>% dplyr::select(-c(diagnosis)) %>% as.data.frame(),
  test_y = test_set$diagnosis
)



#####################################
## Exploring training set 
## M and B samples
#####################################

## Distance between each sample
distance_samples <- dist(data$train_x_cs)

## Average distance between all samples
avg_distance_all_samples <- mean(as.matrix(distance_samples))

## Average distance between B samples
avg_distance_b_samples <- mean(as.matrix(distance_samples)[data$train_y == "B"])

## Average distance between M samples
avg_distance_m_samples <- mean(as.matrix(distance_samples)[data$train_y == "M"])

# Average distance between B and M samples
avg_distance_bm_samples <- mean(as.matrix(distance_samples)[data$train_y == "M", data$train_y == "B"])



#####################################
## Correlation between training set 
## features
#####################################

## Identifying correlated predictors
train_corr <- abs(cor(data$train_x_cs))
cut_off <- 90 / 100
corr_index <- findCorrelation(train_corr, cutoff = cut_off, names = FALSE)
corr <- findCorrelation(train_corr, cutoff = cut_off, names = TRUE)


#####################################
## Principal Component Analysis
## training set
#####################################

## Create PCA object
pca <- prcomp(data$train_x_cs)

## Variance scores per principal component
pca.var <- pca$sdev^2
pca.var.per <- pca.var/sum(pca.var)

## First 10 Principal Components
first_10_pcs <- summary(pca)$importance[,1:10]

## Box plot of top 10 principal components by diagnosis
box_plot_first_10_pcs <- tibble(
  as_tibble(pca$x[,1:10]), 
  Diagnosis = train_set$diagnosis
 ) %>%
  gather(key = "Principal Component", value = "Value", -c(Diagnosis)) %>%
  ggplot(aes(`Principal Component`, Value, fill = Diagnosis)) +
  geom_boxplot() +
  scale_fill_discrete(
    name = "Diagnosis",
    breaks = c("B", "M"),
    labels = c("Benign", "Malignant")
  ) +
  theme(legend.position = "top")


## Scatter plot of first 2 principal components by diagnosis
scatter_plot_first_2_pcs <- tibble(
  as_tibble(pca$x[,1:2]), 
  Diagnosis = train_set$diagnosis
) %>%
  ggplot(aes(PC1, PC2, color = Diagnosis)) +
  geom_point() +
  stat_ellipse() +
  xlab(paste("PC1: ", percent(pca.var.per[1],0.1))) +
  ylab(paste("PC2: ", percent(pca.var.per[2],0.1))) +
  scale_color_discrete(
    name = "Diagnosis",
    breaks = c("B", "M"),
    labels = c("Benign", "Malignant")
  ) +
  theme(legend.position = "top")




################################################################################
## Model building & evaluation
################################################################################

#####################################
## Model performance 
## metrics definitions
#####################################

metrics_definitions <- tibble(
  Metric = c(
    "True Positive (TP)",
    "True Negative (TN)",
    "False Positive (FP)",
    "False Negative (FN)",
    "Accuracy",
    "Sensitivity",
    "Specificity",
    "False Positive Rate (FPR)",
    "False Negative Rate (FNR)",
    "F1 Score (F1)"
  ),
  Definition = c(
    "Prediction correctly indicates the presence of a condition or characteristic",
    "Prediction correctly indicates the absence of a condition or characteristic",
    "Prediction wrongly indicates that a particular condition or attribute is present",
    "Prediction wrongly indicates that a particular condition or attribute is absent",
    "Overall Accuracy, ratio of number of correct predictions to the total number of input samples",
    "True Positive Rate, proportion of Positive data points that are correctly considered as Positive, with respect to all Positive data points",
    "True Negative Rate, proportion of Negative data points that are correctly considered as Negative, with respect to all Negative data points",
    "Ratio between the number of Negative events wrongly categorized as Positive and the total number of actual Negative events",
    "Ratio between the number of Postive events wrongly categorized as Negative and the total number of actual Positive events",
    "Harmonic mean of precision (or positive predictive value) and Sensitivity"
  )
)



#####################################
## Results data frame
#####################################

results <- tibble(
  Model = character(),
  ModelType = character(),
  Accuracy = double(),
  Sensitivity = double(),
  Specificity = double(),
  F1 = double(),
  FNR = double(),
  FPR = double()
)



#####################################
## Train control parameters to 
## apply 10-fold cross-validation
#####################################

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,                  ## 10-fold cross-validation
  repeats = 10,                 ## repeat each cross-validation 10 times
  classProbs = TRUE,            ## class probabilities computed
  returnResamp = "final",       ## only save the final resampled summary metrics
  savePredictions = "final"     ## only save the final predictions for each resample
) 



################################################################################
## Random sampling
################################################################################



#####################################
## Simple Random Sample
#####################################

## Equiprobability between the two outcomes (M and B)
p <- 0.5

## Set seed for reproductibility
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(201)                         ## if using R 3.5 or earlier, use `set.seed(201)`
} else {
  print("not using R 3.5 or earlier")
  set.seed(201, sample.kind="Rounding") ## else, `set.seed(201, sample.kind="Rounding")`
}


## Predict outcome for the test data
simple_random_predictions <- sample(c("B", "M"), nrow(data$test_x), prob = c(1 - p, p), replace = TRUE) %>%
  factor(levels = levels(data$test_y))

## Confusion matrix
simple_random_results <- confusionMatrix(simple_random_predictions, data$test_y, positive = "M")

## Save model results
results <- results %>%
  bind_rows(tibble(
    Model = "Simple Random Sample",
    ModelType = "Random Sampling",
    Accuracy = round(simple_random_results$overall["Accuracy"], 3),
    Sensitivity = round(simple_random_results$byClass["Sensitivity"], 3),
    Specificity = round(simple_random_results$byClass["Specificity"], 3),
    F1 = round(simple_random_results$byClass["F1"], 3),
    FNR = round(1 - simple_random_results$byClass["Sensitivity"], 3),
    FPR = round(1 - simple_random_results$byClass["Specificity"], 3)
  ))
  



#####################################
## Weighed Random sampling
#####################################

## Assign the prevalence of benign (B) and malignant (M), the probabilities of being sampled for B and M, respectively.
p <- mean(data$train_y == "M")


## Set seed for reproductibility
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(201)                         ## if using R 3.5 or earlier, use `set.seed(201)`
} else {
  print("not using R 3.5 or earlier")
  set.seed(201, sample.kind="Rounding") ## else, `set.seed(201, sample.kind="Rounding")`
}

## Predict outcome for the test data
weighted_random_predictions <- sample(c("B", "M"), nrow(data$test_x), prob = c(1 - p, p), replace = TRUE) %>%
  factor(levels = levels(data$test_y))

## Confusion matrix
weighted_random_results <- confusionMatrix(weighted_random_predictions, data$test_y, positive = "M")

## Save model results
results <- results %>%
  bind_rows(tibble(
    Model = "Weighted Random Sample",
    ModelType = "Random Sampling",
    Accuracy = round(weighted_random_results$overall["Accuracy"], 3),
    Sensitivity = round(weighted_random_results$byClass["Sensitivity"], 3),
    Specificity = round(weighted_random_results$byClass["Specificity"], 3),
    F1 = round(weighted_random_results$byClass["F1"], 3),
    FNR = round(1 - weighted_random_results$byClass["Sensitivity"], 3),
    FPR = round(1 - weighted_random_results$byClass["Specificity"], 3)
  ))



################################################################################
## Unsupervided Learning
################################################################################



#####################################
## K-Means Clustering
#####################################

## k-means prediction function
predict.kmeans <- function(kmeans_model, x_data) {
  ## Cluster centers
  centers <- kmeans_model$centers
  ## Number of centers
  n_centers <- nrow(centers)
  ## Distance from data-points to the cluster centers
  distances_matrix <- as.matrix(dist(rbind(centers, x_data)))
  distances_matrix <- distances_matrix[-seq(n_centers), seq(n_centers)]
  ## Select cluster with min distance to center
  return(max.col(-distances_matrix))
}

## Elbow method to find the optimal number of clusters
wcss = tibble()
for(i in 1:20) {
  wcss <- wcss %>%
    bind_rows(tibble(
      `Number of clusters` = i,
      WCSS = sum(kmeans(data$train_x_cs, centers = i, nstart = 25)$withinss)
    ))
}

wcss %>%
  ggplot(aes(x = `Number of clusters`, y = WCSS)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1)) +
  geom_vline(
    xintercept = 2,
    linetype = 2, 
    colour = "#FF0000"
  ) +
  xlab("Number of clusters k") +
  ylab("Total within-clusters sum of squares") -> elbow_method_plot


## Set seed for reproductibility
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(202)                         ## if using R 3.5 or earlier, use `set.seed(202)`
} else {
  print("not using R 3.5 or earlier")
  set.seed(202, sample.kind="Rounding") ## else, `set.seed(202, sample.kind="Rounding")`
}


## Model : train using normalized train dataset
kmeans <- kmeans(data$train_x_cs, centers = 2, nstart = 25)

## Predict outcomes for normalized test dataset
kmeans_predictions <- ifelse(predict.kmeans(x_data = data$test_x_cs, kmeans_model = kmeans) == 1, "M", "B") %>%
  factor(levels = levels(data$test_y))

## Confusion matrix
kmeans_results <- confusionMatrix(kmeans_predictions, data$test_y, positive = "M")

## Save model results
results <- results %>%
  bind_rows(tibble(
    Model = "K-Means Clustering",
    ModelType = "Unsupervised Learning",
    Accuracy = round(kmeans_results$overall["Accuracy"], 3),
    Sensitivity = round(kmeans_results$byClass["Sensitivity"], 3),
    Specificity = round(kmeans_results$byClass["Specificity"], 3),
    F1 = round(kmeans_results$byClass["F1"], 3),
    FNR = round(1 - kmeans_results$byClass["Sensitivity"], 3),
    FPR = round(1 - kmeans_results$byClass["Specificity"],3)
  ))



#####################################
## K-Means Clustering without 
## highly correlated features (_whcf))
#####################################

## Set seed for reproductibility
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(202)                         ## if using R 3.5 or earlier, use `set.seed(202)`
} else {
  print("not using R 3.5 or earlier")
  set.seed(202, sample.kind="Rounding") ## else, `set.seed(202, sample.kind="Rounding")`
}


## Model : train using normalized train dataset
kmeans_whcf <- kmeans(data$train_x_cs[,-c(corr_index)], centers = 2, nstart = 25)

## Predict outcomes for normalized test dataset 
kmeans_whcf_predictions <- ifelse(predict.kmeans(x_data = data$test_x_cs[,-c(corr_index)], kmeans_model = kmeans_whcf) == 1, "M", "B") %>%
  factor(levels = levels(data$test_y))

## Confusion matrix
kmeans_whcf_results <- confusionMatrix(kmeans_whcf_predictions, data$test_y, positive = "M")

## Save model results
results <- results %>%
  bind_rows(tibble(
    Model = "K-Means Clustering Without Highly Correlated Features",
    ModelType = "Unsupervised Learning",
    Accuracy = round(kmeans_whcf_results$overall["Accuracy"], 3),
    Sensitivity = round(kmeans_whcf_results$byClass["Sensitivity"], 3),
    Specificity = round(kmeans_whcf_results$byClass["Specificity"], 3),
    F1 = round(kmeans_whcf_results$byClass["F1"], 3),
    FNR = round(1 - kmeans_whcf_results$byClass["Sensitivity"], 3),
    FPR = round(1 - kmeans_whcf_results$byClass["Specificity"], 3)
  ))



################################################################################
## Supervised Leaning | Generative Models
################################################################################

#####################################
## Naive Bayes Model
#####################################

## Set seed for reproductibility
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(203)                         ## if using R 3.5 or earlier, use `set.seed(203)`
} else {
  print("not using R 3.5 or earlier")
  set.seed(203, sample.kind="Rounding") ## else, `set.seed(203, sample.kind="Rounding")`
}

## Model : train using normalized train dataset
naive_bayes <- train(
  x = data$train_x_cs, 
  y = data$train_y, 
  method = "nb",
  tuneGrid = expand.grid(usekernel = c(FALSE, TRUE), fL = c(0, 1), adjust = c(0, 1)),
  trControl = fitControl
)

## Predict outcomes for normalized test dataset
naive_bayes_predictions <- predict(naive_bayes, data$test_x_cs)

## Confusion matrix
naive_bayes_results <- confusionMatrix(naive_bayes_predictions, data$test_y, positive = "M")

## Save model results
results <- results %>%
  bind_rows(tibble(
    Model = "Naive Bayes",
    ModelType = "Supervised Learming (Generative Models)",
    Accuracy = round(naive_bayes_results$overall["Accuracy"], 3),
    Sensitivity = round(naive_bayes_results$byClass["Sensitivity"], 3),
    Specificity = round(naive_bayes_results$byClass["Specificity"], 3),
    F1 = round(naive_bayes_results$byClass["F1"], 3),
    FNR = round(1 - naive_bayes_results$byClass["Sensitivity"], 3),
    FPR = round(1 - naive_bayes_results$byClass["Specificity"], 3)
  ))



#####################################
## Linear Discriminant Analysis 
## Model (LDA) 
#####################################

## Set seed for reproductibility
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(204)                         ## if using R 3.5 or earlier, use `set.seed(204)`
} else {
  print("not using R 3.5 or earlier")
  set.seed(204, sample.kind="Rounding") ## else, `set.seed(204, sample.kind="Rounding")`
}

## Model : train using normalized train dataset
lda <- train(
  x = data$train_x_cs, 
  y = data$train_y, 
  method = "lda", 
  trControl = fitControl
)

## Predict outcomes for normalized test dataset
lda_predictions <- predict(lda, data$test_x_cs)

## Confusion Matrix
lda_results <- confusionMatrix(lda_predictions, data$test_y, positive = "M")

## Save model results
results <- results %>%
  bind_rows(tibble(
    Model = "Linear Discriminant Analysis",
    ModelType = "Supervised Learming (Generative Models)",
    Accuracy = round(lda_results$overall["Accuracy"], 3),
    Sensitivity = round(lda_results$byClass["Sensitivity"], 3),
    Specificity = round(lda_results$byClass["Specificity"], 3),
    F1 = round(lda_results$byClass["F1"], 3),
    FNR = round(1 - lda_results$byClass["Sensitivity"], 3),
    FPR = round(1 - lda_results$byClass["Specificity"], 3)
  ))


#####################################
## Quadartic Discriminant Analysis 
## Model (QDA)
#####################################

## Set seed for reproductibility
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(205)                         ## if using R 3.5 or earlier, use `set.seed(205)`
} else {
  print("not using R 3.5 or earlier")
  set.seed(205, sample.kind="Rounding") ## else, `set.seed(205, sample.kind="Rounding")`
}

## Model : train using normalized train dataset
qda <- train(
  x = data$train_x_cs,
  y = data$train_y, 
  method = "qda", 
  trControl = fitControl
)

## Predict outcomes for normalized test dataset
qda_predictions <- predict(qda, data$test_x_cs)

## Confusion Matrix
qda_results <- confusionMatrix(qda_predictions, data$test_y, positive = "M")

## Save model results
results <- results %>%
  bind_rows(tibble(
    Model = "Quadratic Discriminant Analysis",
    ModelType = "Supervised Learming (Generative Models)",
    Accuracy = round(qda_results$overall["Accuracy"], 3),
    Sensitivity = round(qda_results$byClass["Sensitivity"], 3),
    Specificity = round(qda_results$byClass["Specificity"], 3),
    F1 = round(qda_results$byClass["F1"], 3),
    FNR = round(1 - qda_results$byClass["Sensitivity"], 3),
    FPR = round(1 - qda_results$byClass["Specificity"], 3)
  ))


#####################################
## Quadartic Discriminant Analysis 
## Model (QDA) with PCA
#####################################

## Set seed for reproductibility
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(205)                         ## if using R 3.5 or earlier, use `set.seed(205)`
} else {
  print("not using R 3.5 or earlier")
  set.seed(205, sample.kind="Rounding") ## else, `set.seed(205, sample.kind="Rounding")`
}

## Model : train using non-normalized train dataset
qda_pca <- train(
  x = data$train_x, 
  y = data$train_y,
  method = "qda",
  trControl = fitControl,
  # Pre-processing function to centre, scale and apply pca
  preProcess = c("center", "scale", "pca")
)

## Predict outcomes for non-normalized test dataset
qda_pca_predictions <- predict(qda_pca, data$test_x)

## Confusion Matrix
qda_pca_results <- confusionMatrix(qda_pca_predictions, data$test_y, positive = "M")


## Save model results
results <- results %>%
  bind_rows(tibble(
    Model = "Quadratic Discriminant Analysis with PCA",
    ModelType = "Supervised Learming (Generative Models)",
    Accuracy = round(qda_pca_results$overall["Accuracy"], 3),
    Sensitivity = round(qda_pca_results$byClass["Sensitivity"], 3),
    Specificity = round(qda_pca_results$byClass["Specificity"], 3),
    F1 = round(qda_pca_results$byClass["F1"], 3),
    FNR = round(1 - qda_pca_results$byClass["Sensitivity"], 3),
    FPR = round(1 - qda_pca_results$byClass["Specificity"], 3)
  ))



################################################################################
## Supervised Leaning | Discriminative Models
################################################################################

#####################################
## Logistic Regression Model
#####################################

## Set seed for reproductibility
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(206)                         ## if using R 3.5 or earlier, use `set.seed(206)`
} else {
  print("not using R 3.5 or earlier")
  set.seed(206, sample.kind="Rounding") ## else, `set.seed(206, sample.kind="Rounding")`
}

## Model : train using normalized train dataset
glm <- train(
  x = data$train_x_cs, 
  y = data$train_y, 
  method = "glm", 
  trControl = fitControl
)

## Predict outcomes for normalized test dataset
glm_predictions <- predict(glm, data$test_x_cs)

## Confusion Matrix
glm_results <- confusionMatrix(glm_predictions, data$test_y, positive = "M")

## Save model results
results <- results %>%
  bind_rows(tibble(
    Model = "Logistic Regression",
    ModelType = "Supervised Learming (Discriminative Models)",
    Accuracy = round(glm_results$overall["Accuracy"], 3),
    Sensitivity = round(glm_results$byClass["Sensitivity"], 3),
    Specificity = round(glm_results$byClass["Specificity"], 3),
    F1 = round(glm_results$byClass["F1"], 3),
    FNR = round(1 - glm_results$byClass["Sensitivity"], 3),
    FPR = round(1 - glm_results$byClass["Specificity"], 3)
  ))


#####################################
## Logistic Regression 
## with PCA
#####################################

## Set seed for reproductibility
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(206)                         ## if using R 3.5 or earlier, use `set.seed(206)`
} else {
  print("not using R 3.5 or earlier")
  set.seed(206, sample.kind="Rounding") ## else, `set.seed(206, sample.kind="Rounding")`
}

## Model : train using non-normalized train dataset
glm_pca <- train(
  x = data$train_x, 
  y = data$train_y,
  method = "glm",
  trControl = fitControl,
  # Pre-processing function to centre, scale and apply pca
  preProcess = c("center", "scale", "pca")
)

## Predict outcomes for non-normalized test dataset
glm_pca_predictions <- predict(glm_pca, data$test_x)

## Confusion Matrix
glm_pca_results <- confusionMatrix(glm_pca_predictions, data$test_y, positive = "M")

## Save model results
results <- results %>%
  bind_rows(tibble(
    Model = "Logistic Regression With PCA",
    ModelType = "Supervised Learming (Discriminative Models)",
    Accuracy = round(glm_pca_results$overall["Accuracy"], 3),
    Sensitivity = round(glm_pca_results$byClass["Sensitivity"], 3),
    Specificity = round(glm_pca_results$byClass["Specificity"], 3),
    F1 = round(glm_pca_results$byClass["F1"], 3),
    FNR = round(1 - glm_pca_results$byClass["Sensitivity"], 3),
    FPR = round(1 - glm_pca_results$byClass["Specificity"], 3)
  ))


#####################################
## K-Nearest Beighbour Model (KNN)
#####################################

## Set seed for reproductibility
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(207)                         ## if using R 3.5 or earlier, use `set.seed(207)`
} else {
  print("not using R 3.5 or earlier")
  set.seed(207, sample.kind="Rounding") ## else, `set.seed(207, sample.kind="Rounding")`
}

## Model : train using normalized train dataset
knn <- train(
  x = data$train_x_cs, 
  y = data$train_y,
  method = "knn",
  tuneGrid = data.frame(k = seq(from = 1, to = 30, by = 1)),
  trControl = fitControl
)

## Predict outcomes for normalized test dataset
knn_predictions <- predict(knn, data$test_x_cs)

## Confusion Matrix
knn_results <- confusionMatrix(knn_predictions, data$test_y, positive = "M")

## Save model results
results <- results %>%
  bind_rows(tibble(
    Model = "K-Nearest Neighbors",
    ModelType = "Supervised Learming (Discriminative Models)",
    Accuracy = round(knn_results$overall["Accuracy"], 3),
    Sensitivity = round(knn_results$byClass["Sensitivity"], 3),
    Specificity = round(knn_results$byClass["Specificity"], 3),
    F1 = round(knn_results$byClass["F1"], 3),
    FNR = round(1 - knn_results$byClass["Sensitivity"], 3),
    FPR = round(1 - knn_results$byClass["Specificity"], 3)
  ))


#####################################
## Random Forest Model (RF)
#####################################

## Set seed for reproductibility
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(208)                         ## if using R 3.5 or earlier, use `set.seed(208)`
} else {
  print("not using R 3.5 or earlier")
  set.seed(208, sample.kind="Rounding") ## else, `set.seed(208, sample.kind="Rounding")`
}

## Model : train using normalized train dataset
rf <- train(
  x = data$train_x_cs,
  y = data$train_y,
  method = "rf",
  tuneGrid = data.frame(mtry = seq(3, 15, 2)),
  importance = TRUE,
  trControl = fitControl
)

## Predict outcomes for normalized test dataset
rf_predictions <- predict(rf, data$test_x_cs)

## Confusion Matrix
rf_results <- confusionMatrix(rf_predictions, data$test_y , positive = "M")

## Save model results
results <- results %>%
  bind_rows(tibble(
    Model = "Random Forest",
    ModelType = "Supervised Learming (Discriminative Models)",
    Accuracy = round(rf_results$overall["Accuracy"], 3),
    Sensitivity = round(rf_results$byClass["Sensitivity"], 3),
    Specificity = round(rf_results$byClass["Specificity"], 3),
    F1 = round(rf_results$byClass["F1"], 3),
    FNR = round(1 - rf_results$byClass["Sensitivity"], 3),
    FPR = round(1 - rf_results$byClass["Specificity"], 3)
  ))



#####################################
## Neural Networks
#####################################

## Set seed for reproductibility
if((as.integer(R.version$major) < 3) | ((as.integer(R.version$major) == 3) & (floor(as.numeric(R.version$minor)) <= 5))) {
  print("using R 3.5 or earlier")
  set.seed(209)                         ## if using R 3.5 or earlier, use `set.seed(209)`
} else {
  print("not using R 3.5 or earlier")
  set.seed(209, sample.kind="Rounding") ## else, `set.seed(209, sample.kind="Rounding")`
}

## Model : train using normalized train dataset
nnet <- train(
  x = data$train_x_cs,
  y = data$train_y,
  method = "nnet",
  trace = FALSE,
  trControl = fitControl
)

## Predict outcomes for normalized test dataset
nnet_predictions <- predict(nnet, data$test_x_cs)

## Confusion Matrix
nnet_results <- confusionMatrix(nnet_predictions, data$test_y, positive = "M")

## Save model results
results <- results %>%
  bind_rows(tibble(
    Model = "Neural Networks",
    ModelType = "Supervised Learming (Discriminative Models)",
    Accuracy = round(nnet_results$overall["Accuracy"], 3),
    Sensitivity = round(nnet_results$byClass["Sensitivity"], 3),
    Specificity = round(nnet_results$byClass["Specificity"], 3),
    F1 = round(nnet_results$byClass["F1"], 3),
    FNR = round(1 - nnet_results$byClass["Sensitivity"], 3),
    FPR = round(1 - nnet_results$byClass["Specificity"], 3)
  ))



################################################################################
## RESULTS
################################################################################


#####################################
## Key performance metrics Table
#####################################

results <- results %>%
  mutate(ModelType = factor(
    case_when(
      ModelType == "Random Sampling" ~ 1,
      ModelType == "Unsupervised Learning" ~ 2,
      ModelType == "Supervised Learming (Generative Models)" ~ 3,
      ModelType == "Supervised Learming (Discriminative Models)" ~ 4,
      TRUE ~ 5
    ),
    levels = 1:4,
    labels = c(
      "Random Sampling",
      "Unsupervised Learning",
      "Supervised Learming (Generative Models)",
      "Supervised Learming (Discriminative Models)"
    ))
  )


results %>%
  dplyr::select(-c(ModelType)) %>%
  mutate(
    FNR = scales::percent(FNR, accuracy = 0.1),
    FPR = scales::percent(FPR, accuracy = 0.1)
  ) %>%
  kable(
    caption = "Key performance metrics per model",
    align = 'lrrrrrr',
    booktabs = T,
    format = "latex",
    linesep = ""
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    latex_options = c("scale_down", "hold_position")
  ) %>%
  pack_rows(index = table(results$ModelType )) -> results_table




#####################################
## Tuning
#####################################

## Naive Bayes Model
## Plot accuracy during cross-validation for tuning 
## laplace smoothing (0/1), usekernel (true/false) and adjust (0/1)
x_validation_tuning_naive_bayes_plot <- plot(
  naive_bayes,
  ylab = "Accuracy (repeated cross-validation)",
  xlab = "Laplace correction"
)

## k Nearest Neigbours Model
## Plot accuracy during cross-validation for each value of 
## k (number of neighbours) tuned
x_validation_tuning_knn_plot <- plot(
  knn,
  ylab = "Accuracy (repeated cross-validation)",
  xlab = "Number of Neighbours (k)"
)

## Tuning Random Forest Model
## Plot accuracy during cross-validation for each value of 
## mtry number of randomly selected predictors included in each decision tree
x_validation_tuning_rf_plot <- plot(
  rf,
  ylab = "Accuracy (repeated cross-validation)",
  xlab = "Number of Randomly selected predictors (mtry)"
)



#####################################
## Conclusion
#####################################

results_ranked <- results %>% 
  arrange(desc(Accuracy), desc(F1), FNR) %>%
  mutate(rank = row_number())

number_1_model <- results_ranked %>% filter(rank == 1) %>% dplyr::select(Model) %>% pull()
number_2_model <- results_ranked %>% filter(rank == 2) %>% dplyr::select(Model) %>% pull()


################################################################################
## ....
################################################################################
