library(caret)
library(tidyverse)

data <- read.table("~/Desktop/Temp/Data_git/zip/zip.train.gz") %>% 
  set_names(c("y", paste0("V", 1:256))) %>% 
  arrange(y)

data_test <- read.table("~/Desktop/Temp/Data_git/zip/zip.test.gz") %>% 
  set_names(c("y", paste0("V", 1:256)))
  
plot_figure2 <- function(vec){
  assertthat::assert_that(length(vec) == 256)
  vec = as.numeric(vec)
  image(matrix(vec, nrow = 16, byrow = T), col = grey(seq(1, 0, length = 256)))
}

plot_figure_aux <- function(vec){
  gg <- matrix(vec, nrow = 16, byrow = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    gather(x, value, V1:V16) %>% 
    mutate(x = as.integer(stringi::stri_extract_first_regex(x, "[0-9]+")),
           y = -as.integer(rowname),
           value = as.numeric(value)) %>% 
    ggplot() +
    geom_raster(aes(x, y, fill = value)) +
    scale_fill_gradient2(low = "black", mid='gray80', high = "white") + 
    coord_fixed() +
    theme_classic()
  return(gg)
}

plot_figure <- function(data, i){
  return(plot_figure_aux(data[i,2:257]))
}

plot_figure(data, 1)
plot_figure(data, 1200)
plot_figure(data, 2588)


unique_y <- unique(data$y)
svd_data <- lapply(unique_y, function(x){
  data_subset <- data %>% 
    filter(y == x) %>% 
    select(-y) %>% 
    as.matrix() %>% 
    t()
  svd_i <- irlba::irlba(data_subset, nv = 40)
  return(svd_i$u)
})
names(svd_data) <- paste0("y_", unique_y)


plot_figure_aux(svd_data$y_0[,1])
plot_figure_aux(svd_data$y_0[,2])
plot_figure_aux(svd_data$y_0[,3])

plot_figure_aux(svd_data$y_1[,1])
plot_figure_aux(svd_data$y_1[,2])

plot_figure_aux(svd_data$y_2[,1])
plot_figure_aux(svd_data$y_2[,3])

plot_figure_aux(svd_data$y_3[,1])

plot_figure_aux(svd_data$y_8[,1])



predict_svd <- function(new_images, Us, k = "all"){
  # new_images: matrix in which every column is a stacked image
  # Us: list of SVD matrices
  # k: number of singular images to use
  
  if(k == "all") k = ncol(Us[[1]])
  
  # Matrix of the norms if the residuals for each image. Each row is an image to be classified.
  norm_resids <- as.data.frame(matrix(rep(0.0, length(Us)*ncol(new_images)), nrow = ncol(new_images)))
  names(norm_resids) = names(Us)
  for(i in seq_along(Us)){
    Ui <- Us[[i]][,1:k]
    Utz <- t(Ui) %*% new_images
    res = new_images - (Ui %*% Utz)
    norm_resids[,i] = slam::col_norms(res)
  }
  pred_classes = colnames(norm_resids)[max.col(-norm_resids,ties.method="first")]
  
  out <- norm_resids %>% 
    mutate(sum = rowSums(.)) %>% 
    mutate_all(funs(1 - ./sum)) %>% 
    select(-sum) %>% 
    mutate(pred_class = pred_classes)
  
  return(out)
}

predictions_data <- predict_svd(t(data_test[,2:257]),
                                svd_data) %>% 
  mutate(real_class = paste0("y_", data_test$y))

caret::confusionMatrix(predictions_data$pred_class, predictions_data$real_class)


resultds_preds <- lapply(1:40, function(i){
  predictions <- predict_svd(t(data_test[,2:257]),
                             svd_data, 
                             i) %>% 
    mutate(real_class = paste0("y_", data_test$y))
  cm <- confusionMatrix(predictions$pred_class, predictions$real_class)
  out <- cm$overall %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(k = i)
  return(out)
}) %>% 
  bind_rows()

resultds_preds %>% 
  ggplot(aes(k, Accuracy)) +
  geom_ribbon(aes(ymin = AccuracyLower,
                  ymax = AccuracyUpper),
              alpha = 0.2) +
  geom_line() +
  geom_point() +
  theme_bw()

resultds_preds %>% 
  ggplot(aes(k, Kappa)) +
  geom_line() +
  geom_point() +
  theme_bw()


