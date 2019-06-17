
library(tidyverse)
library(caret)

#Loading data
edx <- readRDS("~/Documents/Data science course/movielens_data/edx.rds")
validation <- readRDS("~/Documents/Data science course/movielens_data/validation.rds")

#Checking for missing values in the variables used for this analysis.

sum(is.na(edx$userId))
sum(is.na(edx$movieId))
sum(is.na(edx$rating))

#Data exploration
str(edx)
head(edx) %>% knitr::kable()

edx %>% count(rating) %>% arrange(desc(n)) %>% knitr::kable()

edx_movieId_unique <- unique(edx$movieId)
length(edx_movieId_unique)

edx_users_unique <- unique(edx$userId)
length(edx_users_unique)

movie_ratings <- edx %>% group_by(title) %>% summarize(n_rating = length(rating))
movie_ratings %>% arrange(desc(n_rating)) %>% head() %>% knitr::kable() 

movie_rating <- edx %>% group_by(title) %>% filter(n() >= 100) %>% summarize(avg_rating = mean(rating)) 
movie_rating %>% arrange(desc(avg_rating)) %>% head() %>% knitr::kable() 

#A model that assumes the same rating for all movies 
#and users with all the differences explained by random variation.

mu_hat <- mean(edx$rating)
mu_hat

naive_rmse <- RMSE(edx$rating, mu_hat)
naive_rmse

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse) 

#A model that takes into account that some movies 
#are just generally rated higher than others.

mu <- mean(edx$rating)
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + edx %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, edx$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect Model",
                                     RMSE = model_1_rmse))
rmse_results %>% knitr::kable()

#A model that takes into account variability 
#in rating behavior accross users.

edx %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>%
  filter(n()>=100) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")

user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, edx$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User Effects Model",
                                     RMSE = model_2_rmse))
rmse_results %>% knitr::kable()

#Regularization of estimate b: when sample size is very large, a case which 
#will give us a stable estimate, then the penalty term labda 
#is effectively ignored. When the sample size is small, 
#then the estimate b is shrunken towards 0.
#The larger labda, the more we shrink.
#Labda and bs are determined based on cross-validation.

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <-
    edx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, edx$rating))
})

qplot(lambdas, rmses)

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized Movie + User Effect Model",
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

#Testing best-performing model based on validation data.

mu <- mean(edx$rating)
b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))
b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
predicted_ratings <-
    validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
rmse_final <- RMSE(predicted_ratings, validation$rating)
rmse_final

