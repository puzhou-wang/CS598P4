
# ============================================================
# Functions used in implementation of movie recommendation.
# ============================================================

# load libraries
library(data.table)
library(dplyr)
library(recommenderlab)

# genre-based recommender
genreRecommender <- function(movies, ratings, user_genre){
  # combine rating information into movies
  mr = ratings %>% 
    group_by(MovieID) %>% 
    summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
    inner_join(movies, by = 'MovieID')
  
  # movies from selected genre
  mr_sel = mr[grep(user_genre, mr$Genres),]
  
  # recommender based on popularity
  recom_pop = arrange(mr_sel, desc(ratings_per_movie))
  
  recom_pop
}