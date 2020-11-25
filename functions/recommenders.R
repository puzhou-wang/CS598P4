
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


# UBCF recommender
rateRecommender <- function(ratings, user_ratings){
  # fake UserID and Timestamp
  user_id = max(ratings$UserID) + 1
  user_ratings$UserID = user_id
  user_ratings$Timestamp = 8824
  
  # combine user ratings into the rating dataframe
  cr = rbind(ratings, as.data.frame(user_ratings))
  i = paste0('u', cr$UserID)
  j = paste0('m', cr$MovieID)
  x = cr$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rrmat = new('realRatingMatrix', data = Rmat)
  
  # find out the row number of user data
  user_row = which(dimnames(Rmat)[[1]] %in% c(paste0('u', user_id)))
  
  # train the UBCF model
  set.seed(8824)
  recom_UBCF = Recommender(Rrmat[-user_row,], method='UBCF', parameter=list(normalize='Z-score',method='Cosine',nn=25))
  
  # predict top10 recommendations for user
  recom_res = predict(recom_UBCF, Rrmat[user_row,], type="ratings")
  
  as(recom_res, 'data.frame')
}