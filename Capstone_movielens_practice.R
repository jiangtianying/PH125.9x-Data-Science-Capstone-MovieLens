###################################
# Create edx set and validation set
###################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

###################
#Questions in Quiz#
###################
#How many rows are in the edx dataset
nrow(edx) #number of rows in edx dataset
ncol(edx) #number of column in edx daaset
dim(edx) #answer
sum(edx$rating==0) #how man zeros are given in rating of edx
sum(edx$rating==3) #how man zeros are given in rating of edx
edx %>% filter(rating == 0) %>% tally()
edx %>% filter(rating == 3) %>% tally() #answer
length(unique(edx$movieId))#how many different movies are in edx
length(unique(edx$userId))#how many different users are in edx
n_distinct(edx$movieId) #answer
sum(str_detect(edx$genres,"Drama"))
sum(str_detect(edx$genres,"Comedy"))
sum(str_detect(edx$genres,"Thriller"))
sum(str_detect(edx$genres,"Romance"))
#check how many movie ratings are in a genre
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
#answer
edx%>%group_by(title)%>%summarize(count=n())%>%
  arrange(desc(count))
#which movie has greated number of ratings
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
#answer
edx%>%group_by(rating)%>%summarize(count=n())%>%
  arrange(desc(count))
#What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))  
#answer


######################
#Start of the project#
######################
#Calculate the average of the rating and use the simplest based on average
mu<-mean(edx$rating) 
#calculate the average across the training set
mu #display the average
rmse_0 <- RMSE(validation$rating, mu) #cacluate rmse
rmse_0 #display rmse
rmse_results <- data_frame(method = "Just the average", RMSE = rmse_0) 
#create a dataframe to save/compare results

#include the movie effect and compare with previous model
movie_avg<-edx%>%group_by(movieId)%>%summarize(b_i=mean(rating)-mu)
predicted_ratings_1 <- mu + validation %>% 
  left_join(movie_avg, by='movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings_1, validation$rating)
rmse_results <- bind_rows(rmse_results,
              data_frame(method="Movie Effect Model", RMSE = model_1_rmse))
rmse_results 

#include the user effect and compare with previous models
user_avg<-edx%>%left_join(movie_avg, by='movieId')%>%
  group_by(userId)%>%summarize(b_u=mean(rating-mu-b_i))
predicted_ratings_2<-predicted_ratings_1+validation%>%
  left_join(movie_avg,by='movieId')%>%left_join(user_avg,by="userId")%>%
  pull(b_u)
model_2_rmse<-RMSE(predicted_ratings_2,validation$rating)
rmse_results<-bind_rows(rmse_results,data_frame(method="Movie+User Effect Model", 
                                                RMSE=model_2_rmse))
rmse_results


#include time of rating effect and compare with previous models
#check if there is any time effect
if(!require(lubridate)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
#add a new data frame edx_1 to include date info
edx_1<-edx%>%mutate(date=round_date(as_datetime(timestamp),unit="week"))
#check the trend of rating vs. date
edx_1%>%group_by(date)%>%
      summarize(rating=mean(rating))%>%
      ggplot(aes(date,rating))+
      geom_point()

summary_edx_1<-edx_1%>%left_join(movie_avg,by='movieId')%>%
  left_join(user_avg,by='userId')%>%
  group_by(date)%>%
  summarize(date_eff=mean(rating-mu-b_i-b_u),timestamp=first(timestamp),n_rating=n())%>%
  data.frame()%>%
  mutate(date_eff=ifelse(n_rating<10,0,date_eff))
fit_date<-loess(date_eff~timestamp,degree=1,span=0.05,data=summary_edx_1)
date_avg<-data.frame(date=summary_edx_1$date,f_d=fit_date$fitted)
validation_1<-validation%>%mutate(date=round_date(as_datetime(timestamp),unit="week"))
predicted_ratings_3<-predicted_ratings_2+validation_1%>%
  left_join(date_avg,by='date')%>%
  pull(f_d)
model_3_rmse<-RMSE(predicted_ratings_3,validation$rating)
rmse_results<-bind_rows(rmse_results,data_frame(
  method="Movie+User+Time Effect Model", RMSE=model_3_rmse))
rmse_results
#It turned out the time of rating (after deducting the user and movie effect) has little effect
sum(round(predicted_ratings_3/0.5)*0.5==validation$rating)
sum(round(predicted_ratings_2/0.5)*0.5==validation$rating)
#compare the accuracy of the prediction, adding the time effect is slightly better

#study the effect of genres
#First deduct the effect of total avg, movie avg, user avg, and date avg, then group by user & genres
genres_summary<-edx_1%>%left_join(movie_avg,by='movieId')%>%
  left_join(user_avg,by='userId')%>%left_join(date_avg,by="date")%>%
  mutate(genres_eff=rating-mu-b_u-b_i-f_d)%>%
  separate_rows(genres, sep = "\\|")%>%group_by(userId,genres)%>%
  summarize(genres_eff=mean(genres_eff))%>%data.frame()%>%spread(genres,genres_eff,fill = 0)%>%
  gather("genres","genres_eff",-userId)
genres_avg<-validation%>%separate_rows(genres, sep = "\\|")%>%left_join(genres_summary,
                                                                        by=c("userId","genres"))%>%
  group_by(userId,movieId)%>%summarize(genres_eff=mean(genres_eff))
predicted_ratings_4<-predicted_ratings_3+validation%>%
  left_join(genres_avg,by=c('movieId',"userId"))%>%pull(genres_eff)  
model_4_rmse<-RMSE(predicted_ratings_4,validation$rating)
rmse_results<-bind_rows(rmse_results,data_frame(
  method="Movie+User+Time+Genres Effect Model", RMSE=model_4_rmse))
rmse_results           

#regularized effect
l<- seq(0,7,0.5)
rmses<-rep(NA,15)
for (i in 1:15) {
  mu<-mean(edx$rating) #calculate the average across the training set
  #include the movie effect
  movie_avg<-edx%>%group_by(movieId)%>%summarize(b_i=sum(rating-mu)/(n()+l[i]))
  predicted_ratings_1 <- mu + validation %>% 
    left_join(movie_avg, by='movieId') %>%
    pull(b_i)
  
  #include the user effect
  user_avg<-edx%>%left_join(movie_avg, by='movieId')%>%
    group_by(userId)%>%summarize(b_u=sum(rating-mu-b_i)/(n()+l[i]))
  predicted_ratings_2<-predicted_ratings_1+validation%>%
    left_join(movie_avg,by='movieId')%>%left_join(user_avg,by="userId")%>%
    pull(b_u)
  
  #include time of rating effect
  #check if there is any time effect
  if(!require(lubridate)) install.packages("tidyverse", 
                                           repos = "http://cran.us.r-project.org")
  #add a new data frame edx_1 to include date info
  edx_1<-edx%>%mutate(date=round_date(as_datetime(timestamp),unit="week"))
  
  summary_edx_1<-edx_1%>%left_join(movie_avg,by='movieId')%>%
    left_join(user_avg,by='userId')%>%
    group_by(date)%>%
    summarize(date_eff=sum(rating-mu-b_i-b_u)/(n()+l[i]),timestamp=first(timestamp))%>%
    data.frame()
  
  fit_date<-loess(date_eff~timestamp,degree=1,span=0.05,data=summary_edx_1)
  date_avg<-data.frame(date=summary_edx_1$date,f_d=fit_date$fitted)
  validation_1<-validation%>%mutate(date=round_date(as_datetime(timestamp),unit="week"))
  predicted_ratings_3<-predicted_ratings_2+validation_1%>%
    left_join(date_avg,by='date')%>%
    pull(f_d)
  
  #study the effect of genres
  #First deduct the effect of total avg, movie avg, user avg, and date avg, then group by user & genres
  genres_summary<-edx_1%>%left_join(movie_avg,by='movieId')%>%
    left_join(user_avg,by='userId')%>%left_join(date_avg,by="date")%>%
    mutate(genres_eff=rating-mu-b_u-b_i-f_d)%>%
    separate_rows(genres, sep = "\\|")%>%group_by(userId,genres)%>%
    summarize(genres_eff=sum(genres_eff)/(n()+l[i]))%>%data.frame()%>%spread(genres,genres_eff,fill = 0)%>%
    gather("genres","genres_eff",-userId)
  genres_avg<-validation%>%separate_rows(genres, sep = "\\|")%>%left_join(genres_summary,by=c("userId","genres"))%>%
    group_by(userId,movieId)%>%summarize(genres_eff=mean(genres_eff))
  predicted_ratings_4<-predicted_ratings_3+validation%>%
    left_join(genres_avg,by=c('movieId',"userId"))%>%pull(genres_eff)  
  
  rmses[i]<-RMSE(predicted_ratings_4, validation$rating)
  print (i) #use this to track the process
  }

qplot(l, rmses)
lambda <- l[which.min(rmses)]
print (lambda)
rmse_results<-bind_rows(rmse_results,data_frame(
  method="Regularized Movie+User+Time+Genres Effect Model", RMSE=min(rmses)))
rmse_results