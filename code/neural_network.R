library(tidyverse)
library(nnet)
library(dplyr)
library(plyr)
library(readr)
library(caret)
library(ggplot2)
library(devtools)
library(ggimage)
source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')



###############################################################################
#Reading in the data
###############################################################################

dv<-read_csv('./data/cleaned_baseball_weather.csv')
dv$total_walks<-dv$v_walks+dv$h_walks
dv$total_error<-dv$v_errors+dv$h_errors
dv$total_runs<-dv$h_score+dv$v_score
dv$total_strikeouts<-dv$h_strikeouts+dv$v_strikeouts
dv$total_wildpitches<-dv$v_wild_pitches+dv$h_wild_pitches
dv$total_hits<-dv$h_hits+dv$v_hits
dv$penalized_strikes<-dv$total_strikeouts-dv$total_walks
###############################################################################
#Editing data to get only combined variables with weather
###############################################################################

dv1<-dv[,c(9,10,39:75)]
str(dv1)
dv1<-drop_na(dv1)
dv1<-na.omit(dv1)
dv1<-dv1[,-c(10, 18, 25, 27)]
#Removes columnns that are all 0's
###############################################################################
#Normalizing the data
###############################################################################
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
dv1<- as.data.frame(lapply(dv1[1:35], min_max_norm))
###############################################################################
#Creating test train split for pitching stats
###############################################################################
dv_pitching<-dv1

dv_pitching<-dv_pitching[,c(1:28,35)]

set.seed(1)

dv_pitching_split <- sample(1:nrow(dv_pitching), floor(0.7*nrow(dv_pitching)))
dv_pitching_train <- dv_pitching[dv_pitching_split, ]
dv_pitching_test <- dv_pitching[-dv_pitching_split, ]
###############################################################################
#Setting up model and evaluating with rmse
###############################################################################
net1 <- nnet(penalized_strikes ~., data = dv_pitching_train, 
             size = 20, decay = .001, maxit = 2000, linout = F)
pred <- predict(net1, newdata = dv_pitching_test, type = "r")


RMSE = function(test_data,predictions){
  result <- sqrt(mean((test_data - predictions)^2))
  return(result)
}

RMSE(dv_pitching_test$penalized_strikes,pred)
###############################################################################
#Setting up model and evaluating with caret to use corss val and grid search 
###############################################################################
controlList <- trainControl(method = "cv", number = 5, search = "grid")
tuneMatrix <- expand.grid(size = seq(from = 5, to = 30, by = 5),
                          decay = seq(from = 0.01, to = 0.1, by = 0.01))

set.seed(1)
caret_net <- train(penalized_strikes ~.,
                   data = dv_pitching_train,
                   method = "nnet",
                   linout = FALSE,
                   TRACE = FALSE,
                   maxit = 100,
                   tuneGrid = tuneMatrix,
                   trControl = controlList)

pred1 <- predict(caret_net, newdata = dv_pitching_test)

RMSE(dv_pitching_test$penalized_strikes,pred1)

plot(x=pred1, y= dv_pitching_test$penalized_strikes,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(a=0, b=1)

#Caret package code inspired from
#https://machinelearningmastery.com/tuning-machine-learning-models-using-the-caret-r-package/
#https://www.projectpro.io/recipes/tune-hyper-parameters-grid-search-r
#https://stackoverflow.com/questions/48425452/get-carets-model-parameters-for-nnet

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

#plot each model
plot.nnet(caret_net$finalModel, cex.val = .75)
###############################################################################
#Repeating for total number of hits
###############################################################################
dv_hits<-dv1

dv_hits<-dv_hits[,c(1:28,34)]

set.seed(1)

dv_hits_split <- sample(1:nrow(dv_hits), floor(0.7*nrow(dv_hits)))
dv_hits_train <- dv_hits[dv_hits_split, ]
dv_hits_test <- dv_hits[-dv_hits_split, ]




caret_net1 <- train(total_hits ~.,
                   data = dv_hits_train,
                   method = "nnet",
                   linout = FALSE,
                   TRACE = FALSE,
                   maxit = 100,
                   tuneGrid = tuneMatrix,
                   trControl = controlList)

pred2 <- predict(caret_net1, newdata = dv_hits_test)

RMSE(dv_hits_test$total_hits,pred2)

plot(x=pred2, y= dv_hits_test$total_hits,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(a=0, b=1)
result<-data.frame(dv_hits_test$total_hits,pred2)
ggplot(result,aes(x=pred2,y=dv_hits_test.total_hits))+
  geom_point()+
  geom_abline(slope=1,intercept = 0, color='red')+
  labs(x='Predicted Value of Normalized Hits',y='Actual Value of Normalized Hits',
       title = 'Neural Network Results VS Actual Results')+
  theme_minimal()
plot.nnet(caret_net1$finalModel, cex.val = .75)
###############################################################################
#Repeating for total number of errors
###############################################################################

dv_errors<-dv1

dv_errors<-dv_errors[,c(1:28,30)]

set.seed(1)

dv_errors_split <- sample(1:nrow(dv_errors), floor(0.7*nrow(dv_errors)))
dv_errors_train <- dv_errors[dv_errors_split, ]
dv_errors_test <- dv_errors[-dv_errors_split, ]




caret_net2 <- train(total_error ~.,
                    data = dv_errors_train,
                    method = "nnet",
                    linout = FALSE,
                    TRACE = FALSE,
                    maxit = 100,
                    tuneGrid = tuneMatrix,
                    trControl = controlList)

pred3 <- predict(caret_net2, newdata = dv_errors_test)

RMSE(dv_errors_test$total_error,pred3)

plot(x=pred3, y= dv_errors_test$total_error,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(a=0, b=1)