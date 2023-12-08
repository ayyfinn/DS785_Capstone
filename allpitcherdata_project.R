#Title: MLB 2023 Pitcher Data
#Author: Sean Finlon
#Date: 12/8/2023


setwd("D:/DS 785/Model")

library(baseballr)
library(dplyr)

library(tree)
library(randomForest)
library(gbm)
library(bst)
library(plyr)
library(ggplot2)
library(ggformula)
library(GGally)
library(caret)
library(stringr)
library(mice)
library(glmnet)
library(nnet)
library(caret)
library(ISLR)
library(NeuralNetTools)
library(tidyverse)


data_list = list() #create empty list

#loop through season in segments to not overload statcast data retrieval
for(i in 1:72){
  
  #opening day: 3/30/2023
  start = as.Date("2023-03-30")+3*(i-1)
  end = as.Date("2023-03-30")+3*i
  
  data_list[[paste(start)]] = statcast_search(start_date = start,
                                              end_date = end,
                                              player_type = 'pitcher')
}

#creates single dataframe from list
data_df = do.call(rbind.data.frame, data_list)

#identifies all unique pitchers in the 2023 MLB Season
all_pitchers = unique(data_df$pitcher)

#outputs dataframe containing all pitcher data from 2023 into csv
#write.csv(data_df,file = "allpitcherdata2023.csv",row.names = FALSE)

#outputs a single vector of the unique pitcher statcast id numbers through the 2023 MLB Season into csv
write.csv(all_pitchers,file = "uniquepitcherlist.csv",row.names = FALSE)


