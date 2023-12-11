setwd("D:/DS 785/Model") #Set the working directory unique to my computer

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
library(gridExtra)


###############################
#Batter data retrieval
#load in unique batter list from the 2023 MLB Season, create as vector
batters2023 = read.csv("uniquebatterlist.csv")
batters2023 = as.vector(batters2023$x)

get_batter_data = function(names){
  #input: a vector of unique player Statcast ID numbers
  
  #output: a list containing a dataframe of each individual batter's Statcast data for every record from the 2023 MLB Season with the
  #         specified variables, including the log transformation of xwOBA
  
  b_list = list() #create empty list
  
  #Interested variables
  colnames_batmod = c("release_pos_x","release_pos_z","player_name","batter","plate_x",
                      "plate_z","vx0","vy0","vz0","ax","ay","az","release_spin_rate","release_pos_y","spin_axis","estimated_woba_using_speedangle","woba_value")
  
  #gather batting data for each individual player in 2023 season for loop
  for(i in 1:length(names)){
    #baseballr statcast function
    data = statcast_search(start_date = "2023-01-01", 
                           end_date = "2023-11-28", 
                           player_type = 'batter',
                           playerid = names[i])
    
    #selects only specified variables
    data = data %>%
      select(all_of(colnames_batmod))
    
    #removes instances without a measurable woba value
    data = data[is.na(data$woba_value)==FALSE,]
    
    #if a plate appearance ends without a ball in play, the woba value is added to the estimated woba variable
    #Otherwise, keeps the estimated value
    data = data %>% 
      mutate(estimated_woba_using_speedangle = ifelse(is.na(estimated_woba_using_speedangle), woba_value, estimated_woba_using_speedangle))
    
    #remove the woba variable column
    data = subset(data, select = -woba_value)
    
    #removes any incomplete rows
    data = na.omit(data)
    
    #log + 1 transformation of the estimated woba variable
    data$xwoba_log = log(data$estimated_woba_using_speedangle+1)
    
    #removes the non-transformed variable
    data = subset(data, select = -estimated_woba_using_speedangle)
    
    #stores dataframe in specified player's list input
    b_list[[paste(names[i])]] = data
  }
  
  return(b_list)
}

#use function to retrieve individual season batter data for each unique batter that made an appearance in the MLB 2023
batter_data = get_batter_data(batters2023)

#unlist data gathered from batter_data function, store in dataframe
allbatdata = do.call(rbind.data.frame, batter_data)
#remove player_name & batter columns from dataframe to prepare for scaling
allbatdata_scale = subset(allbatdata,select = -c(player_name,batter))
#create vector of means and standard deviations of each batter variable
allbatdata_means = colMeans(allbatdata_scale)
allbatdata_SD = apply(allbatdata_scale,2,sd)


########
#Batter Model Building

#linear model
get_batter_model = function(data,cv,means,sd){
  #input: data list gathered from 'get_batter_data()' function, number of desired cross-validation folds, corresponding dataset variable means matrix,
  #       and corresponding dataset variable standard deviations matrix
  
  #output: a list containing batter models
  m_list = list()
  
  data = keep(data, ~all(nrow(.x) >= 250))
  
  #10-fold cross-validation
  ctrl = trainControl(method = "cv", number = cv)
  
  for(i in 1:length(data)){
    player_name = data[[i]]$player_name[1]
    dataused = data[[i]]
    dataused = dataused %>%
      select(-c("player_name","batter"))
    
    #handedness = dataused$p_throws
    
    #dataused = dataused %>%
    #  select(-c("p_throws"))
    
    data_scale = matrix(NA,nrow=nrow(dataused),ncol=ncol(dataused))
    data_scale = as.data.frame(data_scale)
    colnames(data_scale) = colnames(dataused)
    
    for(i in 1:ncol(dataused)){
      data_scale[,i] = (dataused[,i] - means[i])/sd[i]
    }
    
    dataused = data_scale
    #dataused$p_throws = handedness
    
    model = train(xwoba_log ~ .,
                  data = dataused,
                  method = "glm",
                  trControl = ctrl)
    
    m_list[[paste(player_name)]] = model
  }
  
  return(m_list)
}

bat_mod = get_batter_model(batter_data,10,allbatdata_means,allbatdata_SD)

#### neural network
get_batter_nn_model = function(data,means,sd){
  
  m_list = list()
  
  data = keep(data, ~all(nrow(.x) >= 250))
  
  #10-fold cross-validation
  ctrl = trainControl(method = "cv", number = 10)
  
  for(i in 1:length(data)){
    player_name = data[[i]]$player_name[1]
    dataused = data[[i]]
    dataused = dataused %>%
      select(-c("player_name","batter"))
    
    #handedness = dataused$p_throws
    
    #dataused = dataused %>%
    #  select(-c("p_throws"))
    
    data_scale = matrix(NA,nrow=nrow(dataused),ncol=ncol(dataused))
    data_scale = as.data.frame(data_scale)
    colnames(data_scale) = colnames(dataused)
    
    for(i in 1:ncol(dataused)){
      data_scale[,i] = (dataused[,i] - means[i])/sd[i]
    }
    
    dataused = data_scale
    #dataused$p_throws = handedness
    
    set.seed(10)
    model = train(xwoba_log ~ .,
                  data = dataused,
                  method = "nnet",
                  #tuneGrid = expand.grid(size = seq(2,5,by=1), decay = seq(2,20,by=1)),
                  tuneGrid = expand.grid(size = 2, decay = 11),
                  linout = TRUE,
                  trace = FALSE,
                  #preProc = c("center", "scale"),
                  trControl = ctrl,
                  maxit = 2000)
    
    m_list[[paste(player_name)]] = model
  }
  
  return(m_list)
}

nn_models = get_batter_nn_model(batter_data,allbatdata_means,allbatdata_SD)

################

#pitcher data retrieval
pitchers2023 = read.csv("uniquepitcherlist.csv")
pitchers2023 = as.vector(pitchers2023$x)

get_pitcher_data = function(names){
  
  p_list = list()
  
  #colnames_batmod = c("pitch_type","release_speed","release_pos_x","release_pos_z","player_name","pitcher","p_throws","pfx_x","pfx_z","plate_x",
  #                    "plate_z","vx0","vy0","vz0","ax","ay","az","effective_speed","release_spin_rate","release_extension","release_pos_y","spin_axis")
  
  colnames_batmod = c("pitch_type","release_pos_x","release_pos_z","player_name","pitcher","plate_x",
                      "plate_z","vx0","vy0","vz0","ax","ay","az","release_spin_rate","release_pos_y","spin_axis")
  
  for(i in 1:length(names)){
    data = statcast_search(start_date = "2023-03-30", 
                           end_date = Sys.Date(), 
                           player_type = 'pitcher',
                           playerid = names[i])
    
    data = data %>%
      select(colnames_batmod)
    
    p_list[[paste(names[i])]] = data
  }
  
  return(p_list)
}

pitcher_data = get_pitcher_data(pitchers2023)

#find average of pitcher quantitative variables
get_avg_pitches = function(pitchers,means,sd){
  
  pitchers = keep(pitchers, ~all(nrow(.x) >= 250))
  
  a_list = list()
  f_list = list()
  
  for(i in 1:length(pitchers)){
    player_name = unique(pitchers[[i]]$player_name)[1]
    id = unique(pitchers[[i]]$pitcher)[1]
    throws = unique(pitchers[[i]]$p_throws)[1]
    data = pitchers[[i]]
    data = na.omit(data)
    data = data %>%
      select(-c("player_name","pitcher"))
    
    avg_data = data %>%
      group_by(pitch_type) %>%
      summarise_all(mean)
    
    pitch_type = avg_data$pitch_type
    
    avg_data = avg_data %>%
      select(-c("pitch_type"))
    
    data_scale = matrix(NA,nrow=nrow(avg_data),ncol=ncol(avg_data))
    data_scale = as.data.frame(data_scale)
    colnames(data_scale) = colnames(avg_data)
    
    for(i in 1:ncol(avg_data)){
      data_scale[,i] = (avg_data[,i] - means[i])/sd[i]
    }
    
    avg_data = data_scale
    
    avg_data$pitch_type = pitch_type
    avg_data$player_name = player_name
    avg_data$pitcher = id
    #avg_data$p_throws = throws
    
    a_list[[as.character(player_name)]] = avg_data
  }
  
  return(a_list)
}

#find frequency of pitcher pitch types
get_freq_pitches = function(pitchers){
  
  pitchers = keep(pitchers, ~all(nrow(.x) >= 250))
  
  f_list = list()
  
  for(i in 1:length(pitchers)){
    player_name = unique(pitchers[[i]]$player_name)[1]
    id = unique(pitchers[[i]]$pitcher)[1]
    
    data = pitchers[[i]]
    data = na.omit(data)
    data = table(data$pitch_type)/nrow(data)
    data$player_name = player_name
    data$pitcher = id
    
    f_list[[as.character(player_name)]] = data
  }
  
  return(f_list)
}

#input pitcher profile into batter models
get_xwoba_batters = function(batters,pitchers){
  
  xwoba_list = list()
  
  for(i in 1:length(batters)){
    for(j in 1:length(pitchers)){
      for(k in  1:nrow(pitchers[[j]])){
        current.data = pitchers[[j]][k,] %>%
          select(-c("pitch_type","player_name","pitcher"))
        
        xwoba_list[[names(batters)[[i]]]][[names(pitchers)[[j]]]][[k]] = predict(batters[[i]],current.data)
      }
    }
  }
  return(xwoba_list)
}


pitcher_avg = get_avg_pitches(pitcher_data,allbatdata_means,allbatdata_SD)
pitcher_freq = get_freq_pitches(pitcher_data)

allbattervpitcher = get_xwoba_batters(nn_models,pitcher_avg)

#multiply proportionate pitch frequency to performance findings
get_xwoba_wfreq = function(xwoba_list,freq_list){
  
  xwoba_perpitcher_list = list()
  
  for(i in 1:length(xwoba_list)){
    for(j in 1:length(xwoba_list[[i]])){
      xwoba_sum = 0 #initialize
      for(k in 1:length(xwoba_list[[i]][[j]])){
        xwoba_sum = xwoba_list[[i]][[j]][[k]]*freq_list[[j]][[k]] + xwoba_sum
      }
      xwoba_perpitcher_list[[names(xwoba_list)[[i]]]][[names(xwoba_list[[i]])[[j]]]] = xwoba_sum
    }
  }
  return(xwoba_perpitcher_list) 
}

battervpitcher_final = get_xwoba_wfreq(allbattervpitcher,pitcher_freq)

#combine results into dataframe and convert back to xwOBA
final.df = do.call("rbind", lapply(battervpitcher_final, as.data.frame))
final.df = (final.df * allbatdata_SD[14]) + allbatdata_means[14]

final.df = exp(final.df)-1
transpose_final.df = t(final.df)
transpose_final.df = as.data.frame(transpose_final.df)

final_stack = stack(transpose_final.df)

avg_final = sort(colMeans(transpose_final.df))
avg_final = as.data.frame(avg_final)

med_final = sort(apply(transpose_final.df,2,median))
med_final = as.data.frame(med_final)

pitch_avg_final = sort(colMeans(final.df))
pitch_avg_final = as.data.frame(pitch_avg_final)

pitch_med_final = sort(apply(final.df,2,median))
pitch_med_final = as.data.frame(pitch_med_final)

#Jorge Soler
ggplot(data=final_stack[(final_stack$ind == "Soler, Jorge"),],aes(values)) + geom_density(alpha=0.2,fill="red") +
  geom_vline(xintercept = 0.9902194, linetype="dashed")

garson(nn_models$`Soler, Jorge`)

#Kyle Schwarber
ggplot(data=final_stack[(final_stack$ind == "Schwarber, Kyle"),],aes(values)) + geom_density(alpha=0.2,fill="red") +
  geom_vline(xintercept = 0.9902194, linetype="dashed")

garson(nn_models$`Schwarber, Kyle`)


#############

#extract linear model results
get_model_lm = function(model_list){
  results_df = model_list[[1]]$results
  
  for(i in 2:length(model_list)){
    results_df = rbind(results_df,model_list[[i]]$results)
  }
  
  coef_df = model_list[[1]]$finalModel$coefficients
  
  for(i in 2:length(model_list)){
    coef_df = rbind(coef_df,model_list[[i]]$finalModel$coefficients)
  }
  
  return(cbind(results_df,coef_df))
}

lm_results = get_model_lm(bat_mod)
lm_results = lm_results %>%
  select(-c("parameter"))
lm_results = colMeans(lm_results)
#barchart(lm_results)


# model_results = list()
# 
# for(i in 1:length(bat_mod_list)){
#   model_df = get_model_lm(bat_mod_list[[i]])
#   
#   model_df1 = model_df %>%
#     select(-c("parameter"))
#   model_results[[i]] = colMeans(model_df1)
# }
# 
# allmodel_results = do.call(rbind, model_results)

######################### tune parameters for neural network

get_model_nn_tuneParam = function(model_list){
  results_list = list()
  
  for(i in 1:length(model_list)){
    results_list[[i]] = model_list[[i]]$results
  }
  
  results_final_df = results_list[[1]]
  
  for(i in 2:length(results_list)){
    results_final_df = results_final_df + results_list[[i]]
  }
  
  results_avg_df = results_final_df/length(results_list)
  
  return(list(results_avg_df,results_list))
}

nn_results = get_model_nn_tuneParam(nn_models)

#test = nn_results[[1]]

all_nn_results = nn_results[[2]]
allnndata = do.call(rbind.data.frame, all_nn_results)
allnndata1 = allnndata
allnndata1 = allnndata[(allnndata$decay>0),]
allnndata1$size = as.factor(allnndata1$size)
allnndata1$decay = as.factor(allnndata1$decay)

ggplot(data=allnndata1[(allnndata1$size == 2 & allnndata1$decay == 11),],aes(RMSE)) + geom_density(alpha=0.2,fill="red") +
  geom_vline(xintercept = 0.9902194, linetype="dashed")

qqnorm(allnndata1[(allnndata1$size == 2 & allnndata1$decay == 11),]$RMSE)
qqline(allnndata1[(allnndata1$size == 2 & allnndata1$decay == 11),]$RMSE)

shapiro.test(allnndata1[(allnndata1$size == 2 & allnndata1$decay == 11),]$RMSE)

test_com = test
test_com = test_com[(test_com$decay > 2),]
test_com$size = as.factor(test_com$size)

ggplot(data=test_com) + geom_line(aes(x=decay, y=RMSE, color=size)) + geom_point(aes(x=decay, y=RMSE, color=size)) + 
  #  xlim(1,10) +
  geom_vline(xintercept = 11, linetype="dashed") +
  annotate("text", x = 14, y = 0.9911, label = "Optimal Size = 2 \n Optimal Decay = 11")

ggplot(df, aes(x=wt, y=mpg, group=cyl)) +
  geom_point(aes(shape=cyl, color=cyl, size=cyl))


######################

#retrieve garson values for each model
get_garson = function(model_list){
  player_name = names(model_list)[1]
  garson_data = garson(model_list[[1]])
  
  nn_transpose = t(garson_data$data)
  colnames(nn_transpose) = nn_transpose[2,]
  nn_transpose = as.data.frame(nn_transpose)
  nn_transpose = nn_transpose[-2,]
  nn_transpose$player_name = player_name
  
  for(i in 2:length(model_list)){
    player_name = names(model_list)[i]
    garson_data = garson(model_list[[i]])
    
    nn_transpose1 = t(garson_data$data)
    colnames(nn_transpose1) = nn_transpose1[2,]
    nn_transpose1 = as.data.frame(nn_transpose1)
    nn_transpose1 = nn_transpose1[-2,]
    nn_transpose1$player_name = player_name
    nn_transpose = rbind(nn_transpose,nn_transpose1)
  }
  
  cols.num = colnames(subset(nn_transpose, select = -c(player_name)))
  nn_transpose[cols.num] = sapply(nn_transpose[cols.num],as.numeric)
  
  return(nn_transpose)
}

nn_garson = get_garson(nn_models)
nn_garson_avg = colMeans(nn_garson[,-14])
nn_garson_med = apply(nn_garson[,-14],2,median)

nn_garson_stack = stack(nn_garson[,-14])
colnames(nn_garson_stack) = c("Importance","Variables")

ggplot(data=nn_garson_stack) + geom_boxplot(aes(Importance,reorder(Variables, Importance, median),fill=Variables)) + 
  theme(legend.position="none",axis.title.y = element_blank())

# plot = list()
# for(i in 1:(ncol(nn_garson)-1)){
#   #var = colnames(nn_garson)[i]
#   plot[[i]] = ggplot(data=nn_garson)+geom_density(alpha=0.2,aes(colnames(nn_garson)[i]))
# }

# 
# plotdata = garson(nn_models$`Sosa, Edmundo`)


######################

#exploratory visuals
allbatdata = do.call(rbind.data.frame, batter_data)
var.names = colnames(select(allbatdata,-c(player_name,batter)))
allbatdata_1 = allbatdata %>%
  select(-c(player_name,p_throws))
allbatdata %>% ggpairs(var.names)

allbatdata_1$xwoba_transform = log(allbatdata_1$estimated_woba_using_speedangle+1)

correlations = cor(allbatdata_1)

library(pheatmap)
pheatmap(correlations, display_numbers = T, treeheight_row = 0, treeheight_col = 0, legend = F, angle_col = 45)

plotnet(nn_models$`Ohtani, Shohei`)


#########
#Pitcher Profile Visualizations

#Strikezone dataframe
strikezone = data.frame(c(-.71,.71,.71,-.71,-.71),
                        c(1.5,1.5,3.5,3.5,1.5))
colnames(strikezone) = c("x","z")

pitcherdisplay = subset(pitcher_avg$`Snell, Blake`, select=c(release_pos_x,release_pos_z,plate_x,plate_z,pitch_type,player_name))
pitcherdisplay$release_pos_x = (pitcherdisplay$release_pos_x * allbatdata_SD[1]) + allbatdata_means[1]
pitcherdisplay$release_pos_z = (pitcherdisplay$release_pos_z * allbatdata_SD[2]) + allbatdata_means[2]
pitcherdisplay$plate_x = (pitcherdisplay$plate_x * allbatdata_SD[3]) + allbatdata_means[3]
pitcherdisplay$plate_z = (pitcherdisplay$plate_z * allbatdata_SD[4]) + allbatdata_means[4]
pitcherdisplayfreq = as.data.frame(pitcher_freq$`Snell, Blake`)

for(i in 1:nrow(pitcherdisplay)){
  pitcherdisplay$freq[i] = pitcherdisplayfreq[1,i]
  pitcherdisplay$freqlab[i] = as.character(round(pitcherdisplay$freq[i],2)*100)
  pitcherdisplay$labels[i] = paste(pitcherdisplay$pitch_type[i],paste(pitcherdisplay$freqlab[i],"%",sep = ""),sep = " ")
}


ggplot() +
  geom_path(data = strikezone, aes(x=x, y=z)) +
  geom_segment(data = pitcherdisplay, aes(x = release_pos_x, y = release_pos_z, xend = plate_x, yend = plate_z, color = pitch_type),linetype = "longdash") +
  geom_point(data = pitcherdisplay, aes(x = release_pos_x, y = release_pos_z, color = pitch_type),shape = 15) +
  geom_point(data = pitcherdisplay, aes(x = plate_x, y = plate_z, size = freq, color = pitch_type)) +
  geom_text(data = pitcherdisplay, aes(x = plate_x, y = plate_z, label=labels, color = pitch_type),hjust=0.5, vjust=2, fontface="bold") +
  coord_equal() +
  labs(caption=pitcherdisplay$player_name[1]) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.caption = element_text(hjust=0.5,vjust=-1, size=rel(1.5), face="bold"),
        legend.position="none") +
  xlim(-4.8,4.8)


