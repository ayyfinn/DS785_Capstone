#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(ggplot2)
library(gtable)
library(ggpubr)
library(gridExtra)

load("data_shiny.RData")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({
      
      playerfind = input$name
      playerfind_ind = grep(playerfind, colnames(transpose_final.df))
      
      p1 = ggplot() + geom_bar(data=nn_garson_stack[nn_garson_stack$Player == playerfind,], aes(x=reorder(Variables, Importance), y=Importance),stat="identity") +
        labs(x='Variables',title=playerfind) + theme(plot.title = element_text(hjust = 0.5, size=rel(1.5), face="bold"),
                                                     axis.title.x=element_text(size=rel(1.25), face="bold"),
                                                     axis.text.x=element_text(size=rel(1.25), face="bold"),
                                                     axis.title.y=element_text(size=rel(1.25), face="bold"),
                                                     axis.text.y=element_text(size=rel(1.25), face="bold"))
      mytheme = gridExtra::ttheme_default(
        core = list(padding = unit(c(2.5, 2.5), "mm")))
      
      player_df = as.data.frame(cbind(transpose_final.df$Pitchers,format(round(transpose_final.df[,playerfind_ind],3),nsmall = 3)))
      colnames(player_df) = c("Pitcher","Proj xwOBA")
      player_df = player_df[order(as.numeric(player_df$'Proj xwOBA')),]
      p2 = tableGrob(head(player_df[order(as.numeric(player_df$'Proj xwOBA')),],5), theme = mytheme, rows = NULL)
      
      
      #Pitcher Profile Visualizations
      
      #Strikezone dataframe
      strikezone = data.frame(c(-.71,.71,.71,-.71,-.71),
                              c(1.5,1.5,3.5,3.5,1.5))
      colnames(strikezone) = c("x","z")
      
      worstpitchmatchfind = head(player_df$Pitcher,1)
      worstpitchmatch_ind = grep(worstpitchmatchfind, names(pitcher_avg))
      
      pitcherdisplay1 = subset(pitcher_avg[[worstpitchmatch_ind]], select=c(release_pos_x,release_pos_z,plate_x,plate_z,pitch_type,player_name))
      pitcherdisplay1$release_pos_x = (pitcherdisplay1$release_pos_x * allbatdata_SD[1]) + allbatdata_means[1]
      pitcherdisplay1$release_pos_z = (pitcherdisplay1$release_pos_z * allbatdata_SD[2]) + allbatdata_means[2]
      pitcherdisplay1$plate_x = (pitcherdisplay1$plate_x * allbatdata_SD[3]) + allbatdata_means[3]
      pitcherdisplay1$plate_z = (pitcherdisplay1$plate_z * allbatdata_SD[4]) + allbatdata_means[4]
      pitcherdisplay1freq = as.data.frame(pitcher_freq[[worstpitchmatch_ind]])
      
      for(i in 1:nrow(pitcherdisplay1)){
        pitcherdisplay1$freq[i] = pitcherdisplay1freq[1,i]
        pitcherdisplay1$freqlab[i] = as.character(round(pitcherdisplay1$freq[i],2)*100)
        pitcherdisplay1$labels[i] = paste(pitcherdisplay1$pitch_type[i],paste(pitcherdisplay1$freqlab[i],"%",sep = ""),sep = " ")
      }
      
      
      p3 = ggplot() +
        geom_path(data = strikezone, aes(x=x, y=z)) +
        geom_segment(data = pitcherdisplay1, aes(x = release_pos_x, y = release_pos_z, xend = plate_x, yend = plate_z, color = pitch_type),linetype = "longdash") +
        geom_point(data = pitcherdisplay1, aes(x = release_pos_x, y = release_pos_z, color = pitch_type),shape = 15) +
        geom_point(data = pitcherdisplay1, aes(x = plate_x, y = plate_z, size = freq, color = pitch_type)) +
        geom_text(data = pitcherdisplay1, aes(x = plate_x, y = plate_z, label=labels, color = pitch_type),hjust=0.5, vjust=2, fontface="bold") +
        coord_equal() +
        labs(caption=pitcherdisplay1$player_name[1],title="Worst Matchups") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              plot.caption = element_text(hjust=0.5,vjust=-1, size=rel(1.5), face="bold"),
              legend.position="none",
              plot.title = element_text(hjust = 0.5,size=rel(1.5),face="bold")) +
        aes(ymin=0) +
        xlim(-4.8,4.8)
      
      p4 = tableGrob(head(player_df[order(-as.numeric(player_df$'Proj xwOBA')),],5), theme = mytheme, rows = NULL)
      
      bestpitchmatchfind = tail(player_df$Pitcher,1)
      bestpitchmatch_ind = grep(bestpitchmatchfind, names(pitcher_avg))
      
      pitcherdisplay2 = subset(pitcher_avg[[bestpitchmatch_ind]], select=c(release_pos_x,release_pos_z,plate_x,plate_z,pitch_type,player_name))
      pitcherdisplay2$release_pos_x = (pitcherdisplay2$release_pos_x * allbatdata_SD[1]) + allbatdata_means[1]
      pitcherdisplay2$release_pos_z = (pitcherdisplay2$release_pos_z * allbatdata_SD[2]) + allbatdata_means[2]
      pitcherdisplay2$plate_x = (pitcherdisplay2$plate_x * allbatdata_SD[3]) + allbatdata_means[3]
      pitcherdisplay2$plate_z = (pitcherdisplay2$plate_z * allbatdata_SD[4]) + allbatdata_means[4]
      pitcherdisplay2freq = as.data.frame(pitcher_freq[[bestpitchmatch_ind]])
      
      for(i in 1:nrow(pitcherdisplay2)){
        pitcherdisplay2$freq[i] = pitcherdisplay2freq[1,i]
        pitcherdisplay2$freqlab[i] = as.character(round(pitcherdisplay2$freq[i],2)*100)
        pitcherdisplay2$labels[i] = paste(pitcherdisplay2$pitch_type[i],paste(pitcherdisplay2$freqlab[i],"%",sep = ""),sep = " ")
      }
      
      
      p5 = ggplot() +
        geom_path(data = strikezone, aes(x=x, y=z)) +
        geom_segment(data = pitcherdisplay2, aes(x = release_pos_x, y = release_pos_z, xend = plate_x, yend = plate_z, color = pitch_type),linetype = "longdash") +
        geom_point(data = pitcherdisplay2, aes(x = release_pos_x, y = release_pos_z, color = pitch_type),shape = 15) +
        geom_point(data = pitcherdisplay2, aes(x = plate_x, y = plate_z, size = freq, color = pitch_type)) +
        geom_text(data = pitcherdisplay2, aes(x = plate_x, y = plate_z, label=labels, color = pitch_type),hjust=0.5, vjust=2, fontface="bold") +
        coord_equal() +
        labs(caption=pitcherdisplay2$player_name[1],title="Best Matchups") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              plot.caption = element_text(hjust=0.5,vjust=-1, size=rel(1.5), face="bold"),
              legend.position="none",
              plot.title = element_text(hjust = 0.5,size=rel(1.5),face="bold")) +
        aes(ymin=0) +
        xlim(-4.8,4.8)
      
      
      #p23 = ggarrange(p2,p3,ncol=2,nrow=1)
      
      #p23_1 = annotate_figure(p23, top = text_grob("Worst Pitcher Matchups", 
      #                                      color = "black", face = "bold", size = 14))
      
      p23_2 = p3 + annotation_custom(p2,
                                   xmin = -3, ymin = -3.5,
                                   xmax = -3)
      
      #p45 = ggarrange(p5,p4,ncol=2,nrow=1)
      
      #p45_1 = annotate_figure(p45, top = text_grob("Best Pitcher Matchups", 
      #                                             color = "black", face = "bold", size = 14))
      
      p45_2 = p5 + annotation_custom(p4,
                                   xmin = 3, ymin = -3.5,
                                   xmax = 3)
      
      #p2345 = ggarrange(p23,p45,ncol=1,nrow=2)
      #p2345_1 = ggarrange(p23_2,p45_2,ncol=1,nrow=2)
      
      
      pitcherfind_see = input$name2
      pitcherfind_see_ind = grep(pitcherfind_see, transpose_final.df$Pitchers)
      
      specific = format(round(transpose_final.df[pitcherfind_see_ind,playerfind_ind],3),nsmall=3)
      
      pitcherdisplay3 = subset(pitcher_avg[[pitcherfind_see_ind]], select=c(release_pos_x,release_pos_z,plate_x,plate_z,pitch_type,player_name))
      pitcherdisplay3$release_pos_x = (pitcherdisplay3$release_pos_x * allbatdata_SD[1]) + allbatdata_means[1]
      pitcherdisplay3$release_pos_z = (pitcherdisplay3$release_pos_z * allbatdata_SD[2]) + allbatdata_means[2]
      pitcherdisplay3$plate_x = (pitcherdisplay3$plate_x * allbatdata_SD[3]) + allbatdata_means[3]
      pitcherdisplay3$plate_z = (pitcherdisplay3$plate_z * allbatdata_SD[4]) + allbatdata_means[4]
      pitcherdisplay3freq = as.data.frame(pitcher_freq[[pitcherfind_see_ind]])
      
      for(i in 1:nrow(pitcherdisplay3)){
        pitcherdisplay3$freq[i] = pitcherdisplay3freq[1,i]
        pitcherdisplay3$freqlab[i] = as.character(round(pitcherdisplay3$freq[i],2)*100)
        pitcherdisplay3$labels[i] = paste(pitcherdisplay3$pitch_type[i],paste(pitcherdisplay3$freqlab[i],"%",sep = ""),sep = " ")
      }
      
      
      p6 = ggplot() +
        geom_path(data = strikezone, aes(x=x, y=z)) +
        geom_segment(data = pitcherdisplay3, aes(x = release_pos_x, y = release_pos_z, xend = plate_x, yend = plate_z, color = pitch_type),linetype = "longdash") +
        geom_point(data = pitcherdisplay3, aes(x = release_pos_x, y = release_pos_z, color = pitch_type),shape = 15) +
        geom_point(data = pitcherdisplay3, aes(x = plate_x, y = plate_z, size = freq, color = pitch_type)) +
        geom_text(data = pitcherdisplay3, aes(x = plate_x, y = plate_z, label=labels, color = pitch_type),hjust=0.5, vjust=2, fontface="bold") +
        coord_equal() +
        labs(caption=paste(pitcherdisplay3$player_name[1],"vs.",playerfind),title=paste("Projected xwOBA =",specific)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              plot.caption = element_text(hjust=0.5,vjust=-1, size=rel(2), face="bold"),
              legend.position="none",
              plot.title = element_text(hjust = 0.5,size=rel(1.5), face="bold")) +
        aes(ymin=0) +
        xlim(-4.8,4.8) +
        geom_text(aes(x = 0,y=0,label="Custom Matchup"),size=rel(5), fontface="bold")
      
      library(grid)

      grid.arrange(p1,
                   p23_2,p45_2,p6,
                   nrow = 2,
                   #ncol = 2,
                   layout_matrix = rbind(c(4,1,1), c(4,2,3))
                   ) + 
        annotation_custom(
          grid.polygon(c(1/3, 1/3, 2/3, 2/3, 0, 1, 1/3, 1, 0, 1, 0, 0, 1, 1),
                       c(0, 1, 0, 0.5, 1, 1, 0.5, 0.5, 0, 0, 0, 1, 0, 1), 
                       id = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7), 
                       gp = gpar(lwd = 1.5)))
      
    }, height = 800, width = 1800)

})
