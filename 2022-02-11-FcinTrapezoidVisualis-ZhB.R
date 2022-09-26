library(dplyr)
library(ggplot2)


lateral2_data <- read.csv("2021-01-05-Lateral2DataBase-ZhB.csv",header = TRUE, sep = ";", dec = ",")
grp_f_lp <- 1:length(lateral2_data[,1])
lateral2_data$grp_f_lp <- grp_f_lp

target_data <- lateral2_data[c("trial","motType","landmark","boundary","bounType","target","targSide","rt","targetY","targetX","pointerY","pointerX","TargPoinDis","DevY","Environm","Condition","TargMot","AbsZTargPoinDis","AngShape","DevX","grp_f_lp")]
target_data$xValue <- target_data$targetX
target_data$yValue <- target_data$targetY
target_data$tp_color <- rep(1, length(lateral2_data[,1]))

pointer_data <- lateral2_data[c("trial","motType","landmark","boundary","bounType","target","targSide","rt","targetY","targetX","pointerY","pointerX","TargPoinDis","DevY","Environm","Condition","TargMot","AbsZTargPoinDis","AngShape","DevX","grp_f_lp")]
pointer_data$xValue <- pointer_data$pointerX
pointer_data$yValue <- pointer_data$pointerY
pointer_data$tp_color <- rep(2, length(lateral2_data[,1]))

linePlot_data <- rbind(target_data, pointer_data)
#restrictions for cleaning are in 3 strings below
StandDistance = 3.29
DistanceLimit = 5
rtLimit <-10

#Target Wc, Rectangle plotting
linePlot_data <- linePlot_data[which((linePlot_data$target == 1) & (linePlot_data$bounType == 3) & (linePlot_data$TargPoinDis < DistanceLimit) & (linePlot_data$rt < rtLimit) & (linePlot_data$AbsZTargPoinDis <= StandDistance)),]
linePlot_dataf <- linePlot_data %>%
#Rename 1 to Landmarks, 2 to Boundary, 3 to Combined
        mutate(Environm = recode(Environm, "1"="Landmarks","2"="Boundary", "3"="Combined"))
linePlot_dataf$Environm<-factor(linePlot_dataf$Environm,levels = c("Landmarks","Boundary","Combined")) 

lineplot_ggplot <- ggplot(linePlot_dataf, aes(x = xValue, y = yValue, group = grp_f_lp)) +
  geom_line() +
  geom_point(aes(colour=tp_color)) +
  facet_wrap(~ Environm)  +
        theme(strip.text = element_text(face = "plain",size = rel(.5)),
              strip.background = element_rect(colour = "black",size = .5))+
        xlim(-12, 12) + ylim(8, 22) +
        scale_y_continuous(breaks = c(8,10,12,14,16,18,20,22))+
        scale_x_continuous(breaks = c(-12,-8,-4,0,4,8,12))+
        expand_limits(x=c(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12), y=c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22))+
  theme(plot.title = element_text(size = rel(.8),lineheight = .9,
  family = "Arial",face = "bold",colour = "black"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  xlab("X,m")+
  ylab ("Y,m")+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(family = "Arial",face = "plain",colour = "black",size = rel(.8)))+
  theme(axis.text.y = element_text(family = "Arial",face = "plain",colour = "black",size = rel(.8)))+
  theme(axis.title = element_text(family = "Arial",face = "plain",colour = "black",size = rel(.8)))     
  

print(lineplot_ggplot)
ggsave("2022-02-11-Target Fc in Trapezoid.png",width = 10, height = 7, unit = "cm", dpi=300) 

