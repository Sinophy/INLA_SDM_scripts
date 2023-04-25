# Comparing the spread of data

library(rWind)   
library(dplyr)
library(ggplot2)
library(egg)
library(ggpubr)
library(wesanderson)
library(fishualize)
library(viridis)

# Load data
Skatedat <- read.csv("data/data_f.csv")
Skate1 <- Skatedat %>% dplyr::select(dcoast, bath, btemp_mi, current_mi, pp, groundspeed, hauldur, haullat, skate_gen)  #both raster and dplyr have select function
Skate1$skate_gen <- as.factor(Skate1$skate_gen)

# Plot boxplots of environmental data
means <- aggregate(bath ~  skate_gen, Skate1, mean)

fun_mean <- function(x){
  return(data.frame(y=mean(x),label=mean(x,na.rm=T)))}


bath <- ggplot (Skate1, aes(x=skate_gen, y = bath)) +
  geom_violin() + ylab("Bathymetry/ m") + xlab("")+
  geom_jitter(aes(colour = skate_gen), alpha = 0.5, width=0.2)+
  geom_boxplot(width=0.1)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none")
bath

temp <- ggplot (Skate1, aes(x=skate_gen, y = btemp_mi)) +
  geom_violin() + ylab("Bottom Temperature/ °C") + xlab("")+
  geom_jitter(aes(colour = skate_gen), alpha = 0.5, width=0.2)+
  geom_boxplot(width=0.1, alpha=0)+
  scale_color_manual(values=c("seagreen3", "deepskyblue3"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none")
temp

current <- ggplot (Skate1, aes(x=skate_gen, y = current_mi)) +
  geom_violin() + ylab("Currents Velocity/ m-1") + xlab("")+
  geom_jitter(aes(colour = skate_gen), alpha = 0.5, width=0.2)+
  geom_boxplot(width=0.1)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none")
current

dcoast <- ggplot (Skate1, aes(x=skate_gen, y = dcoast)) +
  geom_violin() + ylab("Distance to coast/ °") + xlab("")+
  geom_jitter(aes(colour = skate_gen), alpha = 0.5, width=0.2)+
  geom_boxplot(width=0.1)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none")
dcoast

groundspeed <- ggplot (Skate1, aes(x=skate_gen, y = groundspeed)) +
  geom_violin() + ylab("Groundspeed/ kn") + xlab("")+
  geom_jitter(aes(colour = skate_gen), alpha = 0.5, width=0.2)+
  geom_boxplot(width=0.1)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none")
groundspeed

hauldur <- ggplot (Skate1, aes(x=skate_gen, y = hauldur)) +
  geom_violin() + ylab("Haul Duration/ mins") + xlab("")+
  geom_jitter(aes(colour = skate_gen), alpha = 0.5, width=0.2)+
  geom_boxplot(width=0.1)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none")
hauldur

pp <- ggplot (Skate1, aes(x=skate_gen, y = pp)) +
  geom_violin() + ylab("Primary Productivity/ g.m-3.day-1") + xlab("")+
  geom_jitter(aes(colour = skate_gen), alpha = 0.5, width=0.2)+
  geom_boxplot(width=0.1)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none")
pp


haullat <- ggplot (Skate1, aes(x=skate_gen, y = haullat)) +
  geom_violin() + ylab("Haul latitude/ ˚") + xlab("")+
  geom_jitter(aes(colour = skate_gen), alpha = 0.5, width=0.2)+
  geom_boxplot(width=0.1)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none")
haullat


ggarrange(bath, temp, current, groundspeed, hauldur, haullat, pp, ncol = 7, nrow = 1) %>%
  ggexport(filename = "environmentalplot1.jpeg", width = 1318, height = 1000)


ggarrange(bath, temp, current, pp,ncol = 4, nrow = 1)

#______________________________________________________________________________#

# visualising posterior distributions

betatemp <- predp$marginals.fixed$temp.std
head(betatemp)
betatemp_df <- as.data.frame(betatemp)


# Make function for inverse standardization - hard coded
StdNoMore <- function(x) { (x* sd) + mean}

mean <- mean(Skate$btemp_mi)  #adjust for original variable
sd <- sd(Skate$btemp_mi)

betatemp_df
betatemp_df$x0 <- StdNoMore(betatemp_df$x)

#______________________________________________________________________________#


postdist <- ggplot(betatemp_df, aes(x = x0, y = y))+
  geom_line()+
  labs(x = "Bottom Temperature/ °C", y = "Probability of skate presence")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none", plot.margin = margin(1,1,1,1, "cm"))
postdist

Skate$skate_gen <- as.factor(Skate$skate_gen)




temp <- ggplot (Skate, aes(x= skate_gen, y = btemp_mi)) +
  geom_jitter(aes(colour = skate_gen), size = 3,  alpha = 0.3, width=0.2)+
  geom_violin(alpha = 0) + ylab("Bottom Temperature/ °C") + xlab("")+
  geom_boxplot(width=0.1, alpha=0)+
  scale_color_manual(values=c("#56B4E9", "#E69F00"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
        axis.ticks.x=element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size = 18), legend.position="none", plot.margin = margin(1,1,1,1, "cm"))
temp

ggarrange(temp, postdist, ncol = 2, nrow = 1) 
  ggsave(filename = "export/environmentalplot1.jpeg", width = 10, dpi=700)

             
