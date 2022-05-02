library(ggeffects)
library(ggplot2)
library("tidyverse")
options(scipen = 999) #sets R to no e numbers
rm(list = ls()) #clears environment

setwd("C:/Users/jlg2117/OneDrive - Imperial College London/PhDv2/Analysis/Spider monkey/Results/binomial/predictions binomial")
Habitat_pred<- read.csv("Habitat_pred.csv", header=TRUE)
Protection_pred<- read.csv("Protection_pred.csv", header=TRUE)
Forest<- read.csv("binomial_forestcover_predict.csv", header=TRUE)
sec<- read.csv("Binomial_secondary_predict.csv", header=TRUE)
primary<- read.csv("Binomial_Primary_predict.csv", header=TRUE)
house<- read.csv("Binomial_newhouse_predict.csv", header=TRUE)
dist<- read.csv("Binomial_distcorc_predict.csv", header=TRUE)

setwd("C:/Users/jlg2117/OneDrive - Imperial College London/PhDv2/Analysis/Spider monkey/dfs")
Presence_with_covs<- read.csv("Presence_with_covs_Updated.csv", header=TRUE)

#order habitat 
#Habitat_pred$Habitat <- factor(Habitat_pred$Habitat , levels=c("Old Growth", "Secondary", "Mangrove", "Teak", "Palm", "Grassland"))
#bar plot for habitat binomial/count
#p<-ggplot(Habitat_pred, aes(x = Habitat, y = Occurance, fill=Dataset)) + 
  #geom_bar(aes(fill = Dataset),stat = "identity",position = "dodge") + 
  #scale_fill_brewer(palette="Dark2")+
#  scale_fill_grey(start=0.6, end=0.3)+
#  theme_classic(base_size = 22)+
# ylim(0, 0.7)+
#  ylab("Occupancy Probability")+
#  xlab("Land Use")+
  #geom_errorbar( aes(x=Habitat, ymin=Lower, ymax=Upper, group=factor(Dataset), width=0.4, colour="orange", alpha=0.9, size=1.3))
#  geom_bar(position=position_dodge(), stat="identity") +
#  geom_errorbar(aes(ymin=Lower, ymax=Upper),
#                width=.2,position=position_dodge(.9))
#p
#p + theme(axis.text = element_text(colour = "black"))

#p + theme_replace(axis.text = element_text(colour = "black"))
#p+theme(legend.position = c(0.85, 0.85))
#p+theme(axis.text=element_text(size=12),
#        axis.title=element_text(size=14,face="bold"))

#just one dataset
#Habitat_pred_one<-Habitat_pred[!(Habitat_pred$Dataset=="Reduced"),]

#order habitat 
Habitat_pred$Habitat <- factor(Habitat_pred$Habitat , levels=c("Old Growth", "Secondary", "Mangrove", "Teak", "Palm", "Grassland"))

p<-ggplot(Habitat_pred, aes(x = Habitat, y = Occurance)) + 
  #geom_bar(aes(fill = Dataset),stat = "identity",position = "dodge") + 
  #scale_fill_brewer(palette="Dark2")+
  geom_point(data = Habitat_pred, aes(x = Habitat, y = Occurance), size=5, shape=4)+
  #scale_fill_grey(start=0.3)+
  theme_classic(base_size = 22)+
  ylim(0, 1)+
  xlab("Land Use")+
  #geom_col(width = 0.2,position = position_dodge(0.7))+
  ylab("Occupancy Probability")+  
  #geom_errorbar( aes(x=Habitat, ymin=Lower, ymax=Upper, group=factor(Dataset), width=0.4, colour="orange", alpha=0.9, size=1.3))
  #geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Lower, ymax=Upper),
                width=.2,position=position_dodge(.9))
p
#p + theme(axis.text = element_text(colour = "black"))
p + geom_point(data = Presence_with_covs, aes(x = Habitat, y = Presence),  size=5, shape=16, colour="Black")
#p + theme_update(axis.text=element_text(size=12),axis.title=element_text(size=12))
#p+theme_update(legend.position = c(0.85, 0.85))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#land use
  Habitat_pred$Habitat <- factor(Habitat_pred$Habitat , levels=c("Old Growth", "Secondary", "Mangrove", "Teak", "Palm", "Grassland"))



#protection
Protection_pred$Protection <- factor(Protection_pred$Protection , levels=c("Wildlife Refuge", "Corcovado NP", "GD Forest Reserve", "Unprotected", "Terreba-Sierpe NP", "Piedras Blancas NP"))

coeff <- 100
p <- ggplot()+
  geom_point(data = Protection_pred, aes(x = Protection, y = Occurance), size=6, shape=4) +
  geom_errorbar(data = Protection_pred,aes(x = Protection, y = Occurance,ymin=Lower, ymax=Upper),
                width=.2,position=position_dodge(.9))+
  #ylim(0, 0.4)+
  #geom_point(stat="identity", position="dodge") +
  geom_point(data = Protection_pred, aes(x = Protection, y = Number/coeff),  size=4, shape=16, colour="Black") +
  scale_y_continuous(sec.axis = sec_axis(~.*coeff, name = "Occupied Sites (%)")) +
  ylab("Occurrence Probability") +
  xlab("Protection")
p + theme_classic(base_size = 21) 



#forest cover
  coeff <- 3
p5 <- ggplot()+
  geom_line(data = Forest, aes(x = Forest.Cover, y = Occurance))+
  #geom_point(data = Forest, aes(x = Forest.Cover, y = Occurance), size=6, shape=4) +
  geom_ribbon(data = Forest,aes(x = Forest.Cover, y = Occurance,ymin = Lower, ymax = Upper), alpha = .3, linetype=2, color = NA)+
  
  #ylim(0, 0.4)+
  #geom_point(stat="identity", position="dodge") +
  geom_point(data = Presence_with_covs, aes(x = ForestCover100Perc, y = Presence/coeff),  size=4, shape=16, colour="Black") +
  scale_y_continuous(sec.axis = sec_axis(~.*coeff, name = "Presence/Absence")) +
  ylab("Occurrence Probability") +
  xlab("Forest Cover (%)")
p5 + theme_classic(base_size = 21) 

#primary road
coeff <- 3.4
p6 <- ggplot()+
  geom_line(data = primary, aes(x = Primary, y = Occurance))+
  #geom_point(data = primary, aes(x = Primary, y = Occurance), size=6, shape=4) +
  geom_ribbon(data = primary,aes(x = Primary, y = Occurance,ymin = Lower, ymax = Upper), alpha = .3, linetype=2, color = NA)+
  
  #ylim(0, 0.4)+
  #geom_point(stat="identity", position="dodge") +
  geom_point(data = Presence_with_covs, aes(x = Primary1000km, y = Presence/coeff),  size=4, shape=16, colour="Black") +
  scale_y_continuous(sec.axis = sec_axis(~.*coeff, name = "Presence/Absence")) +
  ylab("Occurrence Probability") +
  xlab("Primary Road (km)")
p6 + theme_classic(base_size = 21)

#secondary road
coeff <- 6
p7 <- ggplot()+
  geom_line(data = sec, aes(x = Road, y = Occurance))+
  #geom_point(data = sec, aes(x = Road, y = Occurance), size=6, shape=4) +
  geom_ribbon(data = sec,aes(x = Road, y = Occurance,ymin = Lower, ymax = Upper), alpha = .3, linetype=2, color = NA)+
  
  #ylim(0, 0.4)+
  #geom_point(stat="identity", position="dodge") +
  geom_point(data = Presence_with_covs, aes(x = Secondary200km, y = Presence/coeff),  size=4, shape=16, colour="Black") +
  scale_y_continuous(sec.axis = sec_axis(~.*coeff, name = "Presence/Absence")) +
  ylab("Occurrence Probability") +
  xlab("Secondary Road (km)")
p7 + theme_classic(base_size = 21)

#house
coeff <- 4.5
p8 <- ggplot()+
  geom_line(data = house, aes(x = House, y = Occurance))+
  #geom_point(data = house, aes(x = House, y = Occurance), size=6, shape=4) +
  geom_ribbon(data = house,aes(x = House, y = Occurance,ymin = Lower, ymax = Upper), alpha = .3, linetype=2, color = NA)+
  
  xlim(0, 150)+
  #geom_point(stat="identity", position="dodge") +
  geom_point(data = Presence_with_covs, aes(x = House1000km, y = Presence/coeff),  size=4, shape=16, colour="Black") +
  scale_y_continuous(sec.axis = sec_axis(~.*coeff, name = "Presence/Absence")) +
  ylab("Occurrence Probability") +
  xlab("Density of Buildings (km2)")
p8 + theme_classic(base_size = 21)

#dist
coeff <- 2.5
p <- ggplot()+
  geom_line(data = dist, aes(x = Dist, y = Occurance))+
  #geom_point(data = dist, aes(x = Dist, y = Occurance), size=6, shape=4) +
  geom_ribbon(data = dist,aes(x = Dist, y = Occurance,ymin = Lower, ymax = Upper), alpha = .3, linetype=2, color = NA)+
  
  #xlim(0, 150)+
  #geom_point(stat="identity", position="dodge") +
  geom_point(data = Presence_with_covs, aes(x = Dist_Corc_km, y = Presence/coeff),  size=4, shape=16, colour="Black") +
  scale_y_continuous(sec.axis = sec_axis(~.*coeff, name = "Presence/Absence")) +
  ylab("Occurrence Probability") +
  xlab("Distance to Corcovado NP (km)")
p + theme_classic(base_size = 21)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#order protection 
Protection_pred$Protection <- factor(Protection_pred$Protection , levels=c("Wildlife Refuge", "Corcovado NP", "GD Forest Reserve", "Unprotected", "Terreba-Sierpe NP", "Piedras Blancas NP"))
#bar plot for habitat binomial/count
p<-ggplot(Protection_pred, aes(x = Protection, y = Occurance)) + 
  #geom_bar(aes(fill = Dataset),stat = "identity",position = "dodge") + 
  #scale_fill_brewer(palette="Dark2")+
  geom_point(data = Protection_pred, aes(x = Protection, y = Occurance), size=5, shape=4)+
  #scale_fill_grey(start=0.3)+
  theme_classic(base_size = 22)+
  ylim(0, 1)+
  xlab("Protection Status")+
  ylab("Occupancy Probability")+
  #geom_errorbar( aes(x=Habitat, ymin=Lower, ymax=Upper, group=factor(Dataset), width=0.4, colour="orange", alpha=0.9, size=1.3))
  #geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Lower, ymax=Upper),
                width=.2,position=position_dodge(.9))
p
p + geom_point(data = Presence_with_covs, aes(x = Protection, y = Presence),  size=5, shape=16, colour="Black")

#p+theme(legend.position = c(1, 1))


#forest line plot
#p<-ggplot(data=Forest, aes(x=Forest.Cover, y = Occurance, fill = Data_set)) +
  #geom_line(position = position_dodge(0.2)) +
  scale_linetype_manual(values=c("solid", "dashed"))+
  geom_line(aes(linetype=Data_set), size=0.6, position = position_dodge(0.2))+
  #geom_point(aes(size=2, shape=21))+
  geom_point(aes(shape=Data_set), size=3)+# ise this for different shape points per line
  #geom_point( size=2, shape=21, fill="Black")+ use this for points all one
  #geom_line(aes(color = Dataset, linetype = Dataset)) + 
  xlab("Forest Cover (%)")+
  ylab("Occupancy Probability")+
  ylim(0, 0.43)+
  #theme(legend.position="top")+
  theme_classic(base_size = 22)+
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill=Data_set), alpha = .3, linetype=2, color = NA)+
  #scale_fill_manual(values = c("blueviolet", "green4"), aesthetics = c("color", "fill"))# use this for colour ribbon
  scale_fill_grey(start=0.4, end=0.1, aesthetics = c("color", "fill"))#use this for bw ribbon
#p+theme(legend.position = c(0.15, 0.80))
##p + theme(axis.text = element_text(colour = "black"))

#just one dataset
forest_one<-Forest[!(Forest$Data_set=="Reduced"),]
p<-ggplot(data=forest_one, aes(x=Forest.Cover, y = Occurance)) +
  geom_line()+
  geom_point(size=5, shape=16)+# ise this for different shape points per line
  xlab("Forest Cover (%)")+
  ylab("Occupancy Probability")+
  ylim(0, 1)+
  #theme(legend.position="top")+
  theme_classic(base_size = 22)+
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = .3, linetype=2, color = "grey")
p
p + geom_point(data = Presence_with_covs, aes(x = ForestCover100_perc, y = Presence),  size=5, shape=1, colour="Black")

    

 
    
    
#secondary line plot
#p<-ggplot(data=sec, aes(x=Road, y = Occurance, fill = Data_set)) +
  #geom_line(position = position_dodge(0.2)) +
  scale_linetype_manual(values=c("solid", "dashed"))+
  geom_line(aes(linetype=Data_set), size=0.6)+
  #geom_point(aes(size=2, shape=21))+
  geom_point(aes(shape=Data_set), size=3)+# ise this for different shape points per line
  #geom_point( size=2, shape=21, fill="Black")+ use this for points all one
  #geom_line(aes(color = Dataset, linetype = Dataset)) + 
  xlab("Secondary Road Density (km)")+
  ylab("Occupancy Probability")+
  #theme(legend.position="top")+
  theme_classic(base_size = 22)+
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill=Data_set), alpha = .3, linetype=2, color = NA)+
  #scale_fill_manual(values = c("blueviolet", "green4"), aesthetics = c("color", "fill"))# use this for colour ribbon
  scale_fill_grey(start=0.4, end=0.1, aesthetics = c("color", "fill"))#use this for bw ribbon
p
p + geom_point(data = Presence_with_covs, aes(x = ForestCover100_perc, y = Presence),  size=6, shape=2, colour="Black")

#p+theme(legend.position = c(0.85, 0.85))
#p + theme(axis.text = element_text(colour = "black"))

#just one dataset
sec_one<-sec[!(sec$Data_set=="Reduced"),]
p<-ggplot(data=sec_one, aes(x=Road, y = Occurance)) +
  geom_line()+
  geom_point(size=5, shape=16)+# ise this for different shape points per line
  xlab("Secondary Road Density (km)")+
  ylab("Occupancy Probability")+
  ylim(0, 1)+
  #theme(legend.position="top")+
  theme_classic(base_size = 22)+
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = .3, linetype=2, color = "grey")
p + geom_point(data = Presence_with_covs, aes(x = Secondary200km, y = Presence),  size=5, shape=1, colour="Black")
p
p + theme(axis.text = element_text(colour = "black"))

#primary line plot
p<-ggplot(data=primary, aes(x=Primary, y = Occurance, fill = Data_set)) +
  #geom_line(position = position_dodge(0.2)) +
  scale_linetype_manual(values=c("solid", "dashed"))+
  geom_line(aes(linetype=Data_set), size=0.6)+
  #geom_point(aes(size=2, shape=21))+
  geom_point(aes(shape=Data_set), size=3)+# ise this for different shape points per line
  #geom_point( size=2, shape=21, fill="Black")+ use this for points all one
  #geom_line(aes(color = Dataset, linetype = Dataset)) + 
  xlab("Primary Road Density (km)")+
  ylab("Occupancy Probability")+
  #theme(legend.position="top")+
  theme_classic(base_size = 22)+
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill=Data_set), alpha = .3, linetype=2, color = NA)+
  #scale_fill_manual(values = c("blueviolet", "green4"), aesthetics = c("color", "fill"))# use this for colour ribbon
  scale_fill_grey(start=0.4, end=0.1, aesthetics = c("color", "fill"))#use this for bw ribbon
p
p + geom_point(data = Presence_with_covs, aes(x = Secondary200km, y = Presence),  size=6, shape=2, colour="Black")

p+theme(legend.position = c(0.85, 0.85))
p + theme(axis.text = element_text(colour = "black"))

#just one dataset
primary_one<-primary[!(primary$Data_set=="Reduced"),]
p<-ggplot(data=primary_one, aes(x=Primary, y = Occurance)) +
  geom_line()+
  geom_point(size=5, shape=16)+# ise this for different shape points per line
  xlab("Primary Road Density (km)")+
  ylab("Occupancy Probability")+
  ylim(-0.001, 1)+
  #theme(legend.position="top")+
  theme_classic(base_size = 22)+
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = .3, linetype=2, color = "grey")
p
p + geom_point(data = Presence_with_covs, aes(x = Primary1000km, y = Presence),  size=5, shape=1, colour="Black")

p + theme(axis.text = element_text(colour = "black"))
p

house <- house[-c(57),] 

#house line plot
p<-ggplot(data=house, aes(x=House, y = Occurance, fill = Dataset)) +
  #geom_line(position = position_dodge(0.2)) +
  #scale_linetype_manual(values=c("solid", "dashed"))+
  geom_line(aes(linetype=Dataset), size=0.6)+
  #geom_point(aes(size=2, shape=21))+
  geom_point(aes(shape=Dataset), size=3)+# ise this for different shape points per line
  #geom_point( size=2, shape=21, fill="Black")+ use this for points all one
  #geom_line(aes(color = Dataset, linetype = Dataset)) + 
  xlab("Density of Houses (Km2)")+
  ylab("Occupancy Probability")+
  #theme(legend.position="top")+
  theme_classic(base_size = 22)+
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill=Dataset), alpha = .3, linetype=2, color = NA)+
  #scale_fill_manual(values = c("blueviolet", "green4"), aesthetics = c("color", "fill"))# use this for colour ribbon
  scale_fill_grey(start=0.4, end=0.1, aesthetics = c("color", "fill"))#use this for bw ribbon
p
p+theme(legend.position = c(0.85, 0.85))
p + theme(axis.text = element_text(colour = "black"))

#just one dataset
house_one<-house[!(house$Dataset=="Reduced"),]
p<-ggplot(data=house, aes(x=House, y = Occurance)) +
  geom_line()+
  geom_point(size=5, shape=16)+# ise this for different shape points per line
  xlab("Density of Houses (Km2)")+
  ylab("Occupancy Probability")+
  ylim(0, 1)+
  xlim(0, 110)+
  #theme(legend.position="top")+
  theme_classic(base_size = 22)+
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = .3, linetype=2, color = "grey")
p
p + geom_point(data = Presence_with_covs, aes(x = House1000km, y = Presence),  size=5, shape=1, colour="Black")
p + theme(axis.text = element_text(colour = "black"))


#corc line plot
p<-ggplot(data=dist, aes(x=Dist, y = Occurance, fill = Data_set)) +
  #geom_line(position = position_dodge(0.2)) +
  scale_linetype_manual(values=c("solid", "dashed"))+
  geom_line(aes(linetype=Data_set), size=0.6, position = position_dodge(0.2))+
  #geom_point(aes(size=2, shape=21))+
  geom_point(aes(shape=Data_set), size=3)+# ise this for different shape points per line
  #geom_point( size=2, shape=21, fill="Black")+ use this for points all one
  #geom_line(aes(color = Dataset, linetype = Dataset)) + 
  xlab("Distance to Corcovado NP")+
  ylab("Occupancy Probability")+
  #theme(legend.position="top")+
  theme_classic(base_size = 22)+
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill=Data_set), alpha = .3, linetype=2, color = NA)+
  #scale_fill_manual(values = c("blueviolet", "green4"), aesthetics = c("color", "fill"))# use this for colour ribbon
  scale_fill_grey(start=0.4, end=0.1, aesthetics = c("color", "fill"))#use this for bw ribbon
p
p+theme(legend.position = c(0.85, 0.85))
p + theme(axis.text = element_text(colour = "black"))

#just one dataset
dist_one<-dist[!(dist$Data_set=="Reduced"),]
p<-ggplot(data=dist_one, aes(x=Dist, y = Occurance)) +
  geom_line()+
  geom_point(size=5, shape=16)+# ise this for different shape points per line
  xlab("Distance to Corcovado NP")+
  ylab("Occupancy Probability")+
  ylim(0, 1)+
  #theme(legend.position="top")+
  theme_classic(base_size = 22)+
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = .3, linetype=2, color = "grey")
p
p + geom_point(data = Presence_with_covs, aes(x = Dist_Corc_km, y = Presence),  size=5, shape=1, colour="Black")

p + theme(axis.text = element_text(colour = "black"))

