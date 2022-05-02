library(ape)
library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(statmod)
library(ggplot2)
library(lmtest)
require(lmtest)
library(gstat)
library(car)
library(multcomp)
library(emmeans)
library(unmarked)
library(tidyr)
library(AICcmodavg)
rm(list = ls()) #clears environment
options(scipen = 999) #sets R to no e numbers

setwd("C:/Users/jlg2117/OneDrive - Imperial College London/PhDv2/Analysis/Spider monkey/dfs")
#loading dfs with environmental variables
Spider_Monkey_Main<- read.csv("Spider_Monkey_Main.csv", header=TRUE)
Spider_Monkey_cov<- read.csv("Spider_Monkey_cov_reduced.csv", header=TRUE)
Spider_Monkey_cov<- read.csv("Presence_with_covs_Updated.csv", header=TRUE)

#creating detection history dataframe
Detection_History <- c("Site", "Presence", "Day")# make new df with these columns
Detection_History <- Spider_Monkey_Main[Detection_History]
Detection_History<-Detection_History %>% spread(Day, Presence, fill = NA, convert = FALSE)#spread the data to wide format
Detection_History<-Detection_History[,-1]#remove first column
Det_Hist<- read.csv("Detection_History.csv", header=TRUE)

#remove dupilcated columns from covariates
Spider_Monkey_cov<-Spider_Monkey_cov %>% distinct(Site, .keep_all = TRUE)
write.csv(Spider_Monkey_cov, "Spider_Monkey_cov.csv")

#check structure
str(Spider_Monkey_Main$Protection)
Spider_Monkey_Main$Protection

#checks for nas
sum(is.na(Spider_Monkey_Main))

#descriptive stats
aggregate(x = Presence_with_covs$Presence,                # Specify data column
          by = list(Presence_with_covs$Elevation),              # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)


#Checking for normality in the data
hist(Dist_Corc, na.rm=TRUE)#plot histogram of points 
shapiro.test(Total_Calls)# checking normality test less than 0.05 means not normal
qqnorm(Total_Calls) # checking normality by qqplot
qqline(Total_Calls)

#keeping only presence data
Spider_Monkey_Main<- filter(Spider_Monkey_Main, Presence>0)

#plots
#Catagorical vs Catagorical variables- Relationship between presence and habitat or presence and protection
ggplot(Spider_Monkey_Main,  aes(x=as.factor(Presence), fill=Protection))+
  geom_bar(position = "stack")
  geom_bar(position = "fill")

#Continous vs continous variables
ggplot(Spider_Monkey_cov, aes(x=Sum_House, y=Presence, group= Habitat, colour= Habitat))+
 #geom_line()+
  geom_point()+
  ylab("Presence")+
  xlab("Dist_Corc")+
  #geom_smooth(method = "gam")+
  theme(legend.position="bottom")+
  scale_x_discrete(breaks=c(0, 5000, 10000, 15000, 20000, 25000, 30000))+
  theme_classic(base_size = 13)


#bar plot- count of the data for catagorical variables
ggplot(Spider_Monkey_Main, aes(x = Presence, fill=as.factor(Habitat))) + 
  geom_bar()
#cont of data for continours or count data- 
ggplot(Spider_Monkey_Main, aes(x = Whinny, fill=as.factor(Protection))) + 
  geom_histogram()
#smootherd version of histogram
ggplot(Spider_Monkey_Main, aes(x = Total_Road)) + 
  geom_density()

  
#pairwise plots
library(GGally)
#method with ggplot 
ggpairs(Spider_Monkey_Main, columns=1:13, lower = list(continuous= wrap("smooth", alpha = 0.3, size=0.01, colour="blue"))) + 
  ggtitle("Indices") +
  theme_classic(base_size = 13) +
  theme(legend.position= "right", plot.title= element_text(hjust = 0.5))

##########################################occupancy model################################################
# define covariates
siteCovs <- Spider_Monkey_cov[,c("House1000km", "Habitat", "Primary_Road", "Secondary200km", "ForestCover200")]

#create an occupancy frame, this can be with or without the siteCovs
Occupancy<-unmarkedFrameOccu(y=Det_Hist, siteCovs = siteCovs)#convert df into something unmarked can recognise
summary(Occupancy)

#model 1 no covariates
No_Cov<-occu(~1~1, data=Occupancy)# 1's say that occ or det doesnt vary with any covariate
No_Cov#summary
No_cov.psi<-backTransform(No_Cov, type="state") #backtransforms occ to give % score
No_cov.psi

No_cov.p<-backTransform(No_Cov, type="det") #backtransforms det to give % score
No_cov.p

No_cov.psi_CI<-confint((No_cov.psi)) #gives confidence intervals for occupancy
No_cov.psi_CI

No_cov.p_CI<-confint((No_cov.p)) #gives confidence intervals for det
No_cov.p_CI


#model 2 adding covariates - primary road and habitat dont converge
# 1's say that occ or det doesnt vary with any covariate
Habitat_Cov_Occ<-occu(~ForestCover200~House1000km+ Habitat +Primary_Road+ForestCover200 + Secondary200km, data=Occupancy)
 
Habitat_Cov_Occ<-occu(~1~?, data=Occupancy)
Habitat_Cov_Occ<-occu(~?~1, data=Occupancy)

Habitat_Cov_Occ#summary
exp(-0.27)

#calls up the occupancy estimate using state and gives predicted occupancy for variables
preds<- data.frame(House1000km=c(0,0.25,0.5,1,2,3,4,5,10,15,20,25,30,35,40, 45,50,55,60,65,70,75,80,85,90,95,100,150,200,250,300))
preds<- data.frame(ForestCover200 =c( 0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
preds<- data.frame(Secondary200km=c(0, 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1))

#testing protection in next chapter
preds<- data.frame(New_Protection =c( "Secondary","Old Growth","Disturbed"))
preds<- data.frame(Protection =c( "Piedras_Blancas","Reserve","Sierpe", "Outside", "Corcovado"))
preds<- data.frame(New_Protection =c( "Nat_Parks","Reserve", "Outside", "Corcovado"))


# testing model fit
fits.Habitat_cov.psi<-predict(Habitat_Cov_Occ, type="state", newdata=preds, appendData=TRUE)
fits.Habitat_cov.psi$type<-"psi"
fits.Habitat_cov.psi
write.table(fits.Habitat_cov.psi, "fits.Habitat_cov.psihabitat.txt")

fits.Habitat_cov.p<-predict(Habitat_Cov_Occ, type="det", newdata=preds, appendData=TRUE)
fits.Habitat_cov.p$type<-"p"
fits.Habitat_cov.p

fits.Habitat_cov.p_Combined<-rbind(fits.Habitat_cov.psi, fits.Habitat_cov.p)#joins 2 tables together
fits.Habitat_cov.p_Combined#view

coef(Habitat_Cov_Occ, type = "state")
vcov(Habitat_Cov_Occ, type = "state")

#goodness of fit
occ_gof <- mb.gof.test(Habitat_Cov_Occ, nsim = 100, plot.hist = FALSE)								
occ_gof$chisq.table <- NULL								
print(occ_gof)

#get candidate models
library(AICcmodavg)
library(MuMIn)

occ_dredge <- dredge(Habitat_Cov_Occ)
mc <- as.data.frame(occ_dredge)
mc

occ_dredge_delta <- get.models(occ_dredge, subset = delta <= 4)								
occ_dredge_delta		
occ_avg <- model.avg(occ_dredge_delta, fit = TRUE)
coef(occ_avg)
summary(occ_avg)

lc <- linearComb(Habitat_Cov_Occ, c(1,0,1,0), type="state") # Estimate abundance on the log scale when forest=0										
backTransform(lc) # Abundance on the original scale										
