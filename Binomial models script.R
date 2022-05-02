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
library(GGally)
library(caret)
library(brglm2)
library(rockchalk)
library(MASS)    
library(mgcv)                                         
library(sf)                                         
library(sp)
library(spdep)                                         
library(raster)                                         
library(vegan)       
library(ade4)       
library(adespatial)       
library(spdep)
library(DHARMa)
library(car)
library(magrittr)
library(ggeffects)
library(sjmisc)
library(MuMIn)
library(effects)
library(PerformanceAnalytics)
install.packages("DescTools")
rm(list = ls()) #clears environment
options(scipen = 999) #sets R to no e numbers

#read in main dfs
setwd("C:/Users/jlg2117/OneDrive - Imperial College London/PhDv2/Analysis/Spider monkey/dfs")
Presence_with_covs<- read.csv("Presence_with_covs_Updated.csv", header=TRUE)
Presence_with_covs<-Presence_with_covs[,-1]#remove first column

#read in dfs forvarience partitioning 
p<- read.csv("varpart_binom.csv", header=TRUE)
env<- read.csv("varpart_binom_env.csv", header=TRUE)

#__________________________________________________________________________________________________________________#
#exploratory plots
#Catagorical vs Catagorical- This shows me the relationship between habitat and protection or presence and habitat or presence and protection
ggplot(Presence_with_covs,  aes(x=as.factor(Presence), fill=Habitat))+
  geom_bar(position = "stack")
geom_bar(position = "fill")

#Continous vs Continous
ggplot(Presence_with_covs, aes(x=ForestCover200, y=Presence))+
  #geom_line()+
  geom_point(size=3.5)+
  ylab("Presence")+
  xlab("Forest Cover")+
  #geom_smooth(method = "gam")+
  #scale_color_viridis()
  #scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom")+
  #scale_x_discrete(breaks=c(0, 5000, 10000, 15000, 20000, 25000, 30000))+
  theme_classic(base_size = 13)
  library(vidrisd)
  
#__________________________________________________________________________________________________________________
#testing for correlation between variables

#method with cor plots
colnames(Presence_with_covs)
chart.Correlation(Presence_with_covs[,c(11,13,34,36,43)], histogram=TRUE, pch=19, method="pearson")

#method with ggplot 
my_fn <- function(data, mapping, pts=list(), smt=list(), ...){
  ggplot(data = data, mapping = mapping, ...) + 
    do.call(geom_point, pts) +
    do.call(geom_smooth, smt) 
}
ggpairs(Presence_with_covs[11,13,34,36,43], lower = list(continuous= wrap(my_fn, alpha = 0.3, size=0.01, colour="blue"))) + 
  ggtitle("Indices") +
  upper=list(params=list(corSize=6))+
  theme_classic(base_size = 13) +
   theme(legend.position= "right", plot.title= element_text(hjust = 0.5))

#___________________________________________________________________________________________________________________________-
#glm in brms for habitat and primary road
HabitatBR <- glm(Presence ~ Habitat.Ordered, family = binomial,
                 method="brglm_fit",data = Presence_with_covs)


HabitatBRac <- glm(Presence ~ Habitat.Order+ac, family = binomial,
                  method="brglm_fit", data = Presence_with_covs)


primary <- glm(Presence ~ Primary1000km, family = binomial,
                    method="brglm_fit",data = Presence_with_covs)


primaryac <- glm(Presence ~ Primary1000km+ac, family = binomial,
                      method="brglm_fit", data = Presence_with_covs)


residuals(HabitatBR)# extract residuals
residualPlots(HabitatBR)#plot residuals
plot(simulateResiduals(primaryac,  n = 1000), rank = TRUE)

#get summary
summary(primary)
summary(HabitatBR)

#get means for each category
aggregate(x = Presence_with_covs$Presence,                # Specify data column
          by = list(Presence_with_covs$Habitat.Order),              # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)

#posthoc tests
anova(HabitatBR, test="F")
anova(primary, test="Chisq")
car::Anova(primary, test="Wald")
summary(glht(HabitatBR, linfct = mcp(Habitat.Order = "Tukey")))

lsmeans(HabitatBR,pairwise ~ Habitat.Order,
        adjust="none",
        type="response")

#test VIF
car::vif(HabitatBR)

#Model selection
AIC(HabitatBRac,HabitatBR)
lrtest(HabitatBRac,HabitatBR)

#bbonferonis pairwise corection habitat
pvals = c(0, 0, 0, 0.54, 0.0003, 0.83, 0.76, 0.0, 0.40, 0.92,0,0.53, 0, 0,59, 0.0002)

adjusted<-p.adjust(pvals, method = "bonferroni")# bonferoni p values
adjusted

#_____________________________________________________________________________________________________________________
#glms with continous covariates
#without ac
Allglm <- glm(Presence ~ ForestCover100^2+Secondary200km++House1000km, family = binomial,
              data = Presence_with_covs)
summary(Allglm)

#with ac
Allglmac <- glm(Presence ~ ForestCover100^2+Secondary200km++House1000km+ac, family = binomial,
                data = Presence_with_covs)
summary(Allglmac)

#test VIF
car::vif(Allglm)

#extract residuals and estimates
confint(Allglmac)#gets CIs of model variables
exp(coef(Allglmac))#gets actual estimtes from model
coef(Allglmloc)#gets logs odds of model
coef(Allglm)
plotResiduals(Allglmac)
plotQQunif(Allglmac)

#gets confidence intervals
confint(Allglm,level=0.95)


#gets the range of values
range(Allglmac$ForestCover200)

#post hoc tests
anova(Allglmac, test="F")
anova(Allglmac, test="Chisq")
car::Anova(Allglmac, test="Wald")
r.squaredGLMM(Allglmac)
r2(Allglmac)

#model selection

AIC(Allglm,Allglmac)
lrtest(Allglm,Allglmac)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Prediction~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#This predicts a value for ever site
glm.probs <- predict(Allglm,type = "response")
glm.probs

#ggpredict
ggpredict(Allglmac, "Secondary200km[all]")
ggpredict(Allglmac, "House1000km")
ggpredict(Allglmac, "Primary1000km[all]")
ggpredict(Allglmac, "ForestCover200[all]")

########predict with one model variable
xforest<-seq(0,1,0.1)#make a seq between these values
#make predictions based on that seq
yforest <- predict(Allglm, list(ForestCover200 = xforest),type="response")
yforest
#plot 
plot(ForestCover200, Presence, pch = 16, xlab = "Forest Cover (%)", ylab = "Occupancy", data=Presence_with_covs)
lines(xforest, yforest)
plot(ForestCover200, Presence)

#effects package plotting
plot(allEffects(Allglmac))#makes plot
e.out <- allEffects(Allglmac)#extracts values for means to use below
e.out$Secondary_Road_km$model.matrix

#predict function for multiple variables
#make new data
ndata <- data.frame(ForestCover100=(c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)),  Secondary200km=0.18, House1000km=6.72, ac=5.5)
ndata <- data.frame(Secondary200km=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1),ForestCover100=0.70, House1000km=6.72, ac=5.5)
ndata <- data.frame(House1000km=c(0,0.25,0.5,1,2,3,4,5,10,15,20,25,30,35,40, 45,50,55,60,65,70,75,80,85,90,95,100,110),ForestCover100 = 0.70, Secondary200km=0.18, ac=5.5)
ndata <- data.frame(Primary1000km=(c(0,0.1,0.25,0.5,0.75,1,2,3,4,5,6,7,8)))
ndata
#predict based on these values
prim_pred<-predict(primary, newdata = ndata, se.fit=TRUE,type = "response",interval = "confidence",level=0.95)
as.data.frame(prim_pred)

write.csv(sec_pred, "binomial_primary_predict_new.csv")

#___________________________Variance partitioning___________________________________________________

p<- read.csv("varpart_binom.csv", header=TRUE)
env<- read.csv("varpart_binom_env.csv", header=TRUE)
env<- read.csv("envreduced.csv", header=TRUE)
p<-p[,-1]#remove first column

devtools::install_github("Stan125/ghp")
hier.part(p, env,
          family = c( "binomial"),
          link = c("cloglog"),
          gof = c("RMSPE"),
          barplot = TRUE)



#______________________________________adding autocovariate_____________________________________________________________________
# define cell coordinates 
coords <- as.matrix(cbind(Presence_with_covs$Lat, Presence_with_covs$Long))

# construct autcovariate - increase neigbourhood dist (nbs) by increments of 0.1 till no cells have zero neighbours
ac <- autocov_dist(as.numeric(Presence_with_covs$Presence), coords, nbs = 2.1, longlat = TRUE)
# combine with cell coordinates
AC <- data.frame(ac = ac, x = Presence_with_covs$Lat, y = Presence_with_covs$Long)
Presence_with_covs<-cbind(Presence_with_covs, ac)


#______________________________Testing for spatial autocorrelation_________________________________________________________________________________________________

E <-resid(Allglm)#this extracts the resuduals from the gamm model
spatialdata <- data.frame(E, Long, Lat)# creates a dataframe wth residuals and coordinates
coordinates(spatialdata) <- c("Lat","Long")#defines them as coorinates
bubble(spatialdata, "E", col = c("purple","blue"),
       main = "Residuals", xlab = "X-coordinates",
       ylab = "Y-coordinates")

#making a variogram for data
Vario <- variogram(E ~ 1, spatialdata)
plot(Vario)

#alternative way to plot variogram
Variogram(Allglm,form=~Lat + Long)
plot(Variogram(Allglm,form=~Lat + Long))

####morans I for spatial autocorrelation
spatialdata <- data.frame(E,Long, Lat)# creates a dataframe wth just PC1 and coordinates
###set up
distances<-as.matrix(dist(cbind(spatialdata$Lat, spatialdata$Long)))
distances_Inverse<-1/distances
distances_Inverse[is.infinite(distances_Inverse)] <- 0

#test 
Moran.I(spatialdata$E, distances)

