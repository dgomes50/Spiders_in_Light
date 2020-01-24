


# > sessionInfo()
# R version 3.5.1 (2018-07-02)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
#   [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
#   [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
#   [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] cowplot_0.9.3 ggpubr_0.1.8  magrittr_1.5  ggplot2_3.0.0 DHARMa_0.2.4  glmmTMB_0.2.3 dplyr_0.8.1  


##### Load Libraries: ####
library(dplyr)
library(glmmTMB)
library(DHARMa)
library("ggpubr")
library(grid)
library(cowplot)

#### LOAD DATA ####

### Abundance
TOURS<-read.csv("TOURS.csv")
TOURS$Light[TOURS$Light=="0"]<-"No light"
TOURS$Light[TOURS$Light=="1"]<-"Light"

### WEBS
WEB<-read.csv("webs.csv")
WEB$Area=pi*(WEB$Dv/2)*(WEB$Dh/2)-(pi*(WEB$Dfz/2)^2) # calculate web catch area
WEB$Asy<-(WEB$Rt-(WEB$Dv-WEB$Rt))/WEB$Dv # calculate web asymmetry
WEB$Prey[which(WEB$Prey=="NaN")]<-NA

### Body Condition
SP<-read.csv("BC.csv")
SP$Wt<-SP$weight.g. ### Wt is measured in grams (g) # clean up names
SP$Fe<-SP$femur.mm. ### Fe is measured in milimeter (mm) # clean up names
SP$cond<-as.character(SP$cond)
SP$cond[which(SP$cond=="no_light")]<-("No Light") # clean up names
SP$cond[which(SP$cond=="light")]<-("Light") # clean up names
SP$cond<-as.factor(SP$cond)

# 
# 
# 

### Abundance data ####

## Summary Statistics for ABUNDANCE
Abun<-group_by(TOURS, Light) %>%
  dplyr::summarise(
    count = n(),
    total = sum(Spiders),
    mean = mean(Spiders, na.rm=TRUE),
    SD = sd(Spiders, na.rm=TRUE),
    median = median(Spiders, na.rm = TRUE),
    IQR = IQR(Spiders, na.rm = TRUE),
    min_range = min(Spiders, na.rm=TRUE),
    max_range = max(Spiders, na.rm=TRUE)
      )

Abun

## Statistical testing of abundance data:

# plot(density(TOURS$Spiders)) # look at distribution
# var(TOURS$Spiders)/mean(TOURS$Spiders) # overdispersed

mod1<-glmmTMB(Spiders ~ Light, family=nbinom1(link="log"), data=TOURS)
# plot(simulateResiduals(mod1)) # check residuals = good fit

# check model output:
summary(mod1)

#95% confidence interval
# exp(0.32024+1.96*0.09572)  # Upper 95% CI
# exp(0.32024-1.96*0.09572)  # Lower 95% CI
# exp(0.32024) # Parameter estimate



### WEB MEASUREMENTS ####

### WEB AREA ###

## summary stats
Web<-group_by(WEB, Condition) %>%
  dplyr::summarise(
    count = n(),
    median = median(Area, na.rm = TRUE),
    mean = mean(Area, na.rm=TRUE),
    SD = sd(Area, na.rm=TRUE)
  )
Web

# plot(density(WEB$Area)) # look at distribution
# var(WEB$Area)/mean(WEB$Area) # Overdispersed?

## Statistical testing
mod2<-glm(Area~Condition,family=Gamma(link="log"),data=WEB)
# plot(simulateResiduals(mod2)) # check residual plots

summary(mod2)

# exp(0.43308) # parameter estimate
# exp(0.43308+ 1.96*0.12917) # Upper 95% CI
# exp(0.43308- 1.96*0.12917) # Lower 95% CI



### VERTICAL WEB ASYMMETRY ###

# plot(density(WEB$Asy)) # look at distribution

WEB$Condition<-relevel(WEB$Condition,"No Light")
mod3<-lm(Asy ~ Condition + Area, data=WEB)

# qqnorm(resid(mod3)) # check residual plots
# qqline(resid(mod3)) # check residual plots

summary(mod3)
confint(mod3)

## summary stats VWA
group_by(WEB, Condition) %>%
  summarise(
    count = n(),
    median = median(Asy, na.rm = TRUE),
    mean = mean(Asy, na.rm=TRUE),
    SD = sd(Asy, na.rm=TRUE)
  )




### PREY CAPTURE ####
plot(density(PREY$Prey)) # plot data distribution
PREY<-WEB[!is.na(WEB$Prey),] # remove NA to look at dispersion
var(PREY$Prey)/mean(PREY$Prey) # overdispersed
    
mod4<-glmmTMB(Prey ~  Condition+Area, data=PREY, family=nbinom2(link="log"))
# plot(simulateResiduals(mod4)) # check residual plots

summary(mod4) 

# exp(1.9376593) # parameter estimate
# exp(1.9376593+1.96*0.1894091) # Upper 95% CI
# exp(1.9376593-1.96*0.1894091) # Lower 95% CI

## Prey capture summary statistics
group_by(WEB,Condition) %>% 
  summarize(
    count=length(which(!is.na(Prey))),
    mean=mean(Prey,na.rm=TRUE),
    SD = sd(Prey, na.rm=TRUE))




## Body Condition ####

## Summary stats for body condition
group_by(SP, cond) %>% 
  summarise(
    count = n(),
    meanWt=mean(Wt),
    meanFe=mean(Fe)
  )

## Generate residuals for Body Condition (following Jakob et al. 1996)
plot(Wt~Fe, data=SP)

Bod.lm = lm(Wt~Fe, data=SP) # run lm for residuals
# plot(Bod.lm) # visualize

SP$res<-resid(Bod.lm) # get residuals into original dataset

## Summary stats for Residuals
Res<-group_by(SP, cond) %>% 
  dplyr::summarise(
    count = n(),
    mean=mean(res),
    SD = sd(res),
    median = median(res, na.rm = TRUE),
    IQR = IQR(res, na.rm = TRUE),
    min_range = min(res, na.rm=TRUE),
    max_range = max(res, na.rm=TRUE)
  ) 

# Res #View summary stats

# plot(density(SP$res)) # look at distribution

lm1<-lm(res ~ cond, data=SP)
# qqnorm(resid(lm1))  # check residual plots
# qqline(resid(lm1))

summary(lm1)
confint(lm1)

lm2<-lm(Fe ~ cond, data=SP)
# qqnorm(resid(lm2)) # check residual plots
# qqline(resid(lm2))

summary(lm2)
confint(lm2)

# Summary table of body size (Femur length)
group_by(SP, cond) %>% 
  dplyr::summarise(
    count = n(),
    mean=mean(Fe),
    SD = sd(Fe),
    median = median(Fe, na.rm = TRUE),
    IQR = IQR(Fe, na.rm = TRUE),
    min_range = min(Fe, na.rm=TRUE),
    max_range = max(Fe, na.rm=TRUE)
  ) 

 
 



#### Graphical Visualization of Data ####

## Figure 3
### Visualization of abundance data 

grob <- grobTree(textGrob("*", x=0.475,  y=0.9, hjust=0,
                          gp=gpar(col="black", fontsize=36, fontface="bold")))

A<-ggboxplot(data=TOURS, x = "Light", y = "Spiders", fill = "Light", palette = c("#00AFBB", "#FC4E07"),
             ylab ="Spiders / \nbridge panel \n", xlab = "", legend="FALSE")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  scale_y_continuous(breaks=c(5,10,15,20,25))+
  annotation_custom(grob)

### Web Area

B<-ggboxplot(data=WEB, x = "Condition", y = "Area", fill = "Condition", order=c("No Light","Light"),palette = c("#00AFBB", "#FC4E07"),
             ylab = expression(paste('Web area ',(cm^2))), xlab = "", legend="FALSE")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  scale_y_continuous(breaks=c(250,500,750,1000,1250))+
  annotation_custom(grob)


### Body Condition plot 
C<-ggboxplot(data=SP, x = "cond", y = "res", fill = "cond", order=c("No Light","Light"), 
             palette = c("#00AFBB", "#FC4E07"),
             ylab ="Body Condition\n(Residual Index) \n", xlab = "\nCondition", legend="FALSE")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  geom_hline(yintercept=0,linetype=2)+
  scale_y_continuous(breaks=c(-0.05,0.00,0.05), limits = c(-.05,.05))+
  annotation_custom(grob)

grob <- grobTree(textGrob("N.S.", x=0.435,  y=0.9, hjust=0,
                          gp=gpar(col="black", fontsize=28, fontface="bold")))

SP$sFe<-scale(SP$Fe)
D<-ggboxplot(data=SP, x = "cond", y = "sFe", fill = "cond", order=c("No Light","Light"), 
             palette = c("#00AFBB", "#FC4E07"),
             ylab ="Body Size\n(Scaled Femur length) \n", xlab = "\nCondition", legend="FALSE")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  geom_hline(yintercept=0,linetype=2)+
  annotation_custom(grob)
                       

## Making final plot of Figure 3 - combining ggplots in a grid of 3 panels

plot_grid(A +rremove("x.text"),B+rremove("x.text"),C,D, ncol=2,align="hv",labels = "AUTO",label_x=.1,label_y=1,hjust=-3, label_size=22)
# 
# 
# 
# 
# 
# 
# 
# 

### Figure 4
Asy1<-ggplot(WEB,aes(Area,Asy, color=Condition, fill=Condition))+
  geom_point(size=2)+
  labs(color="Legend text")+
  geom_smooth(method='lm', se=F)+
  xlab(expression(paste('\nWeb catch area ',(cm^2))))+
  ylab("Vertical web asymmetry\n")+
  guides(fill=FALSE)+
  theme_classic()+
  geom_hline(yintercept=0,linetype=2)+
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14),
          legend.position = c(0.7, 0.7),
          legend.text = element_text(size=14),
          legend.title = element_blank())


# 
# 
# 
# 
# 

### Figure 5
prey1<-ggplot(WEB,aes(Area,Prey, color=Condition, fill=Condition))+
  geom_point(size=2)+
  labs(color="Legend text")+
  geom_smooth(method="lm", alpha=.1, formula=y~x, size=1.1,
              fullrange=F, se=F)+
  xlab(expression(paste('Web catch area ',(cm^2))))+
  ylab("Number of captured prey per web\n")+
  guides(fill=FALSE)+
  theme_classic()+
  scale_y_continuous(breaks=seq(0,100,by=10))+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=18),
        legend.position = c(0.85, 0.8),
        legend.text = element_text(size=20),
        legend.title = element_blank())

grob <- grobTree(textGrob("*", x=0.475,  y=0.9, hjust=0,
                          gp=gpar(col="black", fontsize=36, fontface="bold")))

prey2<-ggboxplot(data=WEB, x = "Condition", y = "Prey", fill = "Condition", order=c("No Light","Light"),palette = c("#00AFBB", "#FC4E07"),
                ylab ="", xlab = "", legend="FALSE")+
  scale_y_continuous(breaks=seq(0,100,by=10))+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  annotation_custom(grob)

plot_grid(prey1,prey2, align="hv")
