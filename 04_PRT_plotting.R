## remove all variables
rm(list=ls())


## set working directory
wdir = "C:/Users/wurmf/Dropbox/Github/PRT_ddm/"
# wdir = "D:/Dropbox/GitHub/rddm/" #home desktop
setwd(wdir)

## load packages
library(ggplot2)
library(gridExtra)
library(emmeans)
# library(rstan)
# library(RWiener)
# library(MASS)
# library(stats) #for posthoc tests
# library(lme4)
# library(lmerTest)
# library(performance)
# 
# library(pbkrtest)
# library(bayestestR)


model = "PRT_ddm_final"
fbtype = c('fb1','fb2','fb3','fb4')
blocks = c('block1','block2','block3')

## prepare figures
pd <- position_dodge(0.1) # move them .05 to the left and right

## load data
load(paste(wdir,"/model/data_",model,".RData",sep=''))
load(paste(wdir,"/model/regression_",model,".RData",sep=''))


#####
## Drift rate
#####
iP = 1


#####
# main effect block
test = emmeans(regfits[[iP]], list(pairwise ~ Stimulus*Block), adjust = "tukey")
y = summary(test)
ggp = y$`emmeans of Stimulus, Block`

p1 = ggplot(ggp, aes(x=Block, y=emmean, colour=Stimulus, group=Stimulus)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), colour="black", width=.1, position=pd) +
  geom_line(position=pd,size=2) +
  geom_point(position=pd, size=5, shape=21, fill="white") + # 21 is filled circle
  xlab("Block number") +
  scale_x_discrete(labels= c(1,2,3)) +
  ylab("Estimated parameter value") +
  scale_colour_hue(name="Stimulus type",    # Legend label, use darker colors
                   breaks=c("1", "2"),
                   labels=c("rich", "lean"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Drift rate\nMain effect block number") +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position="none",
        legend.text = element_text(size=12,face='plain'),
        plot.title = element_text(size=18,face='bold'),
        axis.title= element_text(size=16,face='plain'),
        axis.text = element_text(size=16,face='plain'))




#####
# main effect fbType
test = emmeans(regfits[[iP]], list(pairwise ~ Stimulus*fbType), adjust = "tukey")
y = summary(test)
ggp = y$`emmeans of Stimulus, fbType`

p2 = ggplot(ggp, aes(x=fbType, y=emmean, colour=Stimulus, group=Stimulus)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), colour="black", width=.1, position=pd) +
  geom_line(position=pd,size=2) +
  geom_point(position=pd, size=5, shape=21, fill="white") + # 21 is filled circle
  xlab("Feedback type") +
  scale_x_discrete(labels= c('star','verbal','face','thumb')) +
  ylab("Estimated parameter value") +
  scale_colour_hue(name="Stimulus type",    # Legend label, use darker colors
                   breaks=c("1", "2"),
                   labels=c("rich", "lean"),
                   l=40) +                    # Use darker colors, lightness=
  ggtitle("Drift rate\nMain effect feedback type") +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0.8),
        legend.text = element_text(size=12,face='plain'),
        plot.title = element_text(size=18,face='bold'),
        axis.title= element_text(size=16,face='plain'),
        axis.text = element_text(size=16,face='plain'))

grid.arrange(p1, p2, nrow = 1)


######
##TEST - boxplot

ggplot(Data[[iP]], aes(x=Block, y=Y, fill=Stimulus)) + 
  geom_boxplot() +
  xlab("Block number") +
  scale_x_discrete(labels= c(1,2,3)) +
  ylab("Estimated parameter value") +
  scale_fill_discrete(name = "Stimulus type",
                      labels = c("rich","lean"),
                      l = 40) +# Use darker colors, lightness=40
  ggtitle("Drift rate\nMain effect block number") +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.text = element_text(size=12,face='plain'),
        plot.title = element_text(size=18,face='bold'),
        axis.title= element_text(size=16,face='plain'),
        axis.text = element_text(size=16,face='plain'))



