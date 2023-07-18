## remove all variables
rm(list=ls())


## set working directory
wdir = "C:/Users/wurmf/Dropbox/Github/PRT_ddm_online/"
# wdir = "D:/Dropbox/GitHub/rddm/" #home desktop
setwd(wdir)

## load packages
library(ggplot2)
library(gridExtra)
library(grid)
library(emmeans)
library(ggpubr)
library(cowplot)
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
        legend.position=c(1.4,0.3),
        legend.text = element_text(size=12,face='plain'),
        plot.title = element_text(size=15,face='bold'),
        axis.title= element_text(size=12,face='plain'),
        axis.text = element_text(size=12,face='plain'))




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
        legend.position="none",
        legend.text = element_text(size=12,face='plain'),
        plot.title = element_text(size=15,face='bold'),
        axis.title= element_text(size=12,face='plain'),
        axis.text = element_text(size=12,face='plain'))

#####
# interaction block & fbType
test = emmeans(regfits[[iP]], list(pairwise ~ fbType*Block), adjust = "tukey")
y = summary(test)
ggp = y$`emmeans of fbType, Block`

p3 = ggplot(ggp, aes(x=Block, y=emmean, colour=fbType, group=fbType)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), colour="black", width=.1, position=pd) +
  geom_line(position=pd,size=1) +
  geom_point(position=pd, size=5, shape=21, fill="white") + # 21 is filled circle
  xlab("Block number") +
  scale_x_discrete(labels= c(1,2,3)) +
  ylab("Estimated parameter value") +
  scale_colour_hue(name="Feedback type",    # Legend label, use darker colors
                   breaks=c("fb1", "fb2", "fb3", "fb4"),
                   labels=c("star", "verbal","face", "thumb"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Interaction feedback and block") +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1.3,0.1),
        legend.text = element_text(size=12,face='plain'),
        plot.title = element_text(size=15,face='bold'),
        axis.title= element_text(size=12,face='plain'),
        axis.text = element_text(size=12,face='plain'))


gt <- arrangeGrob(p1,p2,                               # bar plot spaning two columns
                  p3,                               # box plot and scatter plot
                  ncol = 5, nrow = 5, 
                  layout_matrix = rbind(c(1,1,NA,2,2), c(NA,3,3,3,NA)))
# Add labels to the arranged plots
pX <- as_ggplot(gt) +                                # transform to a ggplot
  draw_plot_label(label = c("A", "B", "C"), size = 18,
                  x = c(0, 0.6, 0.2), y = c(1, 1, 0.5)) # Add labels
pX

grid.arrange(
  grobs = p1,p2,p3,
  widths = c(2, 2),
  layout_matrix = rbind(c(1, 2),
                        c(3, 3))
)
grid.arrange(p1, p2,p3, nrow = 2)


gs <- lapply(1:3, function(ii) 
  grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)), textGrob(ii)))
lay <- rbind(c(1,2),
             c(3,3))
grid.arrange(grobs = gs, layout_matrix = lay)


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



