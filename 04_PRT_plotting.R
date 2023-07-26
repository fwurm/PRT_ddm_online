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
library(lemon)
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
iP = 2


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

ggplot2::ggsave('FINAL-driftRate.png',path = paste(wdir,"/figures",sep=''),dpi=300)




#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################


#####
## Boundary separation
#####
iP = 1


#####
# main effect block
test = emmeans(regfits[[iP]], list(pairwise ~ Stimulus*Block), adjust = "tukey")
y = summary(test)
ggp = y$`emmeans of Stimulus, Block`

p4 = ggplot(ggp, aes(x=Block, y=emmean, colour=Stimulus, group=Stimulus)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), colour="black", width=.1, position=pd) +
  geom_line(position=pd,size=2) +
  geom_point(position=pd, size=5, shape=21, fill="white") + # 21 is filled circle
  xlab("Block number") +
  scale_x_discrete(labels= c(1,2,3)) +
  ylab("Estimated parameter value") +
  ylim(1.15,1.32) +
  scale_colour_hue(name="Stimulus type",    # Legend label, use darker colors
                   breaks=c("1", "2"),
                   labels=c("rich", "lean"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Boundary separation\nMain effect block number") +
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

p5 = ggplot(ggp, aes(x=fbType, y=emmean, colour=Stimulus, group=Stimulus)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), colour="black", width=.1, position=pd) +
  geom_line(position=pd,size=2) +
  geom_point(position=pd, size=5, shape=21, fill="white") + # 21 is filled circle
  xlab("Feedback type") +
  scale_x_discrete(labels= c('star','verbal','face','thumb')) +
  ylab("Estimated parameter value") +
  ylim(1.15,1.32) +
  scale_colour_hue(name="Stimulus type",    # Legend label, use darker colors
                   breaks=c("1", "2"),
                   labels=c("rich", "lean"),
                   l=40) +                    # Use darker colors, lightness=
  ggtitle("  \nMain effect feedback type") +
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

p6 = ggplot(ggp, aes(x=Block, y=emmean, colour=fbType, group=fbType)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), colour="black", width=.1, position=pd) +
  geom_line(position=pd,size=1) +
  geom_point(position=pd, size=5, shape=21, fill="white") + # 21 is filled circle
  xlab("Block number") +
  scale_x_discrete(labels= c(1,2,3)) +
  ylab("Estimated parameter value") +
  ylim(1.13,1.33) +
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


gt <- arrangeGrob(p4,p5,                               # bar plot spaning two columns
                  p6,                               # box plot and scatter plot
                  ncol = 5, nrow = 5, 
                  layout_matrix = rbind(c(1,1,NA,2,2), c(NA,3,3,3,NA)))
# Add labels to the arranged plots
pX <- as_ggplot(gt) +                                # transform to a ggplot
  draw_plot_label(label = c("A", "B", "C"), size = 18,
                  x = c(0, 0.6, 0.2), y = c(1, 1, 0.5)) # Add labels
pX

ggplot2::ggsave('FINAL-boundSep.png',path = paste(wdir,"/figures",sep=''),dpi=300)



#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################


#####
## Non-decision Time
#####
iP = 3


#####
# main effect block
test = emmeans(regfits[[iP]], list(pairwise ~ Stimulus*Block), adjust = "tukey")
y = summary(test)
ggp = y$`emmeans of Stimulus, Block`

p7 = ggplot(ggp, aes(x=Block, y=emmean, colour=Stimulus, group=Stimulus)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), colour="black", width=.1, position=pd) +
  geom_line(position=pd,size=2) +
  geom_point(position=pd, size=5, shape=21, fill="white") + # 21 is filled circle
  xlab("Block number") +
  scale_x_discrete(labels= c(1,2,3)) +
  ylab("Estimated parameter value") +
  ylim(0.234,0.275) +
  scale_colour_hue(name="Stimulus type",    # Legend label, use darker colors
                   breaks=c("1", "2"),
                   labels=c("rich", "lean"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Non-decision time\nMain effect block number") +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(0.95,0.8),
        legend.text = element_text(size=12,face='plain'),
        plot.title = element_text(size=15,face='bold'),
        axis.title= element_text(size=12,face='plain'),
        axis.text = element_text(size=12,face='plain'))




#####
# main effect fbType
test = emmeans(regfits[[iP]], list(pairwise ~ Stimulus*fbType), adjust = "tukey")
y = summary(test)
ggp = y$`emmeans of Stimulus, fbType`

p8 = ggplot(ggp, aes(x=fbType, y=emmean, colour=Stimulus, group=Stimulus)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), colour="black", width=.1, position=pd) +
  geom_line(position=pd,size=2) +
  geom_point(position=pd, size=5, shape=21, fill="white") + # 21 is filled circle
  xlab("Feedback type") +
  scale_x_discrete(labels= c('star','verbal','face','thumb')) +
  ylab("Estimated parameter value") +
  ylim(0.234,0.275) +
  scale_colour_hue(name="Stimulus type",    # Legend label, use darker colors
                   breaks=c("1", "2"),
                   labels=c("rich", "lean"),
                   l=40) +                    # Use darker colors, lightness=
  ggtitle("  \nMain effect feedback type") +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position="none",
        legend.text = element_text(size=12,face='plain'),
        plot.title = element_text(size=15,face='bold'),
        axis.title= element_text(size=12,face='plain'),
        axis.text = element_text(size=12,face='plain'))



gt <- arrangeGrob(p7,p8,                               # bar plot spaning two columns                               # box plot and scatter plot
                  ncol = 2, nrow = 2, 
                  layout_matrix = rbind(c(1,2)))
# Add labels to the arranged plots
pX <- as_ggplot(gt) +                                # transform to a ggplot
  draw_plot_label(label = c("A", "B"), size = 18,
                  x = c(0, 0.6), y = c(1, 1)) # Add labels
pX

ggplot2::ggsave('FINAL-nondecTime.png',path = paste(wdir,"/figures",sep=''),dpi=300)

#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################


#####
## Start bias
#####
iP = 4


#####
# main effect block
test = emmeans(regfits[[iP]], list(pairwise ~ Block), adjust = "tukey")
y = summary(test)
ggp = y$`emmeans of Block`

p9 = ggplot(ggp, aes(x=Block, y=emmean, group=1)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), colour="black", width=.1) +
  geom_line(size=2) +
  geom_point(size=5, shape=21, fill="white") + # 21 is filled circle
  xlab("Block number") +
  scale_x_discrete(labels= c(1,2,3)) +
  ylab("Estimated parameter value") +
  ylim(0.025,0.12) +           
  ggtitle("Start bias\nMain effect block number") +
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(0.95,0.8),
        legend.text = element_text(size=12,face='plain'),
        plot.title = element_text(size=15,face='bold'),
        axis.title= element_text(size=12,face='plain'),
        axis.text = element_text(size=12,face='plain'))




#####
# main effect fbType
test = emmeans(regfits[[iP]], list(pairwise ~ fbType), adjust = "tukey")
y = summary(test)
ggp = y$`emmeans of fbType`

p10 = ggplot(ggp, aes(x=fbType, y=emmean, group=1)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), colour="black", width=.1, position=pd) +
  geom_line(position=pd,size=2) +
  geom_point(position=pd, size=5, shape=21, fill="white") + # 21 is filled circle
  xlab("Feedback type") +
  scale_x_discrete(labels= c('star','verbal','face','thumb')) +
  ylab("Estimated parameter value") +
  ylim(0.025,0.12) +
  scale_colour_hue(name="Stimulus type",    # Legend label, use darker colors
                   breaks=c("1", "2"),
                   labels=c("rich", "lean"),
                   l=40) +                    # Use darker colors, lightness=
  ggtitle("  \nMain effect feedback type") +
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
  
p11 = ggplot(ggp, aes(x=Block, y=emmean, colour=fbType, group=fbType)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), colour="black", width=.1, position=pd) +
  geom_line(position=pd,size=1) +
  geom_point(position=pd, size=5, shape=21, fill="white") + # 21 is filled circle
  xlab("Block number") +
  scale_x_discrete(labels= c(1,2,3)) +
  ylab("Estimated parameter value") +
  ylim(-0.01,0.15) +
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



gt <- arrangeGrob(p9,p10,                               # bar plot spaning two columns
                  p11,                               # box plot and scatter plot
                  ncol = 5, nrow = 5, 
                  layout_matrix = rbind(c(1,1,NA,2,2), c(NA,3,3,3,NA)))
# Add labels to the arranged plots
pX <- as_ggplot(gt) +                                # transform to a ggplot
  draw_plot_label(label = c("A", "B", "C"), size = 18,
                  x = c(0, 0.6, 0.2), y = c(1, 1, 0.5)) # Add labels
pX

ggplot2::ggsave('FINAL-startBias.png',path = paste(wdir,"/figures",sep=''),dpi=300)





######
##TEST - boxplot
iP = 2

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

######
##TEST - violin + boxplot
iP = 2

dodge <- position_dodge(width = 0.9)
ggplot(Data[[iP]], aes(x=Block, y=Y, fill=Stimulus)) +
  geom_violin(position = dodge) +
  geom_boxplot(width = 0.3, position = dodge, color="black", alpha=0.2) +
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



######
##TEST - violin + single data points
iP = 2

block = Data[[iP]]$Block
fbtype = Data[[iP]]$fbType
subject = Data[[iP]]$Subject
stimulus = Data[[iP]]$Stimulus
value = Data[[iP]]$Y

df <- data.frame(block,fbtype,subject,stimulus,value,  U = interaction(stimulus,block))

dodge <- position_dodge(width = 0.5)
jitter = position_jitter(w=0.2, h=0)
colors <- c("#000000", "#3cb44b", "#000075", "#f032e6")

ggplot(df, aes(x=U, y=value, fill = stimulus)) +
  geom_violin() +
  geom_line(aes(group = subject, color = fbtype), alpha = 0.6, data = df,position=dodge) +
  scale_color_manual(values=colors)

