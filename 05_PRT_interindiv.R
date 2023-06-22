## remove all variables
rm(list=ls())


## set working directory
wdir = "C:/Users/wurmf/Dropbox/Github/PRT_ddm/"
# wdir = "D:/Dropbox/GitHub/rddm/" #home desktop
setwd(wdir)

## load packages
library(rstan)
library(RWiener)
library(MASS)
library(stats) #for posthoc tests
library(ggplot2)
library(gridExtra)
library(lme4)
library(lmerTest)
library(performance)
library(emmeans)
library(pbkrtest)
library(readxl)
library(ggpubr)
library(bayesplot)

## specify model
model = "PRT_ddm_final"
fbtype = c('fb1','fb2','fb3','fb4')
blocks = c('block1','block2','block3')

## load data from questionnaires
Qdata = read_excel(paste(wdir,'data/PRTdata_questionnaires','.xlsx',sep = ''))
Qdata_split = split(Qdata,Qdata$VersionPRT)

## prepare data
#fit <- readRDS(paste(wdir,'fits/fit_',model,'_',fbtype[[1]],'.rds',sep=''))
list_of_draws <- c("alpha1","alpha2","delta1","delta2","tau1","tau2","zeta")
#preidx <- lapply(list_of_draws, function(ch) grep("sbj", ch)) #identify parameters with "sbj"
#idx = sapply(preidx, function(x) length(x) > 0) #transform to index
paranms = list_of_draws
#paranms = print(list_of_draws[idx])
#paranms_short = gsub("_sbj", "", paranms)


## load data
datdir = paste(wdir,'fits/',sep = '')
nBlock = 3
fitvals = list()
nLength = c()
nSbj = c()
for (i in 1:length(fbtype)) {
  message(paste('feedback',i))
  fit <- readRDS(paste(wdir,'model/fit_',model,'_',fbtype[[i]],'.rds',sep=''))
  
  
  for (iP in 1:length(paranms)) {
    message(paste('   parameter',paranms[iP]))
    
    predat = as.matrix(fit, pars = paranms[iP])
    meandat = apply(predat, c(2), mean)
    fitvals[[fbtype[i]]][[paranms[iP]]] = meandat
    
    nLength[i] = length(meandat)
    nSbj[i] = nLength[i]/length(blocks)
  }
}


scales = c('SHAPS_SUM', 'TEPS_ANT', 'TEPS_CON', 'TEPS_TOTAL', 'PANAS_POS', 'PANAS_NEG', 'BAQ_SUM')
scales = c('hungry', 'thirsty')

scales = c('SHAPS_SUM', 'TEPS_ANT', 'TEPS_CON',
           'TEPS_TOTAL', 'PANAS_POS', 'PANAS_NEG',
           'BAQ_SUM', 'hungry', 'thirsty')



target = 'zeta'
preidx <- lapply(paranms, function(ch) grep(paste('^',target,'.*$',sep=''), ch)) #identify parameters with "sbj"
idx = sapply(preidx, function(x) length(x) > 0) #transform to index
newidx = which(idx==TRUE)

if (length(newidx)==1) {
  
  Data = data.frame(
    Y = c(fitvals[[1]][[newidx]],fitvals[[2]][[newidx]],fitvals[[3]][[newidx]],fitvals[[4]][[newidx]]),
    Block = factor(rep(blocks,times = sum(nSbj))),
    fbType = factor(rep(fbtype,times = nSbj)),
    Subject = factor(rep(1:sum(nSbj), each = nBlock))
  )
  
  y = with(Data,tapply(Y,Subject,mean))
  Data2 = data.frame(ymean = y, fbType = factor(rep(fbtype,times = nSbj)))
  
  
  for (i in 1:length(scales)) {
    Q = c(Qdata_split[[1]][[scales[i]]],Qdata_split[[2]][[scales[i]]],Qdata_split[[3]][[scales[i]]],Qdata_split[[4]][[scales[i]]])
    Data2[scales[i]] = Q
  }
  
  p <- vector(mode = "list", length = length(scales))
  
  for (i in 1:length(scales)) {
    p[[i]] = ggscatter(Data2, x = "ymean", y = scales[i], 
                       add = "reg.line", conf.int = TRUE, 
                       cor.coef = TRUE, cor.method = "pearson",
                       xlab = "y", ylab = "score",title = scales[i])
  }

  
} else if (length(newidx)==2) {
  
  j = 1
  Data1 = data.frame(
    Y = c(fitvals[[1]][[newidx[j]]],fitvals[[2]][[newidx[j]]],fitvals[[3]][[newidx[j]]],fitvals[[4]][[newidx[j]]]),
    Stimulus = factor(1),
    Block = factor(rep(blocks,times = sum(nSbj))),
    fbType = factor(rep(fbtype,times = nSbj)),
    Subject = factor(rep(1:sum(nSbj), each = nBlock))
  )
  j = 2
  Data2 = data.frame(
    Y = c(fitvals[[1]][[newidx[j]]],fitvals[[2]][[newidx[j]]],fitvals[[3]][[newidx[j]]],fitvals[[4]][[newidx[j]]]),
    Stimulus = factor(2),
    Block = factor(rep(blocks,times = sum(nSbj))),
    fbType = factor(rep(fbtype,times = nSbj)),
    Subject = factor(rep(1:sum(nSbj), each = nBlock))
  )
  Data_all = rbind(Data1,Data2)
  
  Data = data.frame(
    Y1 = c(fitvals[[1]][[newidx[1]]],fitvals[[2]][[newidx[1]]],fitvals[[3]][[newidx[1]]],fitvals[[4]][[newidx[1]]]),
    Y2 = c(fitvals[[1]][[newidx[2]]],fitvals[[2]][[newidx[2]]],fitvals[[3]][[newidx[2]]],fitvals[[4]][[newidx[2]]]),
    Block = factor(rep(blocks,times = sum(nSbj))),
    fbType = factor(rep(fbtype,times = nSbj)),
    Subject = factor(rep(1:sum(nSbj), each = nBlock))
  )
  
  y1 = with(Data,tapply(Y1,Subject,mean))
  y2 = with(Data,tapply(Y2,Subject,mean))
  ydiff = y2-y1
  ymean = aggregate(Data_all$Y,list(Data_all$Subject),mean)
  
  Data2 = data.frame(y1 = y1, y2 = y2, ydiff = ydiff, ymean = ymean$x, fbType = factor(rep(fbtype,times = nSbj)))

  for (i in 1:length(scales)) {
    Q = c(Qdata_split[[1]][[scales[i]]],Qdata_split[[2]][[scales[i]]],Qdata_split[[3]][[scales[i]]],Qdata_split[[4]][[scales[i]]])
    Data2[scales[i]] = Q
  }
  
  p <- vector(mode = "list", length = length(scales))
  
  for (i in 1:length(scales)) {
    # p[[i]] = ggscatter(Data2, x = "y1", y = scales[i],
    #                    add = "reg.line", conf.int = TRUE,
    #                    cor.coef = TRUE, cor.method = "pearson",
    #                    xlab = "parameter(rich)", ylab = "score",title = scales[i])
    # p[[i]] = ggscatter(Data2, x = "y2", y = scales[i],
    #                    add = "reg.line", conf.int = TRUE,
    #                    cor.coef = TRUE, cor.method = "pearson",
    #                    xlab = "parameter(lean)", ylab = "score",title = scales[i])
    # p[[i]] = ggscatter(Data2, x = "ydiff", y = scales[i],
    #                    add = "reg.line", conf.int = TRUE,
    #                    cor.coef = TRUE, cor.method = "pearson",
    #                    xlab = "parameter(lean-rich)", ylab = "score",title = scales[i])
    p[[i]] = ggscatter(Data2, x = "ymean", y = scales[i],
                       add = "reg.line", conf.int = TRUE,
                       cor.coef = TRUE, cor.method = "pearson",
                       xlab = "parameter(mean)", ylab = "score",title = scales[i])
  }
  
  
}



ggarrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],p[[8]],p[[9]],
              labels = c("A", "B", "C", "D", "E", "F","G","H"),
              ncol = 3, nrow = 3)

ggarrange(p[[1]],p[[2]],
          labels = c("A", "B"),
          ncol = 2, nrow = 1)



scales = c('hungry', 'thirsty')
for (i in 1:length(scales)) {
  Q = c(Qdata_split[[1]][[scales[i]]],Qdata_split[[2]][[scales[i]]],Qdata_split[[3]][[scales[i]]],Qdata_split[[4]][[scales[i]]])
  Data2[scales[i]] = Q
}
newdata <- subset(Data2, fbType=="fb4")
for (i in 1:length(scales)) {
  # p[[i]] = ggscatter(Data2, x = "y1", y = scales[i],
  #                    add = "reg.line", conf.int = TRUE,
  #                    cor.coef = TRUE, cor.method = "pearson",
  #                    xlab = "parameter(rich)", ylab = "score",title = scales[i])
  # p[[i]] = ggscatter(Data2, x = "y2", y = scales[i],
  #                    add = "reg.line", conf.int = TRUE,
  #                    cor.coef = TRUE, cor.method = "pearson",
  #                    xlab = "parameter(lean)", ylab = "score",title = scales[i])
  # p[[i]] = ggscatter(Data2, x = "ydiff", y = scales[i],
  #                    add = "reg.line", conf.int = TRUE,
  #                    cor.coef = TRUE, cor.method = "pearson",
  #                    xlab = "parameter(lean-rich)", ylab = "score",title = scales[i])
  p[[i]] = ggscatter(newdata, x = "ymean", y = scales[i],
                     add = "reg.line", conf.int = TRUE,
                     cor.coef = TRUE, cor.method = "pearson",
                     xlab = "parameter(mean)", ylab = "score",title = scales[i])
}
ggarrange(p[[1]],p[[2]],
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


