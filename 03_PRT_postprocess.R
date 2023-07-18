## remove all variables
rm(list=ls())


## set working directory
wdir = "C:/Users/wurmf/Dropbox/Github/PRT_ddm_online/"
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
library(bayestestR)
library(writexl)

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


model = "PRT_ddm_final"
fbtype = c('fb1','fb2','fb3','fb4')
blocks = c('block1','block2','block3')


#load exemplary fit
fbnumber = 1
fit <- readRDS(paste(wdir,'model/fit_',model,'_',fbtype[[fbnumber]],'.rds',sep=''))


## prepare for ANOVA
list_of_draws <- c("alpha1","alpha2","delta1","delta2","tau1","tau2","zeta")  #get all parameters
#preidx <- lapply(list_of_draws, function(ch) grep("sbj", ch)) #identify parameters with "sbj"
#idx = sapply(preidx, function(x) length(x) > 0) #transform to index
paranms = list_of_draws #print(list_of_draws[idx])

datdir = paste(wdir,'model/',sep = '')
nBlock = 3
fitvals = list()
nLength = c()
nSbj = c()
minRTs = c()
for (iFB in 1:length(fbtype)) {
  
  #load minRTs
  fn = paste(wdir,"data/data_fb",iFB,".Rda",sep = "")
  dat = loadRData(fn)
  Snums = unique(dat[["participant"]])
  minRT = vector(mode = "double", length = length(Snums))
  for (iS in 1:length(Snums)) {
    idx = dat[['participant']]==Snums[iS]
    minRT[iS]=min(dat$rt[idx])
  }
  minRTs = c(minRTs,minRT)
  
  message(paste('feedback',iFB))
  fit <- readRDS(paste(wdir,'model/fit_',model,'_',fbtype[[iFB]],'.rds',sep=''))
  
  for (iP in 1:length(paranms)) {
    message(paste('   parameter',paranms[iP]))
    
    predat = as.matrix(fit, pars = paranms[iP])
    meandat = apply(predat, c(2), mean)
    fitvals[[fbtype[iFB]]][[paranms[iP]]] = meandat
    
    nLength[iFB] = length(meandat)
    nSbj[iFB] = nLength[iFB]/length(blocks)
  }
}

# read parameters
target = c('alpha',  'delta', 'tau', 'zeta')
Data = list()
regfits = list()
for (iP in 1:length(target)) {
  preidx <- lapply(paranms, function(ch) grep(paste('^',target[iP],'.*$',sep=''), ch)) #identify parameters with "sbj"
  idx = sapply(preidx, function(x) length(x) > 0) #transform to index
  newidx = which(idx==TRUE)
  
  if (length(newidx)==1) {
    
    Data[[iP]] = data.frame(
      Y = c(fitvals[[1]][[newidx]],fitvals[[2]][[newidx]],fitvals[[3]][[newidx]],fitvals[[4]][[newidx]]),
      Block = factor(rep(blocks,times = sum(nSbj))),
      fbType = factor(rep(fbtype,times = nSbj*nBlock)),
      Subject = factor(rep(1:sum(nSbj), each = nBlock))
    )
    
    regfits[[iP]] = lmerTest::lmer(Y ~ Block*fbType + (1|Subject), data=Data[[iP]])
    #anova(fitF2)
    
  } else if (length(newidx)==2) {
    j = 1
    Data1 = data.frame(
      Y = c(fitvals[[1]][[newidx[j]]],fitvals[[2]][[newidx[j]]],fitvals[[3]][[newidx[j]]],fitvals[[4]][[newidx[j]]]),
      Stimulus = factor(1),
      Block = factor(rep(blocks,times = sum(nSbj))),
      fbType = factor(rep(fbtype,times = nSbj*nBlock)),
      Subject = factor(rep(1:sum(nSbj), each = nBlock))
    )
    j = 2
    Data2 = data.frame(
      Y = c(fitvals[[1]][[newidx[j]]],fitvals[[2]][[newidx[j]]],fitvals[[3]][[newidx[j]]],fitvals[[4]][[newidx[j]]]),
      Stimulus = factor(2),
      Block = factor(rep(blocks,times = sum(nSbj))),
      fbType = factor(rep(fbtype,times = nSbj*nBlock)),
      Subject = factor(rep(1:sum(nSbj), each = nBlock))
    )
    Data[[iP]] = rbind(Data1,Data2)
    
    #regfits[[iP]] = lmerTest::lmer(Y ~ Stimulus*Block*fbType + (1|Subject) + (1|Stimulus:Subject) + (1|Block:Subject), data=Data[[iP]])
    regfits[[iP]] = lmerTest::lmer(Y ~ Stimulus*Block*fbType + (1|Subject), data=Data[[iP]])
    #anova(regfits[[iP]])
    
  }
  
}

outfn = paste(wdir,'model/data_',model,'.RData',sep='')
save(Data, file = outfn)

outfn = paste(wdir,'model/regression_',model,'.RData',sep='')
save(regfits, file = outfn)


