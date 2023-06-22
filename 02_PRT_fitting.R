## remove all variables
rm(list=ls())

## set working directory
wdir = "D:/Dropbox/GitHub/rddm/" #home desktop
setwd(wdir)

## load packages
library(rstan)
library(RWiener)

## enable parallel processing
options(mc.cores = parallel::detectCores())

## enable automatic save of compiled code
rstan_options(auto_write = TRUE)

model = "PRT_ddm_final"
modeldir = paste('./models/modedl_',model,'.stan',sep = "")

nChain = 4
nIter = 4000
nWarmup = 2000
nThin = 1

###############################################
# fit model separately for each feedback type 
# (between-subjects design)
##############################################

########
# fb 1
########

## load behavioral data
fn = paste(wdir,"data_fb1.Rda",sep = "")
load(fn)

dat = df_v1

#prepare data
standata <- list(subnum = dat$participant,
                 blocknum = dat$block,
                 stim = ifelse(dat$stim == "rich", 1, 2),
                 rt = dat$rt,
                 corr = dat$corr,
                 nS = length(unique(dat[["participant"]])),
                 nB = length(unique(dat[["block"]])),
                 nT = length(dat$participant))

nS = length(unique(dat[["participant"]]))
nB = length(unique(dat[["block"]]))

# initial values for non-decision time
initf1 <- function() {
  list(mu_tau = array(0.15,dim = c(nB)),tau1 = array(0.15, dim = c(nB,nS)),tau2 = array(0.15, dim = c(nB,nS)))
}

# fit model
fit <- stan(file=modeldir, data = standata, chains = nChain, iter = nIter, warmup = nWarmup, thin = nThin, init = initf1)

#save model fits
fit@stanmodel@dso <- new("cxxdso")
saveRDS(fit, file = paste("fit_",model,"_fb1.rds",sep = ""))
rm(fit, standata)

##############################################

########
# fb 2
########

# load behavioral data
fn = paste(wdir,"data_fb2.Rda",sep = "")
load(fn)

dat = df_v2

standata <- list(subnum = dat$participant,
                 blocknum = dat$block,
                 stim = ifelse(dat$stim == "rich", 1, 2),
                 rt = dat$rt,
                 corr = dat$corr,
                 nS = length(unique(dat[["participant"]])),
                 nB = length(unique(dat[["block"]])),
                 nT = length(dat$participant))

nS = length(unique(dat[["participant"]]))
nB = length(unique(dat[["block"]]))

# initial values for non-decision time
initf2 <- function() {
  list(mu_tau = array(0.15,dim = c(nB)),tau1 = array(0.15, dim = c(nB,nS)),tau2 = array(0.15, dim = c(nB,nS)))
}

#fit model
fit <- stan(file=modeldir, data = standata, chains = nChain, iter = nIter, warmup = nWarmup, thin = nThin, init = initf2)

#save model fits
fit@stanmodel@dso <- new("cxxdso")
saveRDS(fit, file = paste("fit_",model,"_fb2.rds",sep = ""))
rm(fit, standata)

##############################################

########
# fb 3
########

# load behavioral data
fn = paste(wdir,"data_fb3.Rda",sep = "")
load(fn)

dat = df_v3

standata <- list(subnum = dat$participant,
                 blocknum = dat$block,
                 stim = ifelse(dat$stim == "rich", 1, 2),
                 rt = dat$rt,
                 corr = dat$corr,
                 nS = length(unique(dat[["participant"]])),
                 nB = length(unique(dat[["block"]])),
                 nT = length(dat$participant))

nS = length(unique(dat[["participant"]]))
nB = length(unique(dat[["block"]]))
# initial values for non-decision time
initf3 <- function() {
  list(mu_tau = array(0.15,dim = c(3)),tau1 = array(0.15, dim = c(3,73)),tau2 = array(0.15, dim = c(3,73)))
}

#fit model
fit <- stan(file=modeldir, data = standata, chains = nChain, iter = nIter, warmup = nWarmup, thin = nThin, init = initf3)

#save model fits
fit@stanmodel@dso <- new("cxxdso")
saveRDS(fit, file = paste("fit_",model,"_fb3.rds",sep = ""))
rm(fit, standata)

#############################################

########
# fb 4
########

## load behavioral data
fn = paste(wdir,"data_fb4.Rda",sep = "")
load(fn)

dat = df_v4

standata <- list(subnum = dat$participant,
                 blocknum = dat$block,
                 stim = ifelse(dat$stim == "rich", 1, 2),
                 rt = dat$rt,
                 corr = dat$corr,
                 nS = length(unique(dat[["participant"]])),
                 nB = length(unique(dat[["block"]])),
                 nT = length(dat$participant))

nS = length(unique(dat[["participant"]]))
nB = length(unique(dat[["block"]]))

# initial values for non-decision time
initf4 <- function() {
  list(mu_tau = array(0.15,dim = c(3)),tau1 = array(0.15, dim = c(3,82)),tau2 = array(0.15, dim = c(3,82)))
} 

#fit model
fit <- stan(file=modeldir, data = standata, chains = nChain, iter = nIter, warmup = nWarmup, thin = nThin, init = initf4)

#save model fits
fit@stanmodel@dso <- new("cxxdso")
saveRDS(fit, file = paste("fit_",model,"_fb4.rds",sep = ""))
rm(fit, standata)