## remove all variables
rm(list=ls())


## set working directory
wdir = "C:/Users/wurmf/Dropbox/GitHub/rddm/" #office laptop
#wdir = "D:/Dropbox/GitHub/rddm/" #home desktop
setwd(wdir)

## load behavioral data
fn = paste(wdir,"PRTdata.csv",sep = "")
df = read.csv(fn)

## get only relevant variables
df2 = subset(df,select = c(participant,versionPRT,block,reinforcement_schedule,RT_original,response.corr))
names(df2)[names(df2) == "reinforcement_schedule"] = "stim"
names(df2)[names(df2) == "versionPRT"] = "version"
names(df2)[names(df2) == "RT_original"] = "rt"
names(df2)[names(df2) == "response.corr"] = "corr"

## rt outlier removal
idx_2fast = df2[['rt']]<0.25
idx_2slow = df2[['rt']]>2.5
idx_nan = !is.finite(df2[['rt']])
idx = !(idx_2fast|idx_2slow|idx_nan)
df3 = df2[idx,]

## rt outlier removal (option 2)
# = df2
#df4 = df4[df4$rt>=0.2 & df4$rt<=2 & is.finite(df4[['rt']]), ]

#df_split1 = split(df2,df2$version)
#df_split2 = split(df3,df3$version)
df_split = split(df3,df3$version)


##############
#PRT version 1
##############
df_v1 = df_split$`1`
Snums = unique(df_v1[["participant"]])
for (i in 1:length(Snums)) {
  idx = df_v1[['participant']]==Snums[i]
  df_v1$participant[idx]=i
}
save(df_v1,file="data_fb1.Rda")

################
# PRT version 2
################
df_v2 = df_split$`2`
Snums = unique(df_v2[["participant"]])
for (i in 1:length(Snums)) {
  idx = df_v2[['participant']]==Snums[i]
  df_v2$participant[idx]=i
}
save(df_v2,file="data_fb2.Rda")

################
#PRT version 3
################
df_v3 = df_split$`3`
Snums = unique(df_v3[["participant"]])
for (i in 1:length(Snums)) {
  idx = df_v3[['participant']]==Snums[i]
  df_v3$participant[idx]=i
}
save(df_v3,file="data_fb3.Rda")

################
#PRT version 4
################
df_v4 = df_split$`4`
Snums = unique(df_v4[["participant"]])
for (i in 1:length(Snums)) {
  idx = df_v4[['participant']]==Snums[i]
  df_v4$participant[idx]=i
}
save(df_v4,file="data_fb4.Rda")