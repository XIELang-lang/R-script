## load packages
library(haven)
library(dplyr)
library(psych)
library(lavaan)
library(car)  
library(pequod) 

## load the data
data <- read_sav("/20230801_data.sav")

## Data Cleaning
df <- data[,4:7] %>% as.data.frame()
colnames(df) <- c("bps","ps","ase","abo")
  #bps：bedtime procrastination
  #p s：Psychological resilience
  #ase：academic self-efficacy
  #abo：academic burnout

## data scale
df$bps.c <- scale(df$bps,center = T, scale = F)[,]
df$ase.c <- scale(df$ase,center = T, scale = F)[,]
df$ps.c <- scale(df$ps,center = T, scale = F) [,]

## Define interaction items
df$bpsase <- df$bps.c*df$ase.c
df$aseps <- df$ase.c*df$ps.c
df$bpsps <- df$bps.c*df$ps.c 
 
## Spearman correlation analysis
dfcor <- corr.test(df,method="spearman",alpha=.05)
lowerMat(dfcor$r,digits = 3)
lowerMat(dfcor$p,digits = 4)

## model definition
model1 <- "ase~a*bps
            abo~c*bps+b*ase+m*aseps+ps
            direct:=c
            ind :=a*b
            total :=c+a*b
            bh :=b+m*12.76
            bm :=b+0
            bl :=b-m*12.76
            bdiff :=bh-bl
            indh :=a*bh
            indl :=a*bl
            inddiff :=a*bh-a*bl" 

## model fitting
set.seed(1234)
model.fit <- sem(model1,data = df,se="bootstrap",bootstrap=10000)

## result viewing 
summary(model.fit1,standardized=TRUE,fit.measures=TRUE,rsquare=T)
parameterestimates(model.fit1,boot.ci.type = "perc",standardized = T)

## Simple slope
model_point<-lmres(abo~ase*ps, centered=c("ps", "ase"),data=df) 
summary(model_point, type="nested") 
S_slopes<-simpleSlope(model_point,pred="ase",mod1="ps")  
summary(S_slopes)                                            

PlotSlope(S_slopes,namemod=c("low psychological resilience (M-SD)","high psychological resilience (M+SD)"))
print(S_slopes[["Points"]])