#rm(list=ls())

library(tidyverse)

geo_structure <- readRDS("temp_data/geo_structure.rds")
age  <-  0:89

mortality <- readRDS("temp_data/mortality.rds")
mortality <- mortality %>%
  mutate(mx = deaths/pop,
         logmx = log(mx),
         sex = ifelse(sex == "male", 1, 2))

boundary_ages = c(unique(mortality$age), max(mortality$age)+5)


for (j in 1:4) {
  this.kreis = sample(401,1)
  this.name = geo_structure$geo[this.kreis]
  
  this.lambda = result$lambda[,,this.kreis,]
  Q = apply(this.lambda, c(1,3), quantile, probs=c(.10,.50,.90))
  
  matplot(age, Q['50%',,],type='l',lty=1,lwd=2,col=c('royalblue','deeppink'),
          ylim=c(-11,-1),
          main=paste(this.name))
  matlines(age, Q['10%',,],type='l',lty=2,lwd=1,col=c('royalblue','deeppink'))
  matlines(age, Q['90%',,],type='l',lty=2,lwd=1,col=c('royalblue','deeppink'))
  
  
  matpoints(boundary_ages[2:15], t(logmx[,,this.kreis]), pch=c('m','f'), col=c('royalblue','deeppink'),
            cex=.60)
  abline(h=seq(-11,-1,1), v=seq(0,100,10), lty=2, col='lightgrey')
}

