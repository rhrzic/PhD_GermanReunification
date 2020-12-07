rm(list=ls())
library(tidyverse)

load('results/result_1997-2020-10-28-0915.Rdata')

# Our own little database containing the official AGS (=Amtlicher GemeindeschlÃ¼ssel=County Code )
# as well as the names of the counties as well as the respective state (=Bundesland) they belong to.
# the object name is 'geo.df'
geo_structure <- readRDS("temp_data/geo_structure.rds")
mortality <- readRDS("temp_data/mortality.rds")

# Function to estimate life expectancy at birth based on log_e death rates mx
# more or less following Preston et al. (2001)
e0.fun <- function(logmx) {
  mx = exp(logmx)
  ax = rep(0.5, length(mx))
  ax[1] <- 0.1
  qx = mx/(1+(1-ax)*mx)
  qx[qx < 0] <- 0
  qx[qx > 1] <- 1
  px = 1 - qx
  lx <- 100000 * cumprod(c(1, px))[1:(length(px))]
  dx <- c(-diff(lx), lx[length(lx)])
  Lx <- c(lx[-1],0) + ax * dx
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx/lx
  return(ex[1])
}  

# the Kannisto() function extends a logmx schedule to
# very old ages by fitting a regression to the logits
# of high ages
Kannisto = function(logmx, 
                    ages        = 0:89,        
                    fit_ages    = 70:89,
                    extrap_ages = 90:119) {
  
  fit_mx     = exp(logmx[ages %in% fit_ages])
  fit_logits = log(fit_mx / (1-fit_mx))
  logit_coef = coef( lm( fit_logits ~ fit_ages) )
  loga       = logit_coef[1]
  b          = logit_coef[2]
  
  extrap_logits = loga + b*extrap_ages
  extrap_logmx  = log( 1 / (1+exp(-extrap_logits)) )
  
  tmp        = c(logmx, extrap_logmx)
  names(tmp) = c(ages,extrap_ages)
  
  return(tmp)
  
} # Kannisto


# result$lambda is a 90 x nsample x 402 x 2 array of 
# MCMC draws for log mx at ages 0..89 for each (sample,Kreis,sex)
#
# without parallelization this next step is 
# SLOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOW
# e0_array will be nsample x 402 x 2

require(parallel)
parallel_processing = TRUE
ncores = detectCores()-1

cl = makePSOCKcluster(ncores)
clusterExport(cl,list('e0.fun','Kannisto'))
  
  e0_array = parApply(cl, result$lambda, 2:4,
                      function(logmx) {
                        e0.fun( Kannisto(logmx) )
                      })

# Q will initially be 7 x 402 x 2
Q = apply(e0_array, 2:3, quantile, 
          probs=c(.05,.10,.25,.50,.75,.90,.95))

# for historical compatibility, desired output is 
# a 804 x 10 data frame, 
# ordered by AGS within sex
e0.our.model = data.frame(
  AGS  = as.numeric(dimnames(Q)[[2]]),
  Sex  = rep(1:2, c(402,402)),
  Sex2 = rep(c('M','F'), c(402,402)),
  pct05 = as.vector(Q['5%',,]),
  pct10 = as.vector(Q['10%',,]),
  pct25 = as.vector(Q['25%',,]),
  pct50 = as.vector(Q['50%',,]),
  pct75 = as.vector(Q['75%',,]),
  pct90 = as.vector(Q['90%',,]),
  pct95 = as.vector(Q['95%',,])
)          


save(e0.our.model, file="../data/e0-of-our-model.RData")