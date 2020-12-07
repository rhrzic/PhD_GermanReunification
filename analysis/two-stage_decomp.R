Theil <- function(df, ple) {
  
  y_ave <- mean(df[[ple]], na.rm = T)
  N <- length( df[[ple]] )
  
  Ttotal = (1 / N) * sum( (df[[ple]] / y_ave) * log(df[[ple]] / y_ave), na.rm = T)
  
  return(data.frame(Ttotal = Ttotal))
  #return(data.frame(Ttotal = Ttotal, Tnormal = Tnormal, N = N, Tmax = log(N)))
}

onestage.Theil.decomp <- function(df, macroregion, ple) {
  
  y_ave <- mean(df[[ple]], na.rm = T)
  
  N <- length(df[[ple]])

  Ttotal = Theil(df, ple)[[1]]
  
  by_macroregion <- split(df, df[[macroregion]])
  
  Ni <- sapply(by_macroregion, function(x){length(x[[ple]])})
  yi_ave <- sapply(by_macroregion, function(x){mean(x[[ple]], na.rm = T)})

  Tmi <- sapply(by_macroregion, function(x){
    
    Theil(x,ple)[[1]]
  })
  
  Twithin <-  sum( (Ni/N) * (yi_ave/y_ave) * Tmi)
  Tbetween <-  sum( (Ni/N) * (yi_ave/y_ave) * log( yi_ave / y_ave ) )
  
  result <- data.frame(Tbetween = Tbetween, Twithin = Twithin, Ttotal = Ttotal, Additive = Twithin + Tbetween)
  
  return(result)
}

twostage.Theil.decomp <- function(df, country, macroregion, ple) {
  
  y_ave <- mean(df[[ple]], na.rm = T)
  N <- length(df[[ple]])
  
  Ttotal = Theil(df, ple)[[1]]
  
  by_macroregion <- split(df, df[[macroregion]])
  
  yi_ave <- sapply(by_macroregion, function(x){mean(x[[ple]], na.rm = T)})
  Ni <- sapply(by_macroregion, function(x){length(x[[ple]])})
  
  Tbmr <- sum( (yi_ave/y_ave) * (Ni/N) * log(yi_ave/y_ave) )
  
  by_country <- split(df, df[,country])
  
  yij_ave <- sapply(by_country, function(x){mean(x[[ple]], na.rm = T)})
  Nij <- sapply(by_country, function(x){length(x[[ple]])})
  
  Tij <- sapply(by_country, function(x){
    Theil(x, ple)[[1]]
  })
  
  Tpi <- sapply(by_macroregion, function(x){
    by_country2 <- split(x, x[,country])
    Yij <- sapply(by_country2, function(x){mean(x[[ple]], na.rm = T)})
    Nij <- sapply(by_country2, function(x){length(x[[ple]])})
    sum( (Yij / mean(x[[ple]], na.rm = T)) * (Nij/length(x[[ple]])) * log(Yij/mean(x[[ple]], na.rm = T)) )
  })
  
  Tbc <- sum( (yi_ave/y_ave) * (Ni/N) * Tpi, na.rm = T)
  Twc <- sum( (yij_ave/y_ave) * (Nij/N) * Tij, na.rm = T)
  
  result <- data.frame(Tbmr = Tbmr, Tbc = Tbc, Twc = Twc, Ttotal = Ttotal, Tsum = (Tbmr+Tbc+Twc), N = N)
  
  return(result)
}