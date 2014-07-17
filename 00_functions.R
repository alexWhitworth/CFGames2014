

# needed functions - coding
to_int <- function(x, cols) {
  for (i in cols) {
    x[,i] <- as.integer(x[,i])
  }
  return(x)
}

to_time <- function(x, cols) {
  require(lubridate)
  for (i in cols) {
    x[,i] <- ms(x[,i])
  }
  return(x)
}

# adjusting event time for event 6
adj_event <- function(x, cut, reptime) {
  x2 <- x
  # extract min/sec
  m <- minute(x)
  s <- second(x)
  # compare to time cap
  mns <- m * 60 + s
  adj <- which(ifelse(mns > cut, T, F))
  # using "reptime" as time required for each additional rep, adjust
  # times over the time cap to "actual time needed"
  for (i in adj) {
    mns[i] <- round((mns[i] - cut) * reptime + cut, 0)
    m[i]  <- floor(mns[i] / 60)
    s[i]  <- mns[i] - (m[i] * 60)
    x2[i] <- ms(paste(as.character(m[i]), as.character(s[i]), sep=":"))
  }
  return(x2)
}

# scale a period vector
scale_ms <- function(x) {
  # extract min/sec, convert to numeric
  m <- minute(x)
  s <- second(x)
  mns <- m * 60 + s
  # scale converted score
  x2 <- scale(ms)
}

conv_ms <- function(x, func= "mean", na.rm=TRUE, ...) {
  # extract min/sec, convert to numeric
  m <- minute(x)
  s <- second(x)
  mns <- m * 60 + s
  if (func == "mean") {
    x2 <- mean(mns, na.rm= na.rm)
  } else if (func == "sd") {
    x2 <- sd(mns, na.rm= na.rm)
  } else if (func == "median") {
    x2 <- median(mns, na.rm= na.rm) 
  } else if (func == "quantile") {
    x2 <- quantile(mns, na.rm= na.rm, ...)
  } else if (func == "min") {
    x2 <- min(mns, na.rm= na.rm)
  } else if (func == "max") {
    x2 <- max(mns, na.rm= na.rm)
  }
  m2 <- trunc(x2 / 60)
  s2 <- round((x2 - trunc(x2 / 60)) * 60, 0)
  return(ms(paste(as.character(m2), as.character(s2), sep=":")))
}

tab_games <- function(cf, std=F) {
  # DESC: basic tabulations of the CF games data
  # inputs: matrix of CF games data
  # STD = T if scores are standardized, =F if raw scores
  
  # internals
  ath.cnt <- function(x) sum(!is.na(x)) # needed for accurate count (given NA's)
  # tabulate
  if (std == F) {
    ath <- apply(cf, 2, ath.cnt)
    avg <- apply(cf, 2, conv_ms, func= "mean", na.rm=TRUE)
    med <- apply(cf, 2, conv_ms, func= "median", na.rm=TRUE)
    st.dev <- apply(cf, 2, conv_ms, func= "sd", na.rm= TRUE)
    min_ <- apply(cf, 2, conv_ms, func= "min", na.rm=TRUE)
    max_ <- apply(cf, 2, conv_ms, func= "max", na.rm=TRUE)
    # combine and return
    tab_g <- cbind(ath, avg, med, st.dev, min_, max_)
    return(tab_g)
  } else if (std == T) {
    ath <- apply(cf, 2, ath.cnt)
    avg <- apply(cf, 2, mean, na.rm=TRUE)
    med <- apply(cf, 2, median, na.rm=TRUE)
    st.dev <- apply(cf, 2, sd, na.rm= TRUE)
    min_ <- apply(cf, 2, min, na.rm=TRUE)
    max_ <- apply(cf, 2, max, na.rm=TRUE)
    # combine and return
    tab_g <- cbind(ath, avg, med, st.dev, min_, max_)
    return(tab_g)
  }
}