
get_peaks <- function(x) {
  r <- emd(x)
  y <- apply( r$imf[,5:6], 1, sum ) + mean(r$residue)
  plot(x, type="l", col="grey")
  lines( y, type="l", lwd=2)
  n <- length(y)
  i <- y[2:(n-1)] > y[1:(n-2)] & y[2:(n-1)] > y[3:n]
  points( which(i), y[i], pch=15 )
}

get_peaks_first <- function(x) {
  r <- emd(x)
  y <- apply( r$imf[,5:6], 1, sum ) + mean(r$residue)
  n <- length(y)
  i <- y[2:(n-1)] > y[1:(n-2)] & y[2:(n-1)] > y[3:n]
  which(i)[1]
}

get_peak <- function(x) {
  peak <- vector()
  for (i in 1:length(x)) {
    peak[i] <- x[i] > x[i-1]  & x[i] > x[i+1]
  }
  peak
}
