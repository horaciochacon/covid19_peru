
# Peak functions ----------------------------------------------------------

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
    if (x[i] > 10) {
      peak[i] <- x[i] > x[i-1]  & x[i] > x[i+1]
    }
  }
  peak
}

# Plotting functions ------------------------------------------------------

geom_gam_dates <- function() {
    geom_label_repel(
      aes(x = x, y = y, label = label),
      nudge_y = 150,
      data = labels,
      arrow = arrow(length = unit(0.015, "npc"))
    )
}

geom_covid_gam <- function(k, title, label_df, nudge_y, ...) {
  list(
    geom_line(alpha = 0.5),
    geom_smooth(method = "gam", formula = y ~ s(x, k = k)),
    geom_label_repel(
      aes(x = x, y = y, label = label),
      nudge_y = nudge_y,
      data = label_df,
      arrow = arrow(length = unit(0.015, "npc"))
    ),
    labs(title = title, ...)
  )
}