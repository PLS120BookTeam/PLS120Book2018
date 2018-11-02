# Power demo function

## Create graph with two normal pdf's showing alpha, beta and power

power.graph <- function(hmean = 50,
                        sigma = 10,
                        n = 5,
                        diff = 10,
                        alpha = 0.05,
                        tails = 1) {

   sd = sigma / sqrt(n)

   from <- min(hmean - 4 * sd, hmean + diff - 4 * sd)

   to <- max(hmean + 4 * sd, hmean + diff + 4 * sd)

   tmean <- hmean + diff

   pvalue <- alpha / tails

   zcritical <- hmean + sign(diff) * sd * abs(qnorm(p = pvalue))

   beta <- if (diff < 0) {
      round(1 - pnorm(q = zcritical, mean = tmean, sd = sd), 4)
   } else {
      round(pnorm(q = zcritical, mean = tmean, sd = sd), 4)
   }

   curve(dnorm(x, mean = hmean,
               sd = sd),
         from = from, to = to,
         lwd = 2.5, col = "black",
         xlab = "", ylab = "")

   curve(dnorm(x, mean = tmean,
               sd = sd),
         from = from, to = to,
         lwd = 2.5, col = "blue",
         xlab = "", ylab = "", add = TRUE)

   # Create data to shade rejection zone under Ho curve
   if (diff < 0){
      cord.x <- c(from, seq(from = from, to = zcritical, length.out = 50), zcritical)
      cord.y <- c(0, dnorm(
         seq(from = from, to = zcritical, length.out = 50),
         mean = hmean,
         sd = sd), 0)} else {
            cord.x <- c(zcritical, seq(from = zcritical, to = to, length.out = 50), zcritical)
            cord.y <- c(0, dnorm(
               seq(from = zcritical, to = to, length.out = 50),
               mean = hmean,
               sd = sd), 0)
         }

   # Add the shaded area.
   polygon(cord.x, cord.y, col = "orange")


   # Create data to shade failure to reject zone under True curve
   if (diff < 0){
      cord.x <- c(zcritical, seq(from = zcritical, to = to, length.out = 50), zcritical)
      cord.y <- c(0, dnorm(
         seq(from = zcritical, to = to, length.out = 50),
         mean = tmean,
         sd = sd), 0)} else {
            cord.x <- c(from, seq(from = from, to = zcritical, length.out = 50), zcritical)
            cord.y <- c(0, dnorm(
               seq(from = from, to = zcritical, length.out = 50),
               mean = tmean,
               sd = sd), 0)}

   # Add the shaded area.
   polygon(cord.x, cord.y, col = "lightblue")

   curve(dnorm(x, mean = hmean,
               sd = sd),
         from = from, to = to,
         lwd = 2.5, col = "black",
         xlab = "", ylab = "", add = TRUE)

   curve(dnorm(x, mean = tmean,
               sd = sd),
         from = from, to = to,
         lwd = 2.5, col = "blue",
         xlab = "", ylab = "", add = TRUE)

   abline( v = zcritical, col = "black", lwd = 1, lty = 2)

   abline( v = hmean, col = "black", lwd = 1.5, lty = 1)

   abline( v = tmean, col = "blue", lwd = 1.5, lty = 1)

   text(x = hmean, cex = 1.2,
        y = dnorm(x = hmean, mean = hmean, sd = sd) /2,
        pos = 2, srt = 90, labels = "Ho mean")

   text(x = zcritical,
        y = - dnorm(x = hmean, mean = hmean, sd = sd) /18,
        labels = "Ycritical", xpd = TRUE, pos = 1)

   text(x = tmean, cex = 1.2,
        y = dnorm(x = tmean, mean = tmean, sd = sd) /2,
        pos = 2, srt = 90, labels = "True mean", col = "blue")

   text(x = from + (to - from) /40,
        y = seq(from = 0.95, to = 0.60, length.out = 5) *
           dnorm(x = tmean, mean = tmean, sd = sd),
        pos = 4, cex = 1.3,
        labels = c(
           paste("Ho: Mean =  ", as.character(hmean)),
           paste("Sigma =  ", as.character(sigma)),
           paste("r =  ", as.character(n)),
           paste("alpha =  ", as.character(alpha)),
           paste("beta =  ", as.character(beta))
        ))
}


par(mfrow = c(4, 2))
for (i in (1:8)*2){
power.graph(hmean = 50,
            sigma = 10,
            n = i,
            diff = 10,
            alpha = 0.05,
            tails = 1)
}
