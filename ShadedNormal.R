############
# Two tails
curve(dnorm(x),
      from = -3.5,
      to = 3.5,
      lwd = 2.5,
      xlab = "",
      ylab = "")

# Add the shaded area.

cord.x <- c(-3.5, seq(-3.5, qnorm(0.025), 0.01), qnorm(0.025))
cord.y <- c(0, dnorm(seq(-3.5, qnorm(0.025), 0.01)), 0)

polygon(cord.x, cord.y, col = "orange")

cord.x <- c(qnorm(0.975), seq(qnorm(0.975), 3.5, 0.01), qnorm(0.975))
cord.y <- c(0, dnorm(seq(qnorm(0.975), 3.5, 0.01)), 0)

polygon(cord.x, cord.y, col = "orange")


############
# Left tail

left.tail <- function(mean = 0, sd = 1, p = 0.025, lblx = ""){
   x <- qnorm(p = p, mean = mean, sd = sd)
   from <- mean - 3.5 * sd
   to <- mean + 3.5 * sd
   curve(dnorm(x, mean = mean, sd = sd),
         from = from,
         to = to,
         lwd = 2.5,
         xlab = "",
         ylab = "",
         ylim = c(-0.10 * dnorm(x = mean, mean = mean, sd = sd),
                  dnorm(x = mean, mean = mean, sd = sd)))

   # Add the shaded area.

   cord.x <- c(from, seq(from = from, to = x, length.out = 50), x)
   cord.y <- c(0, dnorm(x = seq(from = from, to = x, length.out = 50),
                        mean = mean, sd = sd), 0)

   polygon(cord.x, cord.y, col = "orange")

   text(x = x, y = -0.05 * dnorm(x = mean, mean = mean, sd = sd), labels = lblx)
}

   text(x = -2, y = -0.02, labels = "calculated z")

   arrows(x0 = -2, y0 = -0.06, x1 = -2, y1 = 0, angle = 25, length = 0.15, code = 2)

   arrows(x0 = qnorm(0.05), y0 = -0.06, x1 = qnorm(0.05), y1 = 0, angle = 25, length = 0.15, code = 1)

   text(x = qnorm(0.05), y = -0.04, labels = "critical z")


############
# Right tail

curve(dnorm(x),
      from = -3.5,
      to = 3.5,
      lwd = 2.5,
      xlab = "",
      ylab = "")

# Add the shaded area.


cord.x <- c(qnorm(0.95), seq(qnorm(0.95), 3.5, 0.01), qnorm(0.95))
cord.y <- c(0, dnorm(seq(qnorm(0.95), 3.5, 0.01)), 0)

polygon(cord.x, cord.y, col = "orange")

############ CHISQUARE


### chisq 15 df

# Two tails
curve(dchisq(x, df = 15),
      from = 0,
      to = 35,
      lwd = 2.5,
      xlab = "",
      ylab = "")

# Add the shaded area.

cord.x <- c(0, seq(0, qchisq(0.025, df = 15), 0.01), qchisq(0.025, df = 15))
cord.y <- c(0, dchisq(seq(0, qchisq(0.025, df = 15), 0.01), df = 15), 0)

polygon(cord.x, cord.y, col = "orange")

cord.x <- c(qchisq(0.975, df = 15), seq(qchisq(0.975, df = 15), 35, 0.01), qchisq(0.975, df = 15))
cord.y <- c(0, dchisq(seq(qchisq(0.975, df = 15), 35, 0.01), df = 15), 0)

polygon(cord.x, cord.y, col = "orange")

curve(dchisq(x, df = 15),
      from = 0,
      to = 35,
      lwd = 2.5,
      xlab = "",
      ylab = "",
      add = TRUE)


############ F


### F()
df1 <- 15
df2 <- 15

# Two tails on the right
curve(df(x, df1 = df1, df2 = df2),
      from = 0,
      to = 5,
      lwd = 2.5,
      xlab = "",
      ylab = "",
      ylim = c(-0.05, 0.9))

abline(h = 0)

# Add the shaded area.

cord.x <- c(qf(0.95, df1 = df1, df2 = df2),
            seq(qf(0.95, df1 = df1, df2 = df2), 5, 0.02),
            qf(0.95, df1 = df1, df2 = df2))

cord.y <- c(0,
            df(seq(qf(0.95, df1 = df1, df2 = df2), 5, 0.02),
               df1 = df1, df2 = df2), 0)

polygon(cord.x, cord.y, col = "skyblue")

curve(df(x, df1 = df1, df2 = df2),
      from = 0,
      to = 5,
      lwd = 2.5,
      xlab = "",
      ylab = "",
      add = TRUE,
      ylim = c(-0.05, 0.9))

text(y = -0.04, x = qf(0.95, df1 = df1, df2 = df2), pos = 4,
     labels = "Reject equality of variances", col = "red")

lines(x = c(2.5, 2.8), y = c(0.03, 0.20))

library(latex2exp)

text(y = 0.19, x = 2.80, pos = 4,
     labels = TeX("$\\frac{\\alpha}{2}"), col = "blue")


