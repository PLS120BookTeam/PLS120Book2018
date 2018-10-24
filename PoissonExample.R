ppois(q = 5, lambda = 4) * 0 +
   (ppois(q = 10, lambda = 4) - ppois(q = 5, lambda = 4)) * 30 +
   (ppois(q = 20, lambda = 4) - ppois(q = 10, lambda = 4)) * 0.60 +
   (1 - ppois(q = 20, lambda = 4)) * 100


pnorm(q = 180, mean = 200, sd = 18)
qnorm(p = 0.10, mean = 200, sd = 18)
qnorm(p = 0.10)



pbinom(q = 0, prob = 0.10, size = 10)
1 - pbinom(q = 2, prob = 0.10, size = 10)
1 - 0.90^10 - 10*0.10*0.90^9 - 45*0.10^2*0.90^8



# 10 pears

pnorm(q = 1800, mean = 2000, sd = 18 * sqrt(10))

-200/(18*sqrt(10))


# Tomatoes

log(0.05)/log(0.97)

1 - pbinom(q = 0, size = 98, prob = 0.03)
