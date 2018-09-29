bucket <- c(rep("W", 5), rep("B", 15))

bucket
length(bucket)

sim <- sample(bucket, replace = TRUE, size = 1000)

sum(sim == "W")/length(sim)


sim <- sample(bucket, replace = TRUE, size = 10000)

sum(sim == "W")/length(sim)


#Kahneman and pilot instructors

# Random sequence of performances from a binomial distribution.

plot(dbinom(seq(from = 0, to = 100, by = 5)/5, 20, 0.7), type = "h")

performance <- rbinom(50, 20, 0.7) * 5

p1 <- as.character(performance)
p1[performance > 80] <- "Excellent"
