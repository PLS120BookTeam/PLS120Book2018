block.fx <- unlist(
   apply(
      matrix(trt.n),
      1,
      FUN = function(x)
         rep(c(0.1, 0.2, -0.3), length.out = x)
      )
   )

str(milk.protB)

milk.protB <- milk.protB[, 1:5]

milk.protB <- cbind(milk.protB,block, block.fx)

milk.protB$logProt1 <- milk.protB$logProt + milk.protB$block.fx

mp.lm <- lm(logProt1 ~ Treatment, milk.protB)

mp.lmb <- lm(logProt1 ~ Treatment + block, milk.protB)

anova(mp.lm)

anova(mp.lmb)


# reduce sample size by averaging by treatment and block.

set.seed(1)
out2 <- milk.protB %>%
   group_by(Treatment, block) %>%
   sample_n(5)

out2 <- aggregate(logProt ~ Treatment + block, milk.protB, FUN = mean)

mp.lm <- lm(logProt ~ Treatment, out2)

mp.lmb <- lm(logProt ~ Treatment + block, out2)

anova(mp.lm)

anova(mp.lmb)

boxplot(logProt ~ Treatment, out2)

emmeans(mp.lmb, "Treatment")

milk.protB <- out2

write.table(milk.protB, "milk.protB.txt", row.names = FALSE)
