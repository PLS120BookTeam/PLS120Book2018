# Randomization

trt <- c("thin", "short", "long")

blocks <- c("XL", "LG", "MD", "SM")

horse <- paste("H", 1:12, sep = "")
horse <- paste(horse, blocks, sep = ".")

xl <- c(1, 5, 9)
lg <- xl + 1
md <- lg + 1
sm <- md + 1

horse[xl]

cbind(sample(horse[xl]), sample(trt))
cbind(sample(horse[lg]), sample(lg))
cbind(sample(horse[md]), sample(md))
cbind(sample(horse[sm]), sample(sm))





# Block analysis
library(emmeans)

mydata <- data.frame(block = c("a","b","c","a","b","c"),
                     trt = c(rep("A", 3),rep("B", 3)),
                     Y = c(2,1,4,4,2,4))

mymodel <- lm(Y ~ block + trt, mydata)

anova(mymodel)

emmeans(mymodel, "trt")

