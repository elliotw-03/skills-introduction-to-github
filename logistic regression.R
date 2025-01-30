Bankruptcies <- read.csv("https://tanjakec.github.io/mydata/Bankruptcies.csv")

glimpse(Bankruptcies)
summary(Bankruptcies)

set.seed(123)
split_idx = sample(nrow(Bankruptcies), 53)
bankrup_train = Bankruptcies[split_idx, ]
bankrup_test = Bankruptcies[-split_idx, ]

model1 <- glm(Y ~ X1 + X2 + X3, data = bankrup_train, family = binomial(logit))
model1
summary(model1)

round(exp(coef(model1)), 4)

G_calc <- model1$null.deviance - model1$deviance
Gdf <- model1$df.null - model1$df.residual
pscl::pR2(model1)

anova(model1, test="Chisq")

link_pr <- round(predict(model1,  bankrup_test, type = "link"), 2)
link_pr

response_pr <- round(predict(model1,  bankrup_test, type = "response"), 2)

t(bankrup_test$Y)

coefficients(model1)
