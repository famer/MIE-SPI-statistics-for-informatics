n1 = 20;
n2 = 25;
alpha = 0.01
x=rnorm(n1, mean=10, sd=1.3)
y=rnorm(n2, mean=11.25, sd=1.3)

t.test(x, y=y, paired = FALSE, var.equal = TRUE, conf.level = 1-alpha)

t.test(x, y=y, paired = FALSE, alternative = "less", var.equal = TRUE, conf.level = 1-alpha)
sx2 <- var(x);
sy2 <- var(y);
meanX <- mean(x);
meanY <- mean(y);
degOfFreedom <- n1 + n2 - 2;

sxy <- sqrt ( ((n1-1)*sx2 + (n2-1)*sy2)/degOfFreedom )
t <- (meanX - meanY)/(sxy*sqrt(1/n1+1/n2));

pValue <- pt(t, degOfFreedom);

degOfFreedom;
pValue;
t;