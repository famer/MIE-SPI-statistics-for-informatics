n1 = 20;
n2 = 25;
alpha = 0.01
x=rnorm(n1, mean=10, sd=1.3)
y=rnorm(n2, mean=11.28, sd=1.2)

t.test(x, y=y, paired = FALSE, var.equal = FALSE, conf.level = 1-alpha)
t.test(x, y=y, paired = FALSE, alternative="less", var.equal = FALSE, conf.level = 1-alpha)

sx2 <- var(x);
sy2 <- var(y);
meanX <- mean(x);
meanY <- mean(y);
degOfFreedom <- (sx2/n1 + sy2/n2)^2/( (sx2/n1)^2/(n1-1) + (sy2/n2)^2/(n2-1) );


sxy <- sqrt (sx2/n1 + sy2/n2)

t <- (meanX - meanY)/sxy;

pValue <- pt(t, degOfFreedom);

degOfFreedom;
pValue;
t;