K <- 5;
L <- 10;
K;

n = 20;
alpha = 0.01;
x = rnorm(n, mean=10.5, sd=1.3);

hypothesisTest = t.test(x, mu=10, alternative = "greater", conf.level = 1-alpha);
print(hypothesisTest);

testT <- abs(mean(x) - 10)/(sqrt(var(x))/sqrt(20));
testT;

n <- length(x);
criticalValue <- qt(alpha, 19, lower.tail = FALSE);
avgX <- mean(x);
stdDev <- sd(x);
sqrtn <- sqrt(n);
intv <- criticalValue*(stdDev/sqrtn);

confidence <- c(avgX - intv, +Inf);




