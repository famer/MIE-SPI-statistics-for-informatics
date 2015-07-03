n = 20;
alpha = 0.01
x = rnorm(n, mean=10, sd=1)
error = rnorm(n, mean=0.5, sd=0.8306624)
y = x + error

t.test(x, y=y, paired = TRUE, alternative = "less", conf.level = 1-alpha)

diff <- x - y;
t.test(diff, mu=0, alternative="less", conf.level = 1-alpha);