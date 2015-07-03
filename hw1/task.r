# 1.1
K = 5;
L = 10;
n = K*20;
u = runif(n, 0, 1);
x = sapply(u, function(x) { -1 / L * log(1-x) });
x;

#1.2
# 1
png(filename = '1.2_1.png', width = 800, height = 600);
hist(u, breaks=length(u), freq=TRUE);
graphics.off();

# 2
png(filename = '1.2_2.png', width = 800, height = 600);
hist(x, breaks=length(x), freq=FALSE);

xGrid=seq(min(x),max(x),length=30)
lines (xGrid,dexp(xGrid, rate=L), col='red', lw=2, lty=2)
graphics.off();

# 3

png(filename = '1.2_3.png', width = 800, height = 600);
plot(ecdf(x), verticals=TRUE, do.points = FALSE);
xGrid=seq(min(x),max(x),length=30)
lines (xGrid, pexp(xGrid, rate=L), col='red', lw=2, lty=2)

graphics.off();

# 4

png(filename = '1.2_4.png', width = 800, height = 600);
qqplot((x), rexp(1000, rate=L), plot.it = TRUE);
abline (0, 1, col='red', lw=2, lty=2)
graphics.off();

# 2
lambda = function(t) { 100 + 50/exp((t - 420)^2/(3600*L)) + 100/exp((L*(t - 480 - 30*L)^2)/360000)};
png(filename = '2.1.png', width = 800, height = 600);
curve(lambda(x), from = 0, to = 24*60, n = 24*60, xlab = "t(mins)", ylab = "N of visits per min", main="1 day");
graphics.off();

# 2.2

arrival_times = numeric();
current_time = 1;
while (current_time <= 24 * 60)
{
    increment = rexp(1, lambda(current_time));
    current_time = current_time + increment;
    arrival_times = append(arrival_times, current_time);
}

png(filename = "2.2.png", width = 800, height = 170);
plot(x = arrival_times[1:(K * 10)], y = rep(0, K * 10), type = 'p', xlab = "t(mins)");
graphics.off();

# 2.3
png(filename = "2.3.png", width = 800, height = 600);
h = hist(arrival_times, breaks = 24 * 60, freq = TRUE, plot = FALSE, warn.unused = FALSE);
plot(h$mids, h$counts, col = "gray", type = 'l', xlab = "t(mins)", ylab = "N of visits per min", main = "1 day");
curve(lambda(x), from = 0, to = 24 * 60, n = 24 * 60, add = TRUE, lty = "dashed", col = "red", lwd = 2);
graphics.off();


# 3.2
atc = numeric();
atp = numeric();
for (i in 1:length(arrival_times))
{
	t = runif(1, min = 0, max = 1);
	if (t < K / (K + L))
		atc = append(atc, arrival_times[i])
	else
		atp = append(atp, arrival_times[i]);
}

# Task 3.3
png(filename = "3.1.png", width = 800, height = 600);
h = hist(atc, breaks = 24 * 60, freq = TRUE, plot = FALSE, warn.unused = FALSE);
plot(h$mids, h$counts, col = "gray", type = 'l', xlab = "t(mins)", ylab = "N of visits per min for courrier", main = "1 day");
curve(lambda(x) * (K / (K + L)), from = 0, to = 24 * 60, n = 24 * 60, add = TRUE, col = 'red', lty = 2);
graphics.off();

png(filename = "3.2.png", width = 800, height = 600);
h = hist(atp, breaks = 24 * 60, freq = TRUE, plot = FALSE, warn.unused =
		FALSE);
plot(h$mids, h$counts, col = "gray", type = 'l', xlab = "t (mins)", ylab = "N of visits per min for post", main = "1 day");
curve(lambda(x) * (L / (K + L)), from = 0, to = 24 * 60, n = 24 * 60, add = TRUE, col = 'red', lty = 2);
graphics.off();
