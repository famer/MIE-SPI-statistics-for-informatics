# 3

# 3.1.c
sequenceLength = 2500000;
x = runif(sequenceLength, 0, 100);
print(system.time(sort(x)));

# 3.2
sampleSize = L*40;
time1 = time2 = numeric(sampleSize); # Declare an array
for(i in 1:sampleSize){
  x = runif(sequenceLength, 0, 100); 
  time1[i] = system.time(x1 <- sort(x, method = "quick"),  gcFirst = TRUE)[1];
  time2[i] = system.time(x2 <- sort(x, method = "shell"), gcFirst = TRUE)[1];
}
time1;
time2;
# 3.2.b
alpha <- K/100;
t.test(time1, y=time2, paired = TRUE, alternative = "less", conf.level = 1-alpha)
