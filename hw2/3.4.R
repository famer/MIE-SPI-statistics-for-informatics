#3.4
sequenceLength = 2500000;


sampleSize = L*40;
time1 = time2 = numeric(sampleSize); # Declare an array
for(i in 1:sampleSize){
  x = runif(sequenceLength, 0, 100); # Generate the sequence to be sorted
  time1[i] = system.time(x1 <- sort(x, method = "quick"),  gcFirst = TRUE)[1];
}
sampleSize2 = L*35;
for(i in 1:sampleSize2){
  x = runif(sequenceLength, 0, 100); # Generate the sequence to be sorted
  time2[i] = system.time(x2 <- sort(x, method = "shell"), gcFirst = TRUE)[1];
}

alpha <- K/100;
t.test(time1, y=time2, paired = FALSE, alternative="less", var.equal = FALSE, conf.level = 1-alpha)
