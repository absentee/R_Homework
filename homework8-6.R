exam4 = read.csv('example8_6.csv', header = TRUE)

block1 = exam4$weight[exam4$block == 1]
block2 = exam4$weight[exam4$block == 2]
block3 = exam4$weight[exam4$block == 3]
block4 = exam4$weight[exam4$block == 4]
dose1 = exam4$weight[exam4$dose == 1]
dose2 = exam4$weight[exam4$dose == 2]
dose3 = exam4$weight[exam4$dose == 3]

meanxb = c(mean(block1), mean(block2), mean(block3), mean(block4))
lengthsb = c(length(block1), length(block2), length(block3), length(block4))
vb = length(lengthsb) - 1

meanxd = c(mean(dose1), mean(dose2), mean(dose3))
lengthsd = c(length(dose1), length(dose2), length(dose3))
vd = length(lengthsd) - 1

attach(exam4)
meanx = mean(weight)
C = meanx^2 * length(weight)
vt = length(weight) - 1
SST = sum(weight^2) - C
SSB = sum(meanxb^2*lengthsb) - C
SSD = sum(meanxd^2*lengthsd) - C
SSE = SST - SSB - SSD
ve = vt - vb - vd
f = (SSB/vb)/(SSE/ve)
detach(exam4)

p = pf(f, vb, ve)
print(1 - p)