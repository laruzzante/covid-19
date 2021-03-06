## Exponential growth
# y = a * (1 + r)^x
# a = initial value (the amount before measuring growth or decay)
# r = growth or decay rate (most often represented as a percentage and expressed as a decimal)
# x = number of time intervals that have passed
# (y/a)^(1/x) - 1 = r

BASE_DIR <- "~/covid-19/"
setwd(BASE_DIR)

## March: New confirmed covid-19 cases per day. Data taken from daily situation reports from the WHO website.
days <- as.Date(c('01.03','02.03','03.03','04.03','05.03','06.03','07.03',
          '08.03','09.03','10.03','11.03','12.03','13.03','14.03',
          '15.03','16.03','17.03','18.03','19.03','20.03','21.03',
          '22.03','23.03','24.03','25.03','26.03','27.03','28.03',
          '29.03','30.03','31.03','01.04','02.04','03.04','04.04',
          '05.04','06.04','07.04','08.04'), tryFormats = c("%d.%m"))
it <- c(240,561,347,466,587,769,778,1247,1492,1797,977,2313,2651,
        2547,3497,3590,3233,3526,4207,5322,5986,6557,5560,4789,5249,
        5210,6153,5959,5974,5217,4050,4053,4782,4668,4585,4805,4316,
        3599,3039)
es <- c(13,0,69,37,47,59,117,56,159,435,615,501,825,1266,1522,2000,
        1438,1987,2538,3431,2833,4946,3646,4517,6584,7937,8578,7871,
        8189,6549,6398,9222,7719,8102,7472,7026,6023,4273,5478)
ch <- c(8,6,6,7,20,30,123,55,68,0,159,154,213,267,234,841,0,450,353,
        853,977,1237,894,1044,774,925,1000,1390,1048,1122,1138,696,
        962,1774,862,783,576,509,590)
fr <- c(43,0,91,21,73,138,193,93,410,286,372,495,591,780,829,911,
        1193,1079,0,1834,1598,1821,1525,3794,2410,2895,3866,3756,
        4603,2497,4335,7500,4784,2066,5209,4221,1850,3881,3738)
de <- c(0,72,28,39,66,272,105,156,317,27,157,271,802,693,733,1043,
        1174,1144,1042,2801,7324,3140,3311,4438,2342,4954,5780,6294,
        3965,4751,4615,5453,6156,6174,6082,5936,3677,3834,4003)
uk <- c(3,13,3,12,20,29,49,43,67,46,50,87,134,208,342,251,152,407,
        672,647,706,1035,669,967,1427,1452,2129,2885,2546,2433,2619,
        3009,4324,4244,4450,3735,5903,3802,3634)
all_countries <- list(it,es,ch,fr,de,uk)

## March: New confirmed covid-19 deaths per day. Data taken from daily situation reports from the WHO website.
it_d <- c(8,6,17,28,27,41,49,37,132,97,168,196,189,252,173,368,349,
          345,473,429,625,795,649,601,743,685,660,971,887,758,810,
          839,727,760,764,681,527,636,604)
es_d <- c(0,0,0,0,1,2,2,0,5,18,8,12,36,36,16,152,21,182,107,169,
          235,324,394,462,514,738,655,769,832,838,812,849,864,950,
          932,809,674,637,743)
ch_d <- c(0,0,0,0,0,1,0,1,0,0,1,1,2,0,5,2,1,5,2,12,10,13,4,6,20,17,
          58,36,38,22,38,78,5,158,71,59,49,0,0)
fr_d <- c(0,0,1,1,0,2,3,1,9,11,3,15,13,18,12,36,21,27,0,128,78,112,
          112,186,240,231,364,297,319,291,415,497,505,471,2003,1053,
          518,832,1417)
de_d <- c(0,0,0,0,0,0,0,0,0,2,0,1,3,0,2,4,1,0,0,8,25,22,27,32,23,49,
          55,72,64,66,128,149,140,145,141,184,92,173,254)
uk_d <- c(0,0,0,0,0,0,1,1,0,1,3,0,2,2,11,14,20,5,0,41,33,56,48,54,
          87,41,115,181,260,209,180,381,743,389,684,708,621,439,786)
all_countries_d <- list(it_d,es_d,ch_d,fr_d,de_d,uk_d)

## Computing exponential growth rates r
compute_r <- function(y, t, x){
  r <- c()
  for(i in 2:length(t)){
    new_r <- (y[i] / y[i-1])^(1/x) - 1
    if(abs(new_r) %in% c(Inf, NaN)) new_r <- NA
    r <- c(r, new_r)
  }
  return(r)
}

r_it <- compute_r(it, days, 1)
r_es <- compute_r(es, days, 1)
r_ch <- compute_r(ch, days, 1)
r_fr <- compute_r(fr, days, 1)
r_de <- compute_r(de, days, 1)
r_uk <- compute_r(uk, days, 1)
all_countries_r <- list(r_it,r_es,r_ch,r_fr,r_de,r_uk)

r_it_d <- compute_r(it_d, days, 1)
r_es_d <- compute_r(es_d, days, 1)
r_ch_d <- compute_r(ch_d, days, 1)
r_fr_d <- compute_r(fr_d, days, 1)
r_de_d <- compute_r(de_d, days, 1)
r_uk_d <- compute_r(uk_d, days, 1)
all_countries_r_d <- list(r_it_d,r_es_d,r_ch_d,r_fr_d,r_de_d,r_uk_d)

## Plotting
all_countries_names <- c('it','es','ch','fr','de','uk')
colors <- 1:length(all_countries_names)
pchs <- 1:length(all_countries_names)

pdf('figures/static-new-cases-with-growth-rate.pdf', height = 10, paper = 'a4')

par(mfrow=c(4,1))

# New cases per day
plot(days, unlist(all_countries[1]), main='New cases per day', type='o',
     ylim=c(0,max(unlist(all_countries),na.rm=T)),
     xaxt="n", col=colors[1], pch=pchs[1], ylab='new cases', xlab='day')
axis(1, at=days, labels=T)
for(i in 2:length(all_countries)){
  lines(days, unlist(all_countries[i]), type='o', col=colors[i], pch=pchs[i])
}
legend('topleft',legend=all_countries_names,col=colors,pch=pchs)

# Exponential growth new cases rate r per day
plot(days[-1], unlist(all_countries_r[1]), main='Exponential growth new cases rate r per day', type='o',
     ylim=c(min(unlist(all_countries_r),na.rm=T), max(unlist(all_countries_r),na.rm=T)),
     xaxt="n", col=colors[1], pch=pchs[1], ylab='r', xlab='day')
axis(1, at=days[-1], labels=T)
for(i in 2:length(all_countries_r)){
  lines(days[-1], unlist(all_countries_r[i]), type='o', col=colors[i], pch=pchs[i])
}
legend('topleft',legend=all_countries_names,col=colors,pch=pchs)


# New deaths per day
plot(days, unlist(all_countries_d[1]), main='New deaths per day', type='o',
     ylim=c(0,max(unlist(all_countries_d),na.rm=T)),
     xaxt="n", col=colors[1], pch=pchs[1], ylab='new deaths', xlab='day')
axis(1, at=days, labels=T)
for(i in 2:length(all_countries_d)){
  lines(days, unlist(all_countries_d[i]), type='o', col=colors[i], pch=pchs[i])
}
legend('topleft',legend=all_countries_names,col=colors,pch=pchs)

# Exponential growth new deaths rate r per day
plot(days[-1], unlist(all_countries_r_d[1]), main='Exponential growth new deaths rate r per day', type='o',
     ylim=c(min(unlist(all_countries_r_d),na.rm=T), max(unlist(all_countries_r_d),na.rm=T)),
     xaxt="n", col=colors[1], pch=pchs[1], ylab='r', xlab='day')
axis(1, at=days[-1], labels=T)
for(i in 2:length(all_countries_r_d)){
  lines(days[-1], unlist(all_countries_r_d[i]), type='o', col=colors[i], pch=pchs[i])
}
legend('topleft',legend=all_countries_names,col=colors,pch=pchs)

dev.off <- dev.off()