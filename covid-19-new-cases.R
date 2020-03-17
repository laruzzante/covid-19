## Exponential growth
# y = a * (1 + r)^x
# a = initial value (the amount before measuring growth or decay)
# r = growth or decay rate (most often represented as a percentage and expressed as a decimal)
# x = number of time intervals that have passed
# (y/a)^(1/x) - 1 = r


## March: New confirmed covid-19 cases per day. Data taken from daily situation reports from the WHO website.
days <- c('01.03','02.03','03.03','04.03','05.03','06.03','07.03',
         '08.03','09.03','10.03','11.03','12.03','13.03','14.03',
         '15.03','16.03')
it <- c(240,561,347,466,587,769,778,1247,1492,1797,977,2313,2651,2547,3497,3590)
es <- c(13,0,69,37,47,59,117,56,159,435,615,501,825,1266,1522,2000)
ch <- c(8,6,6,7,20,30,123,55,68,0,159,154,213,267,234,841)
fr <- c(43,0,91,21,73,138,193,93,410,286,372,495,591,780,829,911)
de <- c(0,72,28,39,66,272,105,156,317,27,157,271,802,693,733,1043)
uk <- c(3,13,3,12,20,29,49,43,67,46,50,87,134,208,342,251)
all_countries <- list(it,es,ch,fr,de,uk)


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


## Plotting
all_countries_names <- c('it','es','ch','fr','de','uk')
colors <- 1:length(all_countries_names)
pchs <- 1:length(all_countries_names)

par(mfrow=c(1,2))

# New cases per day
plot(days, unlist(all_countries[1]), main='New cases per day', type='o',
     ylim=c(0,max(unlist(all_countries),na.rm=T)),
     xaxt="n", col=colors[1], pch=pchs[1], ylab='new cases', xlab='day')
axis(1, at=days, labels=T)
for(i in 2:length(all_countries)){
  lines(days, unlist(all_countries[i]), type='o', col=colors[i], pch=pchs[i])
}
legend('topleft',legend=all_countries_names,col=colors,pch=pchs)

# Exponential growth rate r per day
plot(days[-1], unlist(all_countries_r[1]), main='Exponential growth rate r per day', type='o',
     ylim=c(min(unlist(all_countries_r),na.rm=T), max(unlist(all_countries_r),na.rm=T)),
     xaxt="n", col=colors[1], pch=pchs[1], ylab='r', xlab='day')
axis(1, at=days[-1], labels=T)
for(i in 2:length(all_countries_r)){
  lines(days[-1], unlist(all_countries_r[i]), type='o', col=colors[i], pch=pchs[i])
}
legend('topleft',legend=all_countries_names,col=colors,pch=pchs)
