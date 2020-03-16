## Exponential growth
# y = a * (1 + r)^x
# a = initial value (the amount before measuring growth or decay)
# r = growth or decay rate (most often represented as a percentage and expressed as a decimal)
# x = number of time intervals that have passed
# (y/a)^(1/x) - 1 = r


## March: New confirmed covid-19 deaths per day. Data taken from daily situation reports from the WHO website.
days <- c('01.03','02.03','03.03','04.03','05.03','06.03','07.03',
          '08.03','09.03','10.03','11.03','12.03','13.03','14.03',
          '15.03')
it <- c(8,6,17,28,27,41,49,37,132,97,168,196,189,252,173)
es <- c(0,0,0,0,1,2,2,0,5,18,8,12,36,36,16)
ch <- c(0,0,0,0,0,1,0,1,0,0,1,1,2,0,5)
fr <- c(0,0,1,1,0,2,3,1,9,11,3,15,13,18,12)
de <- c(0,0,0,0,0,0,0,0,0,2,0,1,3,0,2)
uk <- c(0,0,0,0,0,0,1,1,0,1,3,0,2,2,11)
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
plot(days, unlist(all_countries[1]), main='New deaths per day', type='o',
     ylim=c(0,max(unlist(all_countries),na.rm=T)),
     xaxt="n", col=colors[1], pch=pchs[1], ylab='new deaths', xlab='day')
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
