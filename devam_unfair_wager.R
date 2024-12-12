trial_unfair_wager <- function() {
smx=0
while (smx<1) {
p_tmp=runif(1)
smx = smx + p_tmp
}
while (smx<2) {
q_tmp=runif(1)
smx =smx + q_tmp
}

if (q_tmp>p_tmp) {
  side='Y'
  #rs= q_tmp
} else {
  side='X'
  #rs= p_tmp
}
return (c(as.character(side),p_tmp,q_tmp))
}

unfair_wager_calc <- function(w) {
winner<-data.frame()
winner<-as.character(winner)
x_a <- y_a <- data.frame(0)
for (x in 1:w) {
tmp_wgr<-trial_unfair_wager()
winner<-rbind(winner,as.character(tmp_wgr[1]))
if (tmp_wgr[1]=="X") {
x_a <- rbind(x_a,tmp_wgr[2])
y_a <- rbind(y_a,tmp_wgr[3])
} else {
x_a <- rbind(x_a,tmp_wgr[2])
y_a <- rbind(y_a,tmp_wgr[3])
}
}
#paste(as.data.frame(table(winner)/length(winner)))
return (table(winner))
}


simulz <- function(lim) {
  a<-unfair_wager_calc(25)/25
  b<-a[1]
  for (d in seq(25,lim,by=25)){
    tm<-unfair_wager_calc(d)
    b<- rbind(b,tm[1]/d)
  }
  return (b)
}

simulz_ <- simulz(1000)
simulz_ <- as.data.frame(simulz_)
simulz_$index <- 1:nrow(simulz_)
loessMod10 <- loess(X ~ index, data=simulz_, span=0.10) # 10% smoothing span
loessMod25 <- loess(X ~ index, data=simulz_, span=0.25) # 25% smoothing span
loessMod50 <- loess(X ~ index, data=simulz_, span=0.50) # 50% smoothing span

lines(smoothed10,x =simulz_$index ,col="red") 
lines(smoothed25,x =simulz_$index ,col="green") 
lines(smoothed50,x =simulz_$index ,col="blue") 