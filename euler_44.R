pentagon_gn <- function(bas,bit) {
sv=c()
gn<-seq(bas,bit,1)
for (x in gn) {
  sv=append(sv,x*(3*x-1)/2)
}
return (sv)
}


pair_pentagon<- function(h,w) {
  pent_d = 0
  pt<-pentagon_gn(h,w) 
  rs <- c()
  frk=0
  tpl=0
  for (x in 1:length(pt)){
    for (y in 1:length(pt)){
      if (x!=y) {
        frk<- abs(pt[x]-pt[y])
        tpl<- pt[x]+pt[y]
      }
      if (any(frk==pt) && any(tpl==pt)) {
        rs=append(rs,frk)
      }
    }
  }
  pent_d = min(is.null(rs),0)
  
  return (pent_d)
  }

