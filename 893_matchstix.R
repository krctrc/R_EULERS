#EULER 893 MATCHSTICKS

#OPTIMIZE DEGERI RAKAM ,ARTI VE ÇARPI İLE GETIR

#10**6 YA KADAR TOPLAM 100 ' KADAR TOPLAM 100
# 1 2
# 2 5
# 3 5
# 4 4
# 5 5
# 6 6
# 7 3
# 8 7
# 9 6
# 0 6
# + 2
# X 2

symbols <- cbind(c(1,2,3,4,5,6,7,8,9,0,'+','x'),c(2,5,5,4,5,6,3,7,6,6,2,2))
symbols <- as.data.frame(symbols)
symbols$V2= as.integer(symbols$V2)
symbols$V2= symbols$V2+1
minimize_sticks <- function(k) {
#10*a + b
  stix=c()
    for (a in 1:9){
    for (b in 0:9){
      if (k==10*a+b) {
        term_1<-as.integer(symbols[a,][2])
        term_2<-as.integer(symbols[b,][2])
        stix<- c(stix,term_1*10+term_2)
      }
    }
  }
#a * b + c
  for (a in 1:9){
    for (b in 1:9){
      for (c in 0:9) {
        if (k==a*b+c) {
          term_1<-as.integer(symbols[a,][2])
          term_2<-as.integer(symbols[b,][2])
          term_3<-as.integer(symbols[c,][2])
            if (c!=0) {
              stix<-c(stix,term_1+term_2+term_3+4)
            } else
              stix<-c(stix,term_1+term_2+2)
        }
      }
    }
  }
    return (if (k%%10==0) 
            (symbols[k/10,][2]+6) else
      min(stix))
}


summation_stix <- function(j) {
  sm_stx<-0
  for (v in (1:j)) {
      sm_stx<- sm_stx +minimize_sticks(v)
  }
  return (sm_stx)
}