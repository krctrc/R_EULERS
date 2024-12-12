numbers_f <- function(a) {
m=floor(a/10000)
n=floor((a-(10000*m))/1000)
l=floor((a-(10000*m)-(1000*n))/100)
k=floor((a-(10000*m)-(1000*n)-(100*l))/10)
o=a-(10000*m)-(1000*n)-(100*l)-(10*k)
return (paste(m,n,l,k,o,sep=""))
}

sum_of_fourth <- function(m,n,k,l) {   
a=(m**4)+(n**4)+(l**4)+(k**4)
return (a)
}

sum_of_fifth <- function(m,n,k,l,o) {   
  a=(m**5)+(n**5)+(l**5)+(k**5)+(o**5)
  return (a)
}

fifth_sm=0

for (a in 1:9) {
  for (b in 0:9) {
    for (c in 0:9) {
      for (d in 0:9) {
        for (e in 0:9) {
            if (sum_of_fifth(a,b,c,d,e)==as.integer(numbers_f((a*10**4+b*10**3+c*10**2+d*10*1+e)))) {
          fifth_sm = fifth_sm + (a*10**4+b*10**3+c*10**2+d*10+e)
        }
      }}}}}
---240559

