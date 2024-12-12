prm_sum_int <- function(a,b) {
  d=c()
for (x in a:b) {
  for (j in 2:x) {
    if (x%%j==0 && x!=j) {
      tmp=x      
    }
  }
  d=append(tmp,d)
}

return (sum(setdiff(seq(a,b),unique(d))))
}

prm_sum <- function() {
  sm=0
  for (v in 1:2000) {
  sm= sm+prm_sum_int((1000)*(v-1)+1,1000*v+1)
  }
  return (sm)
}