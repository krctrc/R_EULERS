# problem 1
a=c()
for (x in 1:909) {
  if (x%%3==0 | x%%5==0) {
    a=append(a,x)
  }
}
sum(a)

# problem 2
f=1
s=2
fibo=c(f,s)

for (x in 1:40) {
  tmp =f+s
  if (tmp<4000000) {
  f =s
  s =tmp
  fibo<-append(fibo,s)
  }
}
length(fibo)
fibo[32]

#problem 3
is.prime <- function(x) { 
  if (x == 1) {
    flag <- FALSE
  } 
  if (x == 2 | x == 3) {
    flag <-  TRUE
  } 
  else {
    flag <- !any(x %% 2:(floor(sqrt(x))) == 0)
  }
  return(flag)
}

l <- 600851475143
n <- floor(sqrt(l))
flag <- logical(n)
for (i in 1:n) {
  flag[i] <- is.prime(i)
}
p <- (1:n)[flag]
pfac <- prime[(l %% p) == 0]
pmax <- max(pfac)


#problem 4
paliondrome <- function(nm) {
nm_str <- as.character(nm)
split_nm_str<-substring(nm_str,1:nchar(nm_str),1:nchar(nm_str))
nch<-length(split_nm_str)
rs <- ''
for (i in nch:1) {
  rs<- paste(rs,split_nm_str[i],sep="")
}
  return (nm_str==rs)
}
pal_n=100001
for (i in 1000:100){
  for (j in 1000:100){
    if (paliondrome(i*j)) {
      tmp=i*j
      if (tmp>pal_n)
      pal_n <- tmp
    }}}

#problem 5
sm_mltp=1
for (i in 20:2) {
  if (sm_mltp%%i!=0)
    sm_mltp = i*sm_mltp
  }

for (i in 20:2){
  if (factorial(i)%%(i*2)==0)
    sm_mltp = sm_mltp/i
}



#problem 6
sm_sq <- sum(sapply(1:1000,function(x) x**2))
sq_sm <- sum(1:1000)**2
sq_sm-sm_sq


#problem 9
for (x in 1:1000) {
  for (y in x:1000-x) {
    z=1000-x-y
      if (x**2 + y**2 == z**2){
         x
         y
         z
      }
  }
}


#problem 10
sm=3#c(1,2)
sm_not_prm=1#c(1)
for (x in 3:1415) {
  sm =sm+x#append(sm,x)
  for (i in 2:(x-1)) {
   if (x%%i==0){
     sm_not_prm = sm_not_prm+x#append(sm_not_prm,x)
     break
   }
  }
}


#problem 12
n=7
cnt_div<-1
while (cnt_div<500) {
total<-sum(1:n)
cnt_div <- 1
for (x in 1:round(total/2)) {
  if (total%%x==0) {
    cnt_div=cnt_div+1
  }
}
n=n+10
}
????

#problem 14
collatz <- function(start_t) {
step = 1
while(start_t!=1) {
if (start_t%%2==1) {
  start_t<-start_t*3 +1
  step =step+1
} else if (start_t%%2==0) {
  start_t<-start_t/2
  step =step+1
}
}
return (step)
}
coll_s <- 10
for (x in 13:1000000) {
  if(collatz(x)>coll_s)
    coll_s <- collatz(x)
  }

coll_s
#525

#problem 16
num_str <- as.character(2**1000)
split_num_str<-substring(num_str,1:nchar(num_str),1:nchar(num_str))
split_num_str<-split_num_str[-c(2,17:21)]
nw<-length(split_num_str)
sm=0
for (i in nw:1) {
  s=as.integer(split_num_str[i])
  sm = s + sm
}
#59

#problem 20
factorial(10)
fact_str <- as.character(factorial(100))
split_fact_str <- substring(fact_str,1:nchar(fact_str),1:nchar(fact_str))
sm_f=0
nc <- length(split_fact_str)
for (i in 1:nc) {
  s=as.integer(split_fact_str[i])
  sm_f = s + sm_f
}
#????


#problem 21
proper_divs <- function(a) {
  su_div=0
  if (a ==1) {
    su_div=1
  }
  else {
  for (x in 1:(a-1)) {
  if (a%%x ==0) {
    su_div = su_div + x
  }
  }
  }
  return (su_div)
}
amicable <- function(h) {
  amic =c()
  for (a in 2:h) {
    b=proper_divs(a)
    if ( proper_divs(b)==a ) {
      amic = append(amic,a)
    }
  }
  amic = unique(amic)
  return (amic)
}  
sum(amicable(10000))
#40284



#euler 23
sum(1:28122)
abundant <- function(x) {
  sm_e=0
  for (y in 1:(x-1)) {
    if (x%%y==0) {
    sm_e=sm_e + y
    }
    }
  if (sm_e>x)
    return (TRUE)
}
abund <-c()

for (x in 12:28122) {
  if (abundant(x))
    abund<- append(abund,x)
}
#????

#euler 24
as.integer(as.character(012))
ints <- c(0,1,2,3,4,5,6,7,8,9)
n=length(ints)
#permutstion?


#euler 25
karsay<-nchar(as.character(112))
f=1
s=1
term_num <-2


while (karsay<1000){
  tmp =f+s
    f =s
    s =tmp
    term_num <-term_num+1
    karsay<-nchar(as.character(s))
  }

#euler 32
three_prod <- function() {
  pand=c()
  for (i in 10:99){
    for (j in 100:999){
  product<-as.character(i*j)
  str_rs<-paste(as.character(i),as.character(j),as.character(i*j),sep="")
  rs<-substring(str_rs,1:nchar(str_rs),1:nchar(str_rs))
  if (length(rs)==9)
  if (all(sort(as.integer(rs))==c(1,2,3,4,5,6,7,8,9)))
    pand<-append(pand,c(i,j,i*j))
    }
  }
  return (pand)
}
three_prod()

#euler 33
cancel_fract <- function() {
  fract=c()
  for (i in 10:99){
    for (j in 10:99){
      if(i%%10==0 | j%%10==0 | i==j)
        next
      str_i <- substring(as.character(i),1:2,1:2)
      str_j <- substring(as.character(j),1:2,1:2)
      if(str_i[1]==str_i[2] | str_j[1]==str_j[2])
        next
      diff_i<- setdiff(str_i,str_j)
      diff_j<- setdiff(str_j,str_i)
      if (all(i/j==as.integer(diff_i)/as.integer(diff_j)) ) {
        fract <- append(fract,paste(min(i,j),max(i,j),"-"))
      }   
    }
  }
  return (unique(fract))
}

cancel_fract()

#EULER 34
#intr<-145
while(intr!=sm_fact){
for (intr in 10:1000){
str_int <- as.character(intr)
split_str_int<-substring(str_int,1:nchar(str_int),1:nchar(str_int))
sm_fact<-0
for (i in 1:length(split_str_int)){
  sm_fact<- sm_fact+factorial(as.integer(split_str_int[i]))
}
}
}#???

#euler 37
#pr_int<-3797
trunc_prm <- c()
pr_int=10
while(length(trunc_prm)<11) {
str_pr_int <- as.character(pr_int)
left<-substring(str_pr_int,1:nchar(str_pr_int),nchar(str_pr_int))
right<-substring(str_pr_int,1,1:nchar(str_pr_int))
pr_int<- pr_int+1
if (length(left)==length(intersect(left,prime)) & length(right)==length(intersect(right,prime)))
  trunc_prm<- append(trunc_prm,pr_int)
}

#euler 39

trian=c()
for (n in 120:1000){
  k=0
for (x in 1:n/2) {
for (a in 1:(n-x)){
  if (a**2 + x**2 == (n-a-x)**2)
    k=k+1
}  
}
  trian<-append(trian,k)
}
trian<-cbind(seq(120,1000,1),trian)
trian[trian[,2]==max(trian[,2])]
#840


#euler 49
x1=1487
x2=x1 + 3330
x3=x1 + 3330 + 3330
!any(x1 %% 2:(floor(sqrt(x1))) == 0)
x4=c()
for (a in 1000:10000) {
  if (
  (!any(a %% 2:(floor(sqrt(a))) == 0)) &
  (!any((a+3330) %% 2:(floor(sqrt(a+3330))) == 0)) &
  (!any((a+6660) %% 2:(floor(sqrt(a+6660))) == 0)) &
  a<10000 & a+3330<10000 & a+6660<10000
  )
    x4 = append(x4,a)
}




#EULER 45
z1=c()
z2=c()
z3=c()
for(z in 1:30000) {
z1=append(z1,z*(z+1)/2)
z2=append(z2,z*(3*z-1)/2)
z3=append(z3,z*(2*z-1))
}
cat(intersect(z1,intersect(z2,z3)))


#euler 46
for (i in seq(3,10000,2)) {
       if (any(prime==i))
             next
       x <- sqrt((i-prime[prime<i])/2)
       if (any(round(x) == x)) {
             next
        } else {
               cat (i, "\n")
           }
}


#EULER 52
perm_mult<-125874
perm_mult_ar<-c(perm_mult,perm_mult*2,perm_mult*3,perm_mult*4,perm_mult*5,perm_mult*6)
split_str_perm_mult=c()

for (i in 1:length(perm_mult_ar)){
str_perm_mult <- as.character(perm_mult_ar[i])
perm_mult_sw<- substring(str_perm_mult,1:nchar(str_perm_mult),1:nchar(str_perm_mult))
pst<-paste(unique(perm_mult_sw))
split_str_perm_mult <-append(split_str_perm_mult,pst)
}
#??????
  

#EULER 53
n<-23
r<-10
factorial(n)/(factorial(r)*factorial(n-r))
t=0
for(n in 23:100){
  for (r in 1:n){
    if (factorial(n)/(factorial(r)*factorial(n-r))>1000000) 
      t=t+1
  }} 
#4075

#EULER 63
a=c()
for (n in 1:10)
for (i in 1:10){
  if (i==nchar(as.character(n**i)))
    a=c(a,n**i)
}

