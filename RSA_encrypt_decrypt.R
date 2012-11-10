library("gmp")
library("schoolmath")
#Euler's totient function
phi <- function(x){
  counter <- 0
  for (i in 1:(x-1)){
    if (gcd.bigz(x,i)==1){
      counter <- counter + 1
    }
  }
  if (x==1){counter=1}
  return(counter)
}
#sieve copied from stackoverflow: http://stackoverflow.com/questions/3789968/generate-a-list-of-primes-in-r-up-to-a-certain-number
sieve <- function(n)
{
  n <- as.integer(n)
  if(n > 1e8) stop("n too large")
  primes <- rep(TRUE, n)
  primes[1] <- FALSE
  last.prime <- 2L
  fsqr <- floor(sqrt(n))
  while (last.prime <= fsqr)
  {
    primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
    sel <- which(primes[(last.prime+1):(fsqr+1)])
    if(any(sel)){
      last.prime <- last.prime + min(sel)
    }else last.prime <- fsqr+1
  }
  which(primes)
}
#convert sentence to ascii
ascii_conversion <- function(x) {
  strtoi(charToRaw(x), 16L)
}
#convert ascii to characters
char_conversion <- function(n) {
  rawToChar(as.raw(n))
}
#sieve of eratosthenes (modified from Beckwith's Python code)
# soe <- function(x){
#   prime_numbers <- rep(TRUE, x)
#   prime_numbers[1] <- FALSE
#   final.prime <- as.integer(2)
#   for (i in final.prime:ceiling(sqrt(x))){
#     prime_numbers[seq.int(as.integer(2)*final.prime, x, final.prime)] <- FALSE
#     final.prime <- final.prime + min(which(prime_numbers[(final.prime+1):x]))
#   }
#   which(prime_numbers)
# }

sentence <- "Hi Jackson!"

ascii_sentence <- ascii_conversion(sentence)

#randomly choose two primes under 1000000 and order them and place in p, q
two_primes <- sort(sample(sieve(10000), 2, replace=FALSE))
p <- two_primes[1]
q <- two_primes[2]
#find their product and place in n
n <- as.bigz(p)*as.bigz(q)
#totient of a number whose only factors are two primes a,b is (a-1)(b-1)
#find totient of primes_product and place in m
m <- (p-1)*(q-1)

#finding e|e is coprime to m
for (i in 2:1000000){
  if (gcd.bigz(m, i) == 1){
    e <- i
    break
  }  
}

#find d such that de%%m==1
for (x in 1:1000000){
  if ( (1+x*m)%%e == 0  ){
    d <- (1+x*m)/e
    break
  }
}

#now: public key is (e, n), private key is (d, n) 

#encode using public key
encoded <- character()
for (x in 1:length(ascii_sentence)){
  encoded[x] <- as.character(as.bigz(ascii_sentence[x])^e %% n)
}

#decode using private key
decoded <- character()
for (y in 1:length(encoded)){
  decoded[y] <- as.integer(pow.bigz(encoded[y],d) %% n)
}

#fully decode into original characters
fully_decoded <- char_conversion(decoded)

#print everything
print(sentence)
print(ascii_sentence)
print(encoded)
print(decoded)
print(fully_decoded)