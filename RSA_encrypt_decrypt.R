#call gmp library
library("gmp")
#sieve of eratosthenes copied from stackoverflow: http://stackoverflow.com/questions/3789968/generate-a-list-of-primes-in-r-up-to-a-certain-number
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
#get a normal-sized factor of d. if it cna't find one, it starts the whole process over
getFactor <- function(x){
  if (isprime(d) == 0){
    for(i in 10000:100000){
      if(x%%i == 0)
        return(i)
    }
    rsa_full(sentence)
  }
}


#function to generate all required values for the function
generate_values <- function(sentence) {
  sentence <<- sentence
  ascii_sentence <<- ascii_conversion(sentence)
  #randomly choose two primes under 1000000 and order them and place in p, q
  two_primes <- sort(sample(sieve(1000000), 2, replace=FALSE))
  p <<- two_primes[1]
  k <<- two_primes[2]
  #find their product and place in n
  n <<- as.bigz(p)*as.bigz(k)
  #totient of a number whose only factors are two primes a,b is (a-1)(b-1)
  #find totient of primes_product and place in m
  m <<- (p-1)*(k-1)
  #finding e|e is coprime to m
  for (i in 2:1000000){
    if (gcd.bigz(m, i) == 1){
      e <<- i
      break
    }  
  }
  #find d such that de%%m==1. if d is prime, start over (because we need to factor it)
  for (x in 1:1000000){
    if ( (1+x*m)%%e == 0 && isprime((1+x*m)/e)==0 ){
      d <<- (1+x*m)/e
      if (isprime(d) != 0){
        rsa_full(sentence)
      }
      break
    }
  }
}

#now: public key is (e, n), private key is (d, n) 

#encode using public key
encode <- function(ascii_sentence, e, n){
  encoded <<- as.bigz(ascii_sentence)^e %% n
}

decode <- function(encoded, d, n) {
  #decode using private key and factored d (to help it run)
  #set i as the discovered factor of d
  f <<- getFactor(d)
  #set o to another factor of d that multipels i to get d
  o <<- d/f
  #use modulus exponentiation to read decoded string
  decoded <- pow.bigz(encoded,min(o,f)) %% n
  decoded <- pow.bigz(decoded,max(o,f)) %% n
  decoded <<- decoded
  #fully decode into original characters
  fully_decoded <<- char_conversion(as.integer(decoded))
}

#create full function with only input being the message
rsa_full <- function(sentence){
  #generate all initial values needed
  generate_values(sentence)
  #encode using public key
  encode(ascii_sentence, e, n)
  #decode using private key
  decode(encoded, d, n)
  #print everything
  print(sentence)
  print(ascii_sentence)
  print(as.character(encoded))
  print(as.character(decoded))
  print(fully_decoded)
}