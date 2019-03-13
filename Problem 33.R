## PROBLEM 33 

library(MASS)
product <- 1

for(a in 1:9){
  for(b in 1:9){
    for(c in 1:a){
      for(d in 1:9){
       if(10*a+b <= 10*c+d) break
       if(a == c & (d/b == ((10*c+d)/(10*a+b)))) { product <- product * (10*c+d)/(10*a+b) ; break}
       if(a == d & (c/b == ((10*c+d)/(10*a+b)))) { product <- product * (10*c+d)/(10*a+b) ; break}
       if(b == c & (d/a == ((10*c+d)/(10*a+b)))) { product <- product * (10*c+d)/(10*a+b) ; break}
       if(b == d & (c/a == ((10*c+d)/(10*a+b)))) { product <- product * (10*c+d)/(10*a+b) ; break}
      }
    }
  }
}

product
as.fractions(product)
