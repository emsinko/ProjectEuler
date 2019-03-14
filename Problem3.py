## PROBLEM 3
import numpy as np

def is_prime(x):
    suma = 0
    if x == 2 or x == 3:
        return(True)
    elif x < 2 or x==4 :
        return(False)
    for i in np.arange(2, int(np.sqrt(x)) + 1):
        if(x % i == 0):
            suma += 1
    return(suma == 0)


num = 600851475143
i = 2
OK = True
while OK: 
    if(num % i == 0):
        if(is_prime(num/i)):
            print(num/i)
            OK = False
    i += 1
