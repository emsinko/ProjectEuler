## PROBLEM 2

# Each new term in the Fibonacci sequence is generated by adding the previous two 
# terms. By starting with 1 and 2, the first 10 terms will be:

#              1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
# By considering the terms in the Fibonacci sequence whose values do not 
# exceed four million, find the sum of the even-valued terms.

import numpy as np

fibo = [1, 2]
OK = True
while OK:
    if(fibo[-1] < 4000000):
        fibo.append(fibo[-1] + fibo[-2])
    else:
        OK = False
fibo = np.array(fibo)
        
np.sum(fibo[fibo % 2 == 0])
