# (#\0 . 0)

import string
s = "(vector 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5)"
cont = 0
for i in s:
    if i =="5" or i =="2":
        cont +=1
     

s2=""
for i in range(74):
    s2= s2+"5"+" "
print (s2)
