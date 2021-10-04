

> **Super  Digit**

  

We  define  super  digit  of  an  integer using  the  following  rules: **x**

 
If has **x** only digit,  then  its  super  digit  is **x**. 

Otherwise,  the  super  digit  of **x** is  equal  to  the  super  digit  of  the  digit-sum  of **x** .  Here,  digit-sum  of  a  number  is  defined  as  the  sum  of  its  digits. 

For  example,  super  digit  of 9875 will  be  calculated  as:

    super_digit(9875)   = super_digit(9+8+7+5)
			    = super_digit(29)
			    = super_digit(2+9)
			    = super_digit(11)
			    = super_digit(1+1)
			    = super_digit(2)
			    = 2

You  are  given  two  numbers **n** and **k**. You  have  to  calculate  the  super  digit  of **P**. **P** is  created  when  number **n** is  concatenated **k** times.  That  is,  if **n = 123** and **k=3**, then **P = 123123123**.

**INPUT FORMAT**

The first line contains two space separated integers, ***n*** and ***k***

***Constraints***

> 1 < ***N*** < 10^(100000)
> 
> 1 < *K* < 10^5

***OUTPUT FORMAT***
Output the super digit of **P**, where **P** is created as described above.

*Sample Input 0*

> 148 3

Sample Output 0

> 3

***EXPLANATION***

    HERE n = 148 and k = 3, so P = 148148148.
	super_digit(P) = super_digit(148148148)
		    = super_digit(1+4+8+1+4+8+1+4+8)
		    = super_digit(39)
		    = super_digit(3+9)
		    = super_digit(12)
		    = super_digit(1+2)
		    = super_digit(3)
		    = 3


