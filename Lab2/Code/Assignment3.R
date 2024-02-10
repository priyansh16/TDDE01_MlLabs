
rm(list=ls()) #this is to clear all the previous data
a=0; b=0.5; c=1;n=20
set.seed(1)# fix random generator seed (same random simulations each time)
x=runif(n, min=0, max=b)# generate n uniform in the rectangle for x
y=runif(n, min=0, max=c) # generate n uniform in the rectangle for y
y_curve=(1/sqrt(2*pi))*exp(-x^2/2)
f_n=sum(y<y_curve) # count how many in the region I
f_n
area_I=(b-a)*c*f_n/n# compute the area of I
area_I

a=0; b=0.5; c=1;n=50
set.seed(1)# fix random generator seed (same random simulations each time)
x=runif(n, min=0, max=b)# generate n uniform in the rectangle for x
y=runif(n, min=0, max=c) # generate n uniform in the rectangle for y
y_curve=(1/sqrt(2*pi))*exp(-x^2/2)
f_n=sum(y<y_curve) # count how many in the region I
f_n
area_I=(b-a)*c*f_n/n # compute the area of I
area_I

a=0; b=0.5; c=1;n=500
set.seed(1)# fix random generator seed (same random simulations each time)
x=runif(n, min=0, max=b)# generate n uniform in the rectangle for x
y=runif(n, min=0, max=c) # generate n uniform in the rectangle for y
y_curve=(1/sqrt(2*pi))*exp(-x^2/2)
f_n=sum(y<y_curve)# count how many in the region I
f_n
area_I=(b-a)*c*f_n/n
area_I

a=0; b=0.5; c=1;n=10000
set.seed(1)# fix random generator seed (same random simulations each time)
x=runif(n, min=0, max=b)# generate n uniform in the rectangle for x
y=runif(n, min=0, max=c) # generate n uniform in the rectangle for y
y_curve=(1/sqrt(2*pi))*exp(-x^2/2)
f_n=sum(y<y_curve) # count how many in the region I
f_n
area_I=(b-a)*c*f_n/n
area_I
