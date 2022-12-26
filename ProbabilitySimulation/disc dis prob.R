x=matrix(1:4,byrow=TRUE,nrow=1)
p=matrix(c(0.25,0.125,0.375,0.25),byrow=FALSE,nrow = 4,ncol = 1)

EX=x%*%p
x_sqr=x^2
EX_sqr=x_sqr%*%p
VX=EX_sqr-EX^2

print(EX)
print(VX)