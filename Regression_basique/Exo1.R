x = c(-17,-15,-5,4,13,20,29,36,37,44)
y = c(-6,-13,-20,-22,-29,-39,-42,-47,-53,-54)
# regression de y sur x de la forme :  y = ax + b
N = length(y)

b = rep(1,N)
a = x

M = cbind(b, a)
Beta_h = solve(t(M)%*%M)%*%t(M)%*%y
Beta_h
SSR = sum( (y - M%*%Beta_h)^ 2) #Plus il est petit, plus la regression est bonne

# b -21.4703229
# a  -0.7554573

# SSR = Somme des carrés des residus en français

