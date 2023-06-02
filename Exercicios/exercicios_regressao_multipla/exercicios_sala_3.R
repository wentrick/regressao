pacman::p_load(tidyverse,Matrix)
#modelo linear multiplo
area = c(1:15)
safra_de_trigo = c(40,38,50,49,50,55,70,55,45,65,72,70,65,80,75)
fertilizante = c(100,150,200,250,300,350,400,410,450,500,550,600,650,700,800)
chuva = c(10,10,20,20,10,20,30,20,10,20,20,30,20,30,30)

dados = data.frame(area,safra_de_trigo,fertilizante,chuva)
dados

#nossas variaveis

n <- nrow(dados) # Number of observations
n

X <- cbind(dados$fertilizante,dados$chuva)
Y <- dados$safra_de_trigo

Y <- as.matrix(Y) 
Y   

#intercepto
X <- cbind(rep(1,n), X) # Adding a column of ones to make the X matrix
X                       # This is your X matrix

#encontrando XTX

XTX = t(X) %*% X
XTX

#encontrando XTY

XTY = t(X) %*% Y
XTY

#encontrando a inversa de XTX

XTX_inv = solve(XTX)
XTX_inv

#encnontrando a matriz de parametros Beta (ela contem o Beta 0 e o Beta 1 ou todos os beta no caso de uma regressao multipla)

beta = XTX_inv %*% t(X) %*% Y
beta

#valores estimados pelo modelo (y chapeu)
Yhat <- X %*% beta # Fitted Values
Yhat

#residuos do modelo
e <- Y - Yhat # Residuals
e

#estimador da variancia residual
sse = t(e) %*% e
sse

#outra forma de estimação do parametro
sse = t(Y) %*% Y - t(beta) %*% t(X) %*% Y
sse

sse/(n-2)

#matriz de covariancias
C = (solve(t(X) %*% X)) %*% t(X)
C

beta = C %*% Y
beta

#variancia de beta (matriz de covariancias estimadas)

v_beta = as.numeric(mse) * XTX_inv #tive que transforma em numerico pq o resultado era uma matriz 1x1 e tava dando erro
v_beta
#variancia de Y

v_Y = mse * identity(2)




