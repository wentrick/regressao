pacman::p_load(tidyverse,Matrix)

#modelo linear simples em termos matriciais
lote = c(1:10)
tamanho_lote = c(30,20,60,80,40,50,60,30,70,60)
horas_homem = c(73,50,128,170,87,108,135,69,148,132)

dados = data.frame(lote,tamanho_lote,horas_homem)


#nossas variaveis

n <- nrow(dados) # Number of observations
n

X <- dados$tamanho_lote
Y <- dados$horas_homem

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

#Vamos repetir o processo usando os dados do exercicio 1  para comparacao

#exercicios de regressao onde x é a massa sem gordura e y é a taxa do metabolismo

massa = c(62.0,62.9,36.1,54.6,48.5,42.0,47.4,50.6,42.0,48.7,40.3,33.1,51.9,42.4,34.5,51.1,41.2,51.9,46.9)
taxa = c(1792,1666,995,1425,1396,1418,1362,1502,1256,1614,1189,913,1460,1124,1052,1347,1204,1867,1439)

n = length(massa)

dados = data.frame(massa,taxa)

#nossas variaveis

n <- nrow(dados) # Number of observations
n

X <- dados$massa
Y <- dados$taxa

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
sse = t(e) %*% e #soma de quadrados do residuo
sse

mse = sse/(n-2)
mse
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

#variancia de Y

v_Y = mse * identity(2)


