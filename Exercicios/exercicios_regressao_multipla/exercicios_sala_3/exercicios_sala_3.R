pacman::p_load(tidyverse,Matrix)


#modelo linear multiplo
area = c(1:15)
safra_de_trigo = c(40,38,50,49,50,55,70,55,45,65,72,70,65,80,75)
fertilizante = c(100,150,200,250,300,350,400,410,450,500,550,600,650,700,800)
chuva = c(10,10,20,20,10,20,30,20,10,20,20,30,20,30,30)

dados = data.frame(area,safra_de_trigo,fertilizante,chuva)
dados


colnames(dados)

#nossas variaveis

n <- nrow(dados) # Number of observations
n

X <- cbind(dados$fertilizante,dados$chuva)
Y <- dados$safra_de_trigo

Y <- as.matrix(Y) 
Y   

J = matrix(data = 1, nrow = n, ncol = n)
J
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

#Fontes de variação

sse = t(e) %*% e #soma de quadrados do residuo
sse

ssr = t(beta) %*% t(X) %*% Y - (1/n) * (t(Y) %*% J %*% Y) #soma de quadrados da regressao
ssr

sst = ssr +sse  #soma de quadrados total
sst
#outra forma de estimação do parametro
sse = t(Y) %*% Y - t(beta) %*% t(X) %*% Y
sse

mse = sse/(n-2)
mse
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

#estiamção pontual
xh = c(1,500,25)
yh = xh %*% beta
m = 4
alfa = 0.10
t_value = qt(1-alfa/2,n-2)

xh %*% XTX_inv %*% xh

ic_sup = yh + t_value * sqrt(mse*((1/m)+(xh %*% XTX_inv %*% xh)))
ic_inf = yh - t_value * sqrt(mse*((1/m)+(xh %*% XTX_inv %*% xh)))
cat("Intervalo de confiança para",xh,": [", ic_inf, ", ", ic_sup, "]\n")

#rodando direto no r
p = length(beta)
#soma de quadrados extra

modelo_completo = lm(safra_de_trigo ~ fertilizante + chuva ,data = dados)
modelo_1 = lm(safra_de_trigo ~ fertilizante ,data = dados)

summary(modelo_completo)
anova(modelo_completo)

#find sse
sse <- sum((fitted(modelo_completo) - dados$safra_de_trigo)^2)
sse

#find ssr
ssr <- sum((fitted(modelo_completo) - mean(dados$safra_de_trigo))^2)
ssr

#find sst
sst <- ssr + sse
sst

#r squared

r2 = ssr / sst
r2

#SSR(X2|X1)
ssr1 <- sum((fitted(modelo_1) - mean(dados$safra_de_trigo))^2)

ssrx1x2 = ssr - ssr1


#tabela anova na mao
anova_table <- data.frame(Fonte_de_variacao = c("Regressao", "X1", "X2|X1","Residuos", "Total"),
                          GL = c(3,1,1,n-4, n-1),
                          SS = c(ssr,ssr1 ,ssrx1x2 ,sse, sst),
                          MQ = c(round(ssr/3,p-1),round(ssr1,2) ,round(ssrx1x2,2), round(sse/(n-p),2), ''),
                          stringsAsFactors = FALSE)
rownames(anova_table) <- NULL

anova_table

