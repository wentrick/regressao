pacman::p_load(tidyverse, readxl, ggcorrplot,GGally)

#1)
#X2 = 3

# Definir a função do polinômio
polinomio <- function(x1,x2) {
  return(25 + 3*x1 + 4*x2 + 1.5*x1*x2)
}

# Plotar o gráfico do polinômio
curve(polinomio(x1 =x ,x2 = 3), from = -10, to = 10, xlab = "x", ylab = "y", main = "Gráfico do Polinômio")


# Fazendo pelo ggplot2

# Criar um data frame com os valores de x
x <- seq(0, 20, length.out = 100)
df <- data.frame(x = x)

# Calcular os valores de y usando a função do polinômio
df$y <- polinomio(x1 = df$x, x2 = 3)

# Plotar o gráfico usando ggplot2
ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  labs(x = "x", y = "y", title = "Gráfico do Polinômio X=3")




#X2 = 6

# Definir a função do polinômio
polinomio <- function(x1,x2) {
  return(25 + 3*x1 + 4*x2 + 1.5*x1*x2)
}

# Plotar o gráfico do polinômio
curve(polinomio(x1 =x ,x2 = 6), from = -10, to = 10, xlab = "x", ylab = "y", main = "Gráfico do Polinômio")

# Criar um data frame com os valores de x
x <- seq(0, 20, length.out = 100)
df <- data.frame(x = x)

# Calcular os valores de y usando a função do polinômio
df$y <- polinomio(x1 = df$x, x2 = 6)

# Plotar o gráfico usando ggplot2
ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  labs(x = "x", y = "y", title = "Gráfico do Polinômio X=6")


#O modelo é aditivo podemos perceber que a escala ela dobra quando dobramos o X indicando que o valores sao maiores


#2)









#3)

















#4)
dados <- read_excel("dados/Dados_ex_6_5.xlsx", 
                    col_types = c("numeric", "numeric", "numeric"))


#a)

# Calcular a matriz de correlação
matriz_cor <- cor(dados)

# Converter a matriz de correlação em um data frame
df_cor <- as.data.frame(matriz_cor)

# Criar o gráfico de correlação (corr plot)
ggcorrplot(df_cor,lab = TRUE)

#matriz grafico de dispersao

ggpairs(dados) #podemos perceber que a variavel X2 é categorica (2,4)

dados = dados %>%
  mutate(X2 = as.factor(X2))

#b)

modelo = lm(Y ~ X1 + X2, data = dados)
summary(modelo)

cov_beta = vcov(modelo) #matriz de covariancia dos parametros
cov_beta
#c)


residuos = modelo$residuals

boxplot(residuo)


#fazendo pelo ggplot

# Calcular medidas resumo
medidas_resumo <- as.data.frame(residuos) %>%
  summarize(
    Min = min(residuos),
    Q1 = quantile(residuos, 0.25),
    Mediana = median(residuos),
    Q3 = quantile(residuos, 0.75),
    Max = max(residuos),
    Media = mean(residuos)
  )

# Criar o boxplot com medidas resumo
ggplot(dados, aes(y = residuos)) +
  geom_boxplot() +
  geom_point(data = medidas_resumo, aes(x=0,y = Media), color = "red", size = 3) +
  labs(x = "Residuos", y = "Valor", title = "Boxplot com Medidas Resumo")



#graficos de residuos

qqnorm(residuos)
qqline(residuos)
shapiro.test(residuos)


#d)

results = data.frame(modelo$model,modelo$fitted.values,modelo$residuals)

plot(results$modelo.fitted.values,results$Y)

plot(results$X1,residuos)

plot(results$X2,residuos)


#e)

SSR = anova(modelo)[[1,2]] + anova(modelo)[[2,2]] #soma de quadrados da regressao x1+x2
SSE = anova(modelo)[[3,2]] #soma de quadrados do residuo

n = length(dados$Y)

alfa = 0.05
est_obs = (SSR/2)/(SSE/n)^2

pchisq(est_obs,alfa,1)


#f)

glres = n-2

c = glres

results = results %>%
  mutate(y_barra = ave(Y, X1, FUN = mean))

SSPE = sum((results$Y - results$y_barra)^2)

SSLF = sum((results$y_barra - results$modelo.fitted.values)^2)


MSPE = SSPE/(n-c)

MSLF = SSLF/(c-2)


f_obs = MSLF/MSPE

pf(f_obs,n-2,n-c)

# 6.6


#a)


#b)

#c)

#calculo de B de bonferroni

p = length(colnames(dados)) - 1 #numero de parametros retirando Y
g = 2 #numero de betas a serem estimados
beta = modelo$coefficients
var_beta = diag(cov_beta)

b = qt(1-(alfa)/(2*g),n-p)


#calculo usando Bonferroni - beta 1

ic_lower <- beta[2] - b*var_beta[2] # Limite inferior do intervalo de confiança
ic_upper <- beta[2] + b*var_beta[2] # Limite superior do intervalo de confiança

cat("Intervalo de confiança de Bonferroni para beta 1: [", ic_lower, ",", ic_upper, "]")

#calculo usando Bonferroni - beta 2
ic_lower <- beta[3] - b*var_beta[3] # Limite inferior do intervalo de confiança
ic_upper <- beta[3] + b*var_beta[3] # Limite superior do intervalo de confiança

cat("Intervalo de confiança de Bonferroni para beta 1: [", ic_lower, ",", ic_upper, "]")

#6.7

#a)
anova(modelo)
SSTO = SSR+SSE

r_2 = SSR/SSTO
r_2
#b)


#6.8


#a

#estimativa intervalar da média de Y dado um x
alfa = 0.05
x_0 = c(1,5,4)


#calculo do t

t = qt(1-(alfa)/(2),n-p)

#calculo Bonferroni
y_new_hat <- x_0 %*% beta # Estimativa pontual da média de Y para o valor de X escolhido
var_y = t(x_0) %*% cov_beta %*% x_0
ic_lower <- y_new_hat - t*var_y # Limite inferior do intervalo de confiança
ic_upper <- y_new_hat + t*var_y # Limite superior do intervalo de confiança

cat("O valor estimado Y quando X1 e X2 =", c(x_0[2],x_0[3]), "é:",y_new_hat)
cat("Intervalo de confiança para a média de Y: [", ic_lower, ",", ic_upper, "]")


#b
m =1
MSE = mean(modelo$residuals^2) #quadrado médio do erro (MSE)
#calculo usando o erro padrao (verificar se esta correto) - Bonferroni
ic_lower <- y_new_hat - t* # Limite inferior do intervalo de confiança
ic_upper <- y_new_hat + t* # Limite superior do intervalo de confiança
var_y_pred = ((1/m)*(t(x_0) %*% cov_beta %*% x_0))
cat("O valor estimado Y quando X=", x_escolhido, "é:",y_new_hat)
cat("Intervalo de confiança de Bonferroni para a média de Y quando X =", x_escolhido, ": [", ic_lower, ",", ic_upper, "]")


