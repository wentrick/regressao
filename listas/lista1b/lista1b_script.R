#### 1A ----


#estimativa intervalar da média de Y dado um x
beta_0 = 100
beta_1 = 20
x_escolhido = 5
sigma_quadrado = 25
y_new_hat <- beta_0 + beta_1*x_escolhido #media
#se assumirmos que os erros seguem normalidade podemos calcular a partir de uma normal de media 200 e desvio padrao 25


n_value_lower <- pnorm(195, mean = y_new_hat,sd = 25)
n_value_upper <- pnorm(205, mean = y_new_hat,sd = 25)


cat("O valor estimado Y quando X=", x_escolhido, "é:",y_new_hat)
#cat("Intervalo de confiança para a média de Y quando X =", x_escolhido, ": [", ic_lower, ",", ic_upper, "]")

prob = n_value_upper-n_value_lower
cat("A probabilidade de Y estar entre 195 e 200 é:",prob)

#### 2A ----

beta_0 = 200
beta_1 = 5
sigma = 4
sigma_quadrado = sigma^2

#para x = 10
x_escolhido = 10
y_new_hat <- beta_0 + beta_1*x_escolhido #media

cat("A distribuicao de x=10 é: N(",y_new_hat,",",sigma_quadrado,")")
#para x = 25
x_escolhido = 25
y_new_hat <- beta_0 + beta_1*x_escolhido #media

cat("A distribuicao de x=10 é: N(",y_new_hat,",",sigma_quadrado,")")