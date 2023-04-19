pacman::p_load(tidyverse)

#exercicios de regressao onde x é a massa sem gordura e y é a taxa do metabolismo

massa = c(62.0,62.9,36.1,54.6,48.5,42.0,47.4,50.6,42.0,48.7,40.3,33.1,51.9,42.4,34.5,51.1,41.2,51.9,46.9)
taxa = c(1792,1666,995,1425,1396,1418,1362,1502,1256,1614,1189,913,1460,1124,1052,1347,1204,1867,1439)

n = length(massa)

dados = data.frame(massa,taxa)

#vamos calcular o beta 0  e beta 1 manualmente com arredondamentos

dados = dados %>%
  mutate(estimado = 117.44 +26.79*massa,
         residuo = taxa - estimado,
         taxa_quadrado = taxa**2,
         massa_quadrado = massa**2,
         massa_x_taxa = massa*taxa)

sum(dados$residuo)

#vamos melhorar nossas estimativas pois devidos aos erros acumulados a soma dos residuos nao deu 0

x_barra = mean(dados$massa) #media amostral de x 
y_barra = mean(dados$taxa) #media amostra de y
x = sum(dados$massa) #somatorio de xi
y = sum(dados$taxa) #somatorio de yi
x_quadrado = sum(dados$massa_quadrado) #somatorio de xi ao quadrado
y_quadrado = sum(dados$taxa_quadrado) #somatorio de yi ao quadrado
xy = sum(dados$massa_x_taxa) #somatorio de x vezes y

beta_1 = (xy - n*x_barra*y_barra)/(x_quadrado-n*(x_barra)**2) #calculo do estimador de beta 1
beta_0 = y_barra - beta_1*x_barra #calculo do estimador de beta 0

dados = dados %>%
  mutate(estimado = 117.44 +26.79*massa,
         estimado_melhor = beta_0+beta_1*massa,
         residuo = taxa - estimado,
         residuo_melhor = taxa - estimado_melhor,
         taxa_quadrado = taxa**2,
         massa_quadrado = massa**2,
         massa_x_taxa = massa*taxa)

erro = sum(dados$residuo_melhor)

# agora sim temos um valor muito mais proximo de zero


#calculando os estimador da variancia dos erros 
erro_quadrado = sum(dados$residuo_melhor**2)

sigma_quadrado = erro_quadrado/(n - 2)

sigma = sqrt(sigma_quadrado)

erro_padrao = sigma/sqrt(n)

coeficiente_variacao = sigma/y_barra #desvio padrao divido pela media

## rodando o modelo dentro do R para verificar os calculos

lm(dados$taxa ~ dados$massa) %>% summary()

## estimando a variancia do beta 0 e beta 1

s2_beta_1 = (sigma_quadrado/(x_quadrado - n* x_barra^2))

s_beta_1 = sqrt(s2_beta_1)

coeficiente_variacao_beta_1 = s_beta_1/beta_1


s2_beta_0 =  sigma_quadrado*(x_quadrado/(n*(x_quadrado - n*x_barra**2)))

s_beta_0 = sqrt(s2_beta_0)

coeficiente_variacao_beta_0 = s_beta_0/beta_0 #variancia maior pois incorpara a variancia de beta 1 (no caso ele repsresenta someente o intercepto ent nao faz mt diferenca)



#estimando intervalo de confianca para beta 1 com 95% de confianca (os sinais estao "trocados pq nao usei o lower.tail = FALSE")
alfa = 0.05
ic_beta_1  = c(beta_1-qt(alfa/2, n-2,lower.tail = FALSE)*s_beta_1,beta_1+qt(alfa/2, n-2,lower.tail = FALSE)*s_beta_1)
##

fit <- lm(dados$taxa ~ dados$massa)

# Cálculo do intervalo de confiança para beta1 com nível de significância de 0.05
alpha <- 0.05
se <- summary(fit)$coefficients[2, 2]
beta1 <- summary(fit)$coefficients[2, 1]
t_crit <- qt(1 - alpha/2, df = fit$df.residual)
lower <- beta1 - t_crit * se
upper <- beta1 + t_crit * se
cat("Intervalo de confiança para beta1: [", lower, ", ", upper, "]\n")
# Cálculo do intervalo de confiança para beta0 com nível de significância de 0.05
alpha <- 0.05
se <- summary(fit)$coefficients[1, 2]
beta1 <- summary(fit)$coefficients[1, 1]
t_crit <- qt(1 - alpha/2, df = fit$df.residual)
lower <- beta1 - t_crit * se
upper <- beta1 + t_crit * se
cat("Intervalo de confiança para beta0: [", lower, ", ", upper, "]\n")
##
#estimando intervalo de confianca para beta 0 com 95% de confianca 
alfa = 0.1

ic_beta_0  = c(beta_0-qt(alfa/2, n-2,lower.tail = FALSE)*s_beta_0,beta_0+qt((alfa/2), n-2,lower.tail = FALSE)*s_beta_0)

#teste de hipotese sobre beta 1 (teste de ausencia de regressao)

#Ho: beta_1 = 0
#H0: beta_1 > 0 (ou != 0)
alfa = 0.95
teste_stat = beta_1/s_beta_1

rc = qt(alfa,n-2) #regiao critica

pvalor = pt(teste_stat,n-2, lower.tail = FALSE) #pvalor

pvalor < alfa #pvalor menos que o alfa de 5% logo rejeitamos H0

#logo existe uma regressao ou seja beta 1 contribui para a relacao da massa sem gordura e o metabolismo


#teste de hipoteses sobre b0

#
#



#teste de hipotese e intervalo de confianca sobre sigma quadrado

gl = n-2
alfa = 0.1

qchisq(alfa/2,gl)

qchisq(alfa/2,gl,lower.tail = FALSE)

ic_sigma_quadrado = c(((n-2)*sigma_quadrado/qchisq(alfa/2,gl,lower.tail = FALSE)),((n-2)*sigma_quadrado/qchisq(alfa/2,gl)))

#ou podemos usar os residuos - verificar nos slides ta dando errado

#ic_sigma_quadrado = c((erro^2/qchisq(alfa/2,gl,lower.tail = FALSE)),(erro^2/qchisq(alfa/2,gl)))

#estimativa intervalar da média de Y dado um x

x_escolhido = 50

#calculo usando o erro padrao (verificar se esta correto)
t_value <- qt(0.975, n - 2,lower.tail = TRUE) # Valor crítico da distribuição t-Student para o nível de confiança de 95% e n-2 graus de liberdade
y_new_hat <- beta_0 + beta_1*x_escolhido # Estimativa pontual da média de Y para o valor de X escolhido
ic_lower <- y_new_hat - t_value*erro_padrao # Limite inferior do intervalo de confiança
ic_upper <- y_new_hat + t_value*erro_padrao # Limite superior do intervalo de confiança

cat("O valor estimado Y quando X=", x_escolhido, "é:",y_new_hat)
cat("Intervalo de confiança para a média de Y quando X =", x_escolhido, ": [", ic_lower, ",", ic_upper, "]")

#calculando alternativo (feito em aula)

t_value <- qt(0.975, n - 2,lower.tail = TRUE) # Valor crítico da distribuição t-Student para o nível de confiança de 95% e n-2 graus de liberdade
y_new_hat <- beta_0 + beta_1*x_escolhido # Estimativa pontual da média de Y para o valor de X escolhido
ic_lower <- y_new_hat - t_value*sigma*sqrt((1/n)+((x_escolhido- x_barra)^2)/(x_quadrado - n*x_barra^2)) # Limite inferior do intervalo de confiança
ic_upper <- y_new_hat + t_value*sigma*sqrt((1/n)+((x_escolhido- x_barra)^2)/(x_quadrado - n*x_barra^2)) # Limite superior do intervalo de confiança
#arrumar os parenteses da equacao (acho que ja arrumei)
cat("O valor estimado Y quando X=", x_escolhido, "é:",y_new_hat)
cat("Intervalo de confiança para a média de Y quando X =", x_escolhido, ": [", ic_lower, ",", ic_upper, "]")


#intervalo de confianca para uma futura observação (note a diferença no tamanho do intervalo)
t_value <- qt(0.975, n - 2,lower.tail = TRUE) # Valor crítico da distribuição t-Student para o nível de confiança de 95% e n-2 graus de liberdade
y_new_hat <- beta_0 + beta_1*x_escolhido # Estimativa pontual da média de Y para o valor de X escolhido
ic_lower <- y_new_hat - t_value*sigma*sqrt(1+(1/n)+((x_escolhido- x_barra)^2)/(x_quadrado - n*x_barra^2)) # Limite inferior do intervalo de confiança
ic_upper <- y_new_hat + t_value*sigma*sqrt(1+(1/n)+((x_escolhido- x_barra)^2)/(x_quadrado - n*x_barra^2)) # Limite superior do intervalo de confiança

cat("O valor estimado Y quando X=", x_escolhido, "é:",y_new_hat)
cat("Intervalo de confiança para a média de Y quando X =", x_escolhido, ": [", ic_lower, ",", ic_upper, "]")



