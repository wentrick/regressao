pacman::p_load(tidyverse,ggpubr)


xi = c(1,0,2,0,3,1,0,1,2,0)
yi = c(16,9,17,12,22,13,8,15,19,11)

dados = data.frame(xi,yi)

#### Represente graficamente ----

ggplot(dados,aes(x = xi,y=yi))+
  geom_point()

#### Calculando e testando correlacao ----

cor(xi, yi, method = "pearson") #traz o coeficiente de correlacao

cor.test(xi, yi, method = "pearson") #traz o coeficiente e o p-valor

#### Calculando Beta_0 e Beta_1 ----

dados = dados %>%
  mutate(yi_quadrado = yi**2,
         xi_quadrado = xi**2,
         xiyi = xi*yi)

n = length(xi)

x_barra = mean(dados$xi) #media amostral de x 
y_barra = mean(dados$yi) #media amostra de y
x = sum(dados$xi) #somatorio de xi
y = sum(dados$yi) #somatorio de yi
x_quadrado = sum(dados$xi_quadrado) #somatorio de xi ao quadrado
y_quadrado = sum(dados$yi_quadrado) #somatorio de yi ao quadrado
xy = sum(dados$xiyi) #somatorio de x vezes y

beta_1 = (xy - n*x_barra*y_barra)/(x_quadrado-n*(x_barra)**2) #calculo do estimador de beta 1
beta_0 = y_barra - beta_1*x_barra #calculo do estimador de beta 0

#calculando os valores estimados e os residuos
dados = dados %>%
  mutate(estimado = beta_0+beta_1*xi)

#representacao visual
ggplot(dados)+
  geom_point(aes(x = xi,y=yi))+
  geom_line(aes(x = xi,y=estimado))

#estimacao pontual quando xi=1

estimado_1 = beta_0+beta_1*1


#estimacao pontual quando xi=2

estimado_2 = beta_0+beta_1*2

#aumento estimado

estimado_2 - estimado_1

#representacao visual para verificar se a reta passa pelo pornto (x_barra,y_barra)
ggplot(dados)+
  geom_point(aes(x = xi,y=yi))+
  geom_line(aes(x = xi,y=estimado))+
  geom_point(aes(x = x_barra,y=y_barra),color = "red")

#calculando o residuo para xi=1

dados = dados %>%
  mutate(residuos = yi - estimado)

dados_x1 = dados %>%
  filter(xi == 1)

dados_x1$residuos

#somando os residuos

sum(dados$residuos)


#calculando MSE (variancia dos erros?)
dados = dados %>%
  mutate(residuos_quadrado = residuos^2)

sigma_quadrado_erro = sum(dados$residuos_quadrado)/(n-2)
sigma_quadrado_erro


#calculando a variancia de Beta 0  e Beta 1 

s2_beta_1 = (sigma_quadrado_erro/(x_quadrado - n* x_barra^2))

s_beta_1 = sqrt(s2_beta_1)

coeficiente_variacao_beta_1 = s_beta_1/beta_1


s2_beta_0 =  sigma_quadrado_erro*(x_quadrado/(n*(x_quadrado - n*x_barra**2)))

s_beta_0 = sqrt(s2_beta_0)

coeficiente_variacao_beta_0 = s_beta_0/beta_0 #variancia maior pois incorpara a variancia de beta 1 (no caso ele repsresenta someente o intercepto ent nao faz mt diferenca)



