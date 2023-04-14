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


coeficiente_variacao = sigma/y_barra #desvio padrao divido pela media

## rodando o modelo dentro do R para verificar os calculos

lm(dados$taxa ~ dados$massa) %>% summary()

## estimando a variancia do beta 0 e beta 1

s2_beta_1 = sigma_quadrado/(x_quadrado - n* x_barra^2)

s_beta_1 = sqrt(s2_beta_1)

coeficiente_variacao_beta_1 = s_beta_1/beta_1


s2_beta_0 =  sigma_quadrado*(x_quadrado/(n*(x_quadrado - n*x_barra**2)))

s_beta_0 = sqrt(s2_beta_0)

coeficiente_variacao_beta_0 = s_beta_0/beta_0 #variancia maior pois incorpara a variancia de beta 1 (no caso ele repsresenta someente o intercepto ent nao faz mt diferenca)









