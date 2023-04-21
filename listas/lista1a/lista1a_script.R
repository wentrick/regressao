#foi tao simples que nem precisou de script mas vou colocar alguma coisa aqui

equation <- function(x) {2*x + 300}
dt = data.frame(x = 1:100)
ggplot(data.frame(x = 1:100), aes(x)) +
  geom_function(fun = equation, colour = "red")