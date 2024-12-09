
testeDownTime = testeDownTIme
View(testeDownTIme)


library(dplyr)
library(ggplot2)


testeDownTime = subset(testeDownTime, idRegistro<=44)

testeDownTime = testeDownTime %>% mutate(downTime = ifelse(percentualMemoria >= 99 | percentualCPU >= 99, 1,0))

testeDownTime$dataHora = as.POSIXct(testeDownTime$dataHora, format= "%Y-%m-%d %H:%M:%S") 
ponto = min(testeDownTime$dataHora)
testeDownTime$tempo = as.numeric(difftime(testeDownTime$dataHora, ponto, units="mins"))

# Criei um modelo com duas variaveis independentes(x1 e x2)



modelo = lm(tempo ~ testeDownTime$percentualMemoria + testeDownTime$percentualCPU, data = testeDownTime)


modelo2 = lm(tempo ~ testeDownTime$percentualMemoria + testeDownTime$percentualCPU, data = testeDownTime)


summary(modelo)
# Interpretação: O coeficiente do resultado me diz que a estimativa de continuação das leituras iria crashar o servidor

ggplot(testeDownTime, aes(x = tempo, y = downTime)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Previsão de Tempo até Crash",
       x = "Tempo (minutos)",
       y = "Tempo Previsto até Crash")

