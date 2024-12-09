library(forecast)
library(tidyverse)

set.seed(123)

df <- data.frame(
  data = seq(as.Date("2023-01-01"), as.Date("2024-10-01"), by ="month"),
  custo = c(2.7, 2.8, 0.5, 0.3, 0.3, 2.4, 2.3, 0.7, 0.4, 0.8, 2.5, 2.9, 2.5, 2.4, 0.5, 0.3, 0.3, 0.8, 2.5, 2.4,0.7, 1.3)
)



ggplot(df, aes(x = data, y = custo)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Custo Mensal ao Longo do Tempo",
    x = "Data",
    y = "Custo"
  ) +
  theme_minimal()



ts_data <- ts(df$custo, start = c(2023, 1), frequency = 12)
#Aqui o modelo pega a coluna que contem os custos e define cada dado como de um "periodo" dentro do total de 12 
#Simbolizando os meses do ano

modelo <- auto.arima(ts_data)
#Auto arioma identifica o melhor modelo para os dados

summary(modelo)
#ARIMA é uma sigla para um conjunto de 3 principios de analise sobre séries temporais
# AR -> Auto Regressive ou seja influenciado por valores anteriores
# I -> INtegrated -> Tem uma "tendencia" clara de subida ou queda
# MA -> Moving Average ->  Erros de previsao passados influenciam os valores atuais


#prepPrevisao <- Arima(ts_data, order = c(0,0,1), seasonal = c(0,0,1))
prepPrevisao <- Arima(ts_data, order = c(0,1,0), seasonal = c(0,1,0))
meses_previsao <- 6
previsao <- forecast(prepPrevisao, h = meses_previsao)

# similar ao predict
#h = horizont

previsao

datas_futuras <- seq(max(df$data) + 1, by = "month", length.out = length(previsao$mean))


resultados_df <- data.frame(
  data = datas_futuras,
  custo = as.numeric(previsao$mean),
  tipo = "Previsão"
)



dados_historicos <- df
dados_historicos$tipo <- "Real"

# Combinando dados históricos e previsões
dados_completos <- rbind(
  dados_historicos,
  resultados_df
)


dados_completos$servidor <- "Servidor 1"


cores <- c("Real" = "#2196F3", "Previsão" = "#FF9800")

ggplot(dados_completos, 
       aes(x = data, y = custo, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge", width = 15) +
  scale_fill_manual(values = cores) +
  theme_minimal() +
  labs(title = "Custos AWS - Servidor 1",
       subtitle = "Valores reais e previsões",
       x = "Data",
       y = "Custo Total ($)",
       fill = "Tipo de Dado") +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b %Y",
               expand = expansion(mult = c(0.02, 0.02))) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#Acentuação


# Calculando preço da LAMBDA:
# Roda a cada 10 segundos, ou seja, 360 vezes por hora. Um mes tem aproximadamente 720 horas
total_execucoes = 360 * 720
# Cada csv tem 435 Bytes
tamanho_total_dados = 435 * total_execucoes
# arredondando para mb
tamanho_total_dados <- round(tamanho_total_dados / (1024^2), 2)

# A LAMBDA cobra por uso de RAM por segundo (GB-segundo)

#informações do cloudWatch
memoria_alocada = 0.5 #0.5 GB ou seja 512 mb
duracao_media = 0.2 #segundos

duracao_gb_segundos = total_execucoes * duracao_media * memoria_alocada
duracao_gb_segundos

preco_gb_segundo = 0.0000166667

preco_lambda = duracao_gb_segundos * preco_gb_segundo
preco_lambda

#custos aws:
custo_leitura_s3 = 0.0004 # por 1000
custo_gravacao_s3 = 0.005 # por 1000
custo_leitura_total =  (total_execucoes / 1000)  * custo_leitura_s3
custo_gravacao_total = (total_execucoes / 1000)  * custo_gravacao_s3

custo_leitura_total
custo_gravacao_total

custo_s3 = custo_gravacao_total + custo_leitura_total
custo_s3

custo_total = custo_s3 + preco_lambda
custo_total


