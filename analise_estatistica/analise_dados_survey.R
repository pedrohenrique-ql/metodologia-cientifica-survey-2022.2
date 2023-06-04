# Importação das bibliotecas

if (!require('tidyverse')) install.packages('tidyverse')
library('tidyverse')

if(!require(ggplot2)) install.packages("ggplot2")
library('ggplot2')

# Importação da base de dados das respostas do questionário

dados <- read.csv('/cloud/project/MC/respostas.csv')

# Visualização de um resumo dos dados
summary(dados)       
head(dados)

# Renomeando colunas
colnames(dados) <- c("hora", "termos", "jaTrabalhouTI", "idade", "experiencia", "modeloDeTrabalho", "tempoExperiencia", "influenciaPerformance", "vantagens", "desvantagens", "melhorModeloDeTrabalho")

# Retirando respostas de respondentes que não trabalham em TI
valor_remover <- "Não"
dados <- subset(dados, !apply(dados == valor_remover, 1, any))

dados <- dados %>% na.omit()

# Renomeando "Híbrido (presencial e remoto) para Híbrido
dados$modeloDeTrabalho[dados$modeloDeTrabalho == "Híbrido (presencial e remoto)"] <- "Híbrido"

# Análise Descritiva - Faixa etária
dados$idade[dados$idade == "18 - 25 anos"] <- 21.5
dados$idade[dados$idade == "25 - 30 anos"] <- 27.5
dados$idade[dados$idade == "30 - 40 anos"] <- 35

dados$idade <- as.numeric(dados$idade)

media_idade <- mean(dados$idade)
variancia_idade <- var(dados$idade)
desvio_padrao_idade <- sd(dados$idade)

# Análise Descritiva - Tempo de trabalho no modelo atual predominante
dados$tempoExperiencia[dados$tempoExperiencia == "Há 1 ano ou menos"] <- 0.5
dados$tempoExperiencia[dados$tempoExperiencia == "Entre 1 e 3 anos"] <- 2
dados$tempoExperiencia[dados$tempoExperiencia == "Entre 3 e 5 anos"] <- 4
dados$tempoExperiencia[dados$tempoExperiencia == "Entre 5 e 10 anos"] <- 7.5
dados$tempoExperiencia[dados$tempoExperiencia == "Há mais de 10 anos"] <- 10

dados$tempoExperiencia <- as.numeric(dados$tempoExperiencia)

media_tempo_experiencia <- mean(dados$tempoExperiencia)
variancia_tempo_experiencia <- var(dados$tempoExperiencia)
desvio_padrao_tempo_experiencia <- sd(dados$tempoExperiencia)


#FUNÇÕES
z <- qnorm(0.95 + (1 - 0.95) / 2)

intervalo_confianca <- function(respostas) {
  media <- mean(respostas)
  desvio_padrao <- sd(respostas)
  margem_erro <- z * desvio_padrao / (length(dados$influenciaPerformance) ** 0.5)

  ic <- c(
    round(max(media - margem_erro, 1), digits=2),
    round(min(media + margem_erro, 5), digits=2)
  )
  
  ic
}

tamanho_min_amostra <- function(respostas, erro) {
  desvio_padrao <- sd(respostas)
  tamanho_min <- ceiling((z * desvio_padrao / erro) ** 2)
  tamanho_min
}

# HIPÓTESE 1: O trabalho remoto influencia positivamente na performance.

#Intervalo de confiança
performance_remoto <- dados$influenciaPerformance[dados$modeloDeTrabalho == "Remoto"]
performance_outros <- dados$influenciaPerformance[dados$modeloDeTrabalho != "Remoto"]
performance_presencial <- dados$influenciaPerformance[dados$modeloDeTrabalho == "Presencial"]
performance_hibrido <- dados$influenciaPerformance[dados$modeloDeTrabalho == "Híbrido"]
performance_todos <- dados$influenciaPerformance

media_performance_remoto <- round(mean(performance_remoto), digits=2)
media_performance_presencial <- round(mean(performance_presencial), digits=2)
media_performance_hibrido <- round(mean(performance_hibrido), digits=2)
media_performance_geral <- round(mean(performance_todos), digits=2)

length(performance_remoto)
performance_hibrido
media_performance_remoto
media_performance_presencial
media_performance_hibrido

hipotese1_remoto_tamanho_min_amostra <- tamanho_min_amostra(
  performance_remoto,
  0.25
)

hipotese1_geral_tamanho_min_amostra <- tamanho_min_amostra(
  performance_todos,
  0.25
)

hipotese1_remoto_tamanho_min_amostra
hipotese1_geral_tamanho_min_amostra

hipotese1_remoto_ic <- intervalo_confianca(performance_remoto)
hipotese1_geral_ic <- intervalo_confianca(performance_todos)

hipotese1_remoto_ic
hipotese1_geral_ic

hipotese1_remoto_margem_erro <- round(media_performance_remoto - hipotese1_remoto_ic[1], digits=2)
hipotese1_comparativa_margem_erro <- round(media_performance_geral - hipotese1_geral_ic[1], digits=2)

hipotese1_remoto_margem_erro
hipotese1_comparativa_margem_erro

hipotese1_teste_remoto <- t.test(
  performance_remoto,
  alternative="greater",
  mu=3
)

hipotese1_teste_comparativo <- t.test(
  performance_remoto, 
  performance_outros,
  alternative="greater",
  mu=3
)

hipotese1_teste_remoto
hipotese1_teste_comparativo

p_value_remoto <- hipotese1_teste_remoto$p.value
p_value_comparativo <- hipotese1_teste_comparativo$p.value

if (p_value_remoto < 0.05) {
  hipotese1_teste_remoto <- "A hipótese é suportada. O trabalho remoto influencia positivamente na performance."
} else {
  hipotese1_teste_remoto <- "A hipótese não é suportada. Não há evidências suficientes para concluir que o trabalho remoto influencia positivamente na performance."
}

if (p_value_comparativo < 0.05) {
  hipotese1_teste_comparativo <- "A hipótese é suportada. O trabalho remoto influencia positivamente na performance em relação aos outros modelos."
} else {
  hipotese1_teste_comparativo <- "A hipótese não é suportada. Não há evidências suficientes para concluir que o trabalho remoto influencia positivamente na performance em relação aos outros modelos."
}
hipotese1_teste_remoto
hipotese1_teste_comparativo


# HIPÓTESE 2: O trabalho remoto influencia positivamente na qualidade de vida

# Cálculo das frequências para o teste de hipótese 2

# Características relacionadas à qualidade de vida
strings_contagem_vantagens <- c(
  "Maior flexibilidade geográfica",
  "Mais tempo livre",
  "Mais conforto",
  "Maior satisfação",
  "Economia de tempo e dinheiro com transporte e alimentação"
)

strings_contagem_desvantagens <- c(
  "Pontos negativos em relação à qualidade de vida",
  "Dificuldade em separar vida pessoal e profissional",
  "Necessidades maiores de infraestrutura (computador, internet e energia)",
  "Falta de motivação",
  "Esgotamento mental ou físico",
  "Custos adicionais"
)


# Quantidade de características relacionadas à qualidade de vida escolhida por cada respondente
qv_remoto_vantagens <- dados$vantagens[dados$modeloDeTrabalho == "Remoto"]
qv_remoto_desvantagens <- dados$desvantagens[dados$modeloDeTrabalho == "Remoto"]
qv_presencial_vantagens <- dados$vantagens[dados$modeloDeTrabalho == "Presencial"]
qv_presencial_desvantagens <- dados$desvantagens[dados$modeloDeTrabalho == "Presencial"]
qv_hibrido_vantagens <- dados$vantagens[dados$modeloDeTrabalho == "Híbrido"]
qv_hibrido_desvantagens <- dados$desvantagens[dados$modeloDeTrabalho == "Híbrido"]
qv_outros_vantagens <- dados$vantagens[dados$modeloDeTrabalho != "Remoto"]
qv_outros_desvantagens <- dados$desvantagens[dados$modeloDeTrabalho != "Remoto"]


#Cálculo da frequência de vantagens e desvantagens
freq_vantagens_remoto <- rep(0, length(qv_remoto_vantagens)) 
freq_desvantagens_remoto <- rep(0, length(qv_remoto_desvantagens))

for (i in seq_along(qv_remoto_vantagens)) {
  resposta <- qv_remoto_vantagens[i]
  for (palavra in strings_contagem_vantagens) {
    if (grepl(palavra, resposta)) {
      freq_vantagens_remoto[i] <- freq_vantagens_remoto[i] + 1
    }
  }
}

for (i in seq_along(qv_remoto_desvantagens)) {
  resposta <- qv_remoto_desvantagens[i]
  for (palavra in strings_contagem_desvantagens) {
    if (grepl(palavra, resposta)) {
      freq_desvantagens_remoto[i] <- freq_desvantagens_remoto[i] + 1
    }
  }
}


freq_vantagens_presencial <- rep(0, length(qv_presencial_vantagens)) 
freq_desvantagens_presencial <- rep(0, length(qv_presencial_desvantagens))

for (i in seq_along(qv_presencial_vantagens)) {
  resposta <- qv_presencial_vantagens[i]
  for (palavra in strings_contagem_vantagens) {
    if (grepl(palavra, resposta)) {
      freq_vantagens_presencial[i] <- freq_vantagens_presencial[i] + 1
    }
  }
}

for (i in seq_along(qv_presencial_desvantagens)) {
  resposta <- qv_remoto_desvantagens[i]
  for (palavra in strings_contagem_desvantagens) {
    if (grepl(palavra, resposta)) {
      freq_desvantagens_presencial[i] <- freq_desvantagens_presencial[i] + 1
    }
  }
}

freq_vantagens_hibrido <- rep(0, length(qv_hibrido_vantagens)) 
freq_desvantagens_hibrido <- rep(0, length(qv_hibrido_desvantagens))

for (i in seq_along(qv_hibrido_vantagens)) {
  resposta <- qv_hibrido_vantagens[i]
  for (palavra in strings_contagem_vantagens) {
    if (grepl(palavra, resposta)) {
      freq_vantagens_hibrido[i] <- freq_vantagens_hibrido[i] + 1
    }
  }
}

for (i in seq_along(qv_hibrido_desvantagens)) {
  resposta <- qv_hibrido_desvantagens[i]
  for (palavra in strings_contagem_desvantagens) {
    if (grepl(palavra, resposta)) {
      freq_desvantagens_hibrido[i] <- freq_desvantagens_hibrido[i] + 1
    }
  }
}

freq_vantagens_outros <- rep(0, length(qv_outros_vantagens)) 
freq_desvantagens_outros <- rep(0, length(qv_outros_desvantagens))

for (i in seq_along(qv_outros_vantagens)) {
  resposta <- qv_outros_vantagens[i]
  for (palavra in strings_contagem_vantagens) {
    if (grepl(palavra, resposta)) {
      freq_vantagens_outros[i] <- freq_vantagens_outros[i] + 1
    }
  }
}

for (i in seq_along(qv_outros_desvantagens)) {
  resposta <- qv_outros_desvantagens[i]
  for (palavra in strings_contagem_desvantagens) {
    if (grepl(palavra, resposta)) {
      freq_desvantagens_outros[i] <- freq_desvantagens_outros[i] + 1
    }
  }
}

#Cálculo da diferença de frequência entre vantagens e desvantagens
diff_freq_remoto <- freq_vantagens_remoto - freq_desvantagens_remoto
diff_freq_presencial <- freq_vantagens_presencial - freq_desvantagens_presencial
diff_freq_hibrido <- freq_vantagens_hibrido - freq_desvantagens_hibrido
diff_freq_outros <- freq_vantagens_outros - freq_desvantagens_outros

diff_vantagens_desvantagens <- c(diff_freq_remoto, diff_freq_outros)
diff_freq_todos <- c(diff_freq_hibrido, diff_freq_presencial, diff_freq_remoto);

hipotese2_media_remoto <- round(mean(diff_freq_remoto), digits=2)
hipotese2_media_geral <- round(mean(diff_freq_todos), digits=2)

hipotese2_media_remoto
hipotese2_media_geral

hipotese2_tamanho_min_amostra_remoto <- tamanho_min_amostra(
  diff_freq_remoto,
  0.475
)

hipotese2_tamanho_min_amostra_geral <- tamanho_min_amostra(
  diff_freq_todos,
  0.45
)

hipotese2_tamanho_min_amostra_remoto
hipotese2_tamanho_min_amostra_geral

hipotese2_ic_remoto <- intervalo_confianca(diff_freq_remoto)
hipotese2_ic_geral <- intervalo_confianca(diff_freq_todos)

hipotese2_ic_remoto
hipotese2_ic_geral

hipotese2_margem_erro_remoto <- round(hipotese2_media_remoto - hipotese2_ic_remoto[1], digits=2)
hipotese2_margem_erro_geral <- round(hipotese2_media_geral - hipotese2_ic_geral[1], digits=2)

hipotese2_margem_erro_remoto
hipotese2_margem_erro_geral

#Teste de hipótese em relação à diferença de frequência relacionada apenas ao remoto
hipotese2_teste_remoto <- t.test(diff_freq_remoto, alternative="greater")
hipotese2_teste_comparativo <- t.test(diff_freq_remoto, diff_freq_outros , alternative="greater")

hipotese2_teste_remoto
hipotese2_teste_comparativo

p_value <- hipotese2_teste$p.value

# Verifica o resultado do teste
if (p_value < 0.05) {
  hipotese2_resultado <- "A hipótese é suportada. O trabalho remoto influencia positivamente na qualidade de vida."
} else {
  hipotese2_resultado <- "A hipótese não é suportada. Não há evidências suficientes para concluir que o trabalho remoto influencia positivamente na qualidade de vida."
}

# Teste de hipótese em relação à diferença de frequência entre o remoto e os outros
hipotese2_teste_comparativo <- t.test(diff_freq_remoto, diff_freq_outros, alternative="greater")

p_value_comparativo <- hipotese2_teste_comparativo$p.value

# Verifica o resultado do teste
if (p_value_comparativo < 0.05) {
  hipotese2_comparativo_resultado <- "A hipótese é suportada. O trabalho remoto influencia positivamente na qualidade de vida em relação aos outros modelos."
} else {
  hipotese2_comparativo_resultado <- "A hipótese não é suportada. Não há evidências suficientes para concluir que o trabalho remoto influencia positivamente na qualidade de vida em relação aos outros modelos."
}

#Resultados finais e gráficos

#GRÁFICOS
todos_valores <- 1:5

# Frequências
freq_presencial <- table(factor(performance_presencial, levels = todos_valores))
freq_remoto <- table(factor(performance_remoto, levels = todos_valores))
freq_hibrido <- table(factor(performance_hibrido, levels = todos_valores))

freq_presencial
freq_remoto
freq_hibrido

mean(freq_presencial)
mean(freq_remoto)
mean(freq_hibrido)

# Criação do gráfico
grafico <- barplot(
  rbind(freq_presencial, freq_remoto, freq_hibrido), 
  beside = TRUE, 
  xlab = "Valor", 
  ylab = "Frequência", 
  ylim = c(0, 20), 
  col = c("#0069b4", "#E5232E", "#fbc424"),
)

media_remoto <- mean(diff_freq_remoto);
media_presencial <- mean(diff_freq_presencial);
media_hibrido <- mean(diff_freq_hibrido);

media_remoto
media_presencial
media_hibrido

grafico <- barplot(
  rbind(media_presencial, media_remoto, media_hibrido), 
  beside = TRUE, 
  xlab = "Valor", 
  ylab = "Frequência", 
  ylim = c(-1, 4),
  col = c("#0069b4", "#E5232E", "#fbc424"),
)

#RESULTADOS
hipotese1_media
hipotese1_tamanho_min_amostra
hipotese1_ic
hipotese1_margem_erro
hipotese1_resultado

hipotese2_media
hipotese2_tamanho_min_amostra
hipotese2_ic
hipotese2_margem_erro
hipotese2_resultado
hipotese2_comparativo_resultado

media_idade
variancia_idade
desvio_padrao_idade

media_tempo_experiencia 
variancia_tempo_experiencia 
desvio_padrao_tempo_experiencia
