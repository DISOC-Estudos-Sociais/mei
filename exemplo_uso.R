# Script de Exemplo - Análise de MEI no Ceará
# 
# Este script demonstra como usar o pipeline sem o {targets}
# Para análises exploratórias rápidas

# Carregar pacotes --------------------------------------------------------

library(PNADcIBGE)
library(srvyr)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

# Carregar funções
source("R/funcoes_mei.R")


# Executar análise passo a passo ------------------------------------------

# Passo 1: Coletar dados
message("Baixando dados da PNAD Contínua 2024...")
dados_brutos <- get_data_pnadc()

# Passo 2: Processar dados
message("Processando dados do Ceará...")
dados_processados <- process_pnadc(dados_brutos)

# Passo 3: Gerar tabela de MEI
message("Gerando tabela de MEI...")
tabela_mei <- calc_tabela_mei(dados_processados)
print(tabela_mei)

# Passo 4: Gerar gráficos
message("Gerando gráficos socioeconômicos...")

# Criar diretório de outputs se não existir
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

# Gráfico 1: Situação do Domicílio
g1 <- create_socioeconomic_plot(
  dados_processados, 
  "V1022", 
  "Situação do Domicílio"
)
ggsave("outputs/grafico_situacao_domicilio.png", g1, 
       width = 10, height = 6, dpi = 300)

# Gráfico 2: Tipo de Área
g2 <- create_socioeconomic_plot(
  dados_processados, 
  "V1023", 
  "Tipo de Área"
)
ggsave("outputs/grafico_tipo_area.png", g2, 
       width = 10, height = 6, dpi = 300)

# Gráfico 3: Sexo
g3 <- create_socioeconomic_plot(
  dados_processados, 
  "V2007", 
  "Sexo"
)
ggsave("outputs/grafico_sexo.png", g3, 
       width = 10, height = 6, dpi = 300)

# Gráfico 4: Cor ou Raça
g4 <- create_socioeconomic_plot(
  dados_processados, 
  "V2010", 
  "Cor ou Raça"
)
ggsave("outputs/grafico_cor_raca.png", g4, 
       width = 10, height = 6, dpi = 300)

# Gráfico 5: Nível de Instrução
g5 <- create_socioeconomic_plot(
  dados_processados, 
  "VD3004", 
  "Nível de Instrução"
)
ggsave("outputs/grafico_instrucao.png", g5, 
       width = 10, height = 6, dpi = 300)

# Gráfico 6: Rendimento
g6 <- create_socioeconomic_plot(
  dados_processados, 
  "VD5009", 
  "Faixa de Rendimento Domiciliar Per Capita"
)
ggsave("outputs/grafico_rendimento.png", g6, 
       width = 10, height = 6, dpi = 300)

message("Análise concluída! Verifique a pasta 'outputs/'")


# Análises adicionais (opcional) ------------------------------------------

# Tabela de número de trabalhos
tabela_trabalhos <- calc_job_number(dados_processados)
print(tabela_trabalhos)

# Tabela de contribuintes
tabela_contrib <- calc_contribuinte(dados_processados)
print(tabela_contrib)

# Salvar tabelas
readr::write_csv(tabela_mei, "outputs/tabela_mei.csv")
readr::write_csv(tabela_trabalhos, "outputs/tabela_trabalhos.csv")
readr::write_csv(tabela_contrib, "outputs/tabela_contribuintes.csv")

message("Tabelas salvas em CSV!")


# Visualizar gráficos individuais -----------------------------------------

# Descomentar para visualizar os gráficos
# print(g1)
# print(g2)
# print(g3)
# print(g4)
# print(g5)
# print(g6)
