# Pipeline de Análise de MEI no Ceará usando {targets}
# 
# Este script define o pipeline completo de análise usando o pacote targets
# Para executar: targets::tar_make()
# Para visualizar: targets::tar_visnetwork()

library(targets)
library(tarchetypes)

# Carregar funções customizadas
source("R/funcoes_mei.R")

# Configurações do pipeline
tar_option_set(
  packages = c(
    "PNADcIBGE",
    "srvyr",
    "dplyr",
    "ggplot2",
    "scales",
    "stringr"
  ),
  format = "rds"
)

# Pipeline
list(
  
  # 1. COLETA DE DADOS ------------------------------------------------------
  
  tar_target(
    name = dados_brutos,
    command = get_data_pnadc(),
    format = "qs"  # Formato eficiente para objetos grandes
  ),
  
  
  # 2. PROCESSAMENTO --------------------------------------------------------
  
  tar_target(
    name = dados_processados,
    command = process_pnadc(dados_brutos),
    format = "qs"
  ),
  
  # 3. TABELAS DESCRITIVAS --------------------------------------------------
  
  tar_target(
    name = tabela_numero_trabalhos,
    command = calc_job_number(dados_processados)
  ),
  
  tar_target(
    name = tabela_contribuintes,
    command = calc_contribuinte(dados_processados)
  ),
  
  tar_target(
    name = tabela_mei,
    command = calc_tabela_mei(dados_processados)
  ),
  
  
  # 4. GRÁFICOS SOCIOECONÔMICOS ---------------------------------------------
  
  # Lista de variáveis socioeconômicas para análise
  tar_target(
    name = variaveis_socioeconomicas,
    command = list(
      list(var = "V1022", label = "Situação do Domicílio"),
      list(var = "V1023", label = "Tipo de Área"),
      list(var = "V2007", label = "Sexo"),
      list(var = "V2010", label = "Cor ou Raça"),
      list(var = "VD3004", label = "Nível de Instrução"),
      list(var = "VD5009", label = "Faixa de Rendimento Domiciliar Per Capita")
    )
  ),
  
  # Gráfico: Situação do Domicílio (V1022)
  tar_target(
    name = grafico_situacao_domicilio,
    command = create_socioeconomic_plot(
      dados_processados, 
      "V1022", 
      "Situação do Domicílio"
    )
  ),
  
  # Gráfico: Tipo de Área (V1023)
  tar_target(
    name = grafico_tipo_area,
    command = create_socioeconomic_plot(
      dados_processados, 
      "V1023", 
      "Tipo de Área"
    )
  ),
  
  # Gráfico: Sexo (V2007)
  tar_target(
    name = grafico_sexo,
    command = create_socioeconomic_plot(
      dados_processados, 
      "V2007", 
      "Sexo"
    )
  ),
  
  # Gráfico: Cor ou Raça (V2010)
  tar_target(
    name = grafico_cor_raca,
    command = create_socioeconomic_plot(
      dados_processados, 
      "V2010", 
      "Cor ou Raça"
    )
  ),
  
  # Gráfico: Nível de Instrução (VD3004)
  tar_target(
    name = grafico_instrucao,
    command = create_socioeconomic_plot(
      dados_processados, 
      "VD3004", 
      "Nível de Instrução"
    )
  ),
  
  # Gráfico: Rendimento (VD5009)
  tar_target(
    name = grafico_rendimento,
    command = create_socioeconomic_plot(
      dados_processados, 
      "VD5009", 
      "Faixa de Rendimento Domiciliar Per Capita"
    )
  ),
  
  
  # 5. SALVAR OUTPUTS -------------------------------------------------------
  
  tar_target(
    name = salvar_tabela_mei,
    command = {
      readr::write_csv2(tabela_mei, "outputs/tabela_mei.csv")
      tabela_mei
    }
  ),
  
  tar_target(
    name = salvar_graficos,
    command = {
      ggplot2::ggsave(
        "outputs/grafico_situacao_domicilio.png", 
        grafico_situacao_domicilio,
        width = 10, height = 6, dpi = 300
      )
      ggplot2::ggsave(
        "outputs/grafico_tipo_area.png", 
        grafico_tipo_area,
        width = 10, height = 6, dpi = 300
      )
      ggplot2::ggsave(
        "outputs/grafico_sexo.png", 
        grafico_sexo,
        width = 10, height = 6, dpi = 300
      )
      ggplot2::ggsave(
        "outputs/grafico_cor_raca.png", 
        grafico_cor_raca,
        width = 10, height = 6, dpi = 300
      )
      ggplot2::ggsave(
        "outputs/grafico_instrucao.png", 
        grafico_instrucao,
        width = 10, height = 6, dpi = 300
      )
      ggplot2::ggsave(
        "outputs/grafico_rendimento.png", 
        grafico_rendimento,
        width = 10, height = 6, dpi = 300
      )
      "Gráficos salvos com sucesso!"
    }
  )
)
