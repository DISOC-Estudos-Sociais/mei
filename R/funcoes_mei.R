#' Funções para Análise de MEI no Ceará - PNAD Contínua
#' 
#' Este arquivo contém funções para coleta, processamento e análise
#' de dados sobre Microempreendedores Individuais (MEI) no estado do Ceará

# Carregar dados da PNAD Contínua ----------------------------------------

#' Baixar dados da PNAD Contínua
#' 
#' @return Objeto de desenho amostral complexo da PNAD
#' @export
get_data_pnadc <- function() {
  PNADcIBGE::get_pnadc(
    year = 2024,
    vars = c(
      "V1022", "V1023", "V2007", "V2010", "V4009", "V4010",
      "V4012", "V4013", "V4019", "V4020", "V4022", "V4032",
      "V403312", "V403412", "V4043", "V4044", "V4046", "V4049",
      "V405012", "V405112", "V5002A", "V5002A2", "V5005A", "V5005A2",
      "VD2002", "VD2006", "VD3004", "VD4002", "VD4007", "VD4010",
      "VD4011", "VD4012", "VD4013", "VD4014", "VD4016", "VD4017",
      "VD4019", "VD4020", "VD5003", "VD5009", "VD4009"
    ),
    interview = 1,
    design = TRUE,
    deflator = TRUE
  )
}


# Processar dados ---------------------------------------------------------

#' Processar dados da PNAD para análise de MEI no Ceará
#' 
#' @param design Objeto de desenho amostral da PNAD
#' @return Objeto survey filtrado e com variáveis processadas
#' @export
process_pnadc <- function(design) {
  design |> 
    srvyr::as_survey(design) |> 
    srvyr::filter(UF == "Ceará") |> 
    srvyr::filter(VD4002 == "Pessoas ocupadas") |>
    srvyr::filter(V4009 == "Um") |> 
    srvyr::mutate(
      VD4016_real = VD4016 * CO1,
      VD4017_real = VD4017 * CO1e,
      VD4019_real = VD4019 * CO1,
      VD4020_real = VD4020 * CO1e
    ) |>
    srvyr::mutate(
      mei = dplyr::case_when(
        VD4009 == "Conta-própria" & V4019 == "Sim" & VD4012 == "Contribuinte" ~ 
          "Conta própria com CNPJ e contribuição social (MEI)",
        VD4009 == "Conta-própria" ~ "Conta própria",
        VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" ~ 
          "Empregado sem carteira",
        .default = "Outros"
      )
    ) |> 
    srvyr::mutate(
      V2010 = dplyr::case_when(
        V2010 == "Branca" ~ "Branca",
        TRUE ~ "Outras"
      )
    )
}

# Tabelas descritivas -----------------------------------------------------

#' Calcular número de trabalhos
#' 
#' @param design Objeto survey processado
#' @return Tabela com totais e proporções por número de trabalhos
#' @export
calc_job_number <- function(design) {
  design |> 
    srvyr::group_by(V4009) |>
    srvyr::summarize(
      total = srvyr::survey_total(), 
      proportion = srvyr::survey_prop()
    )
}


#' Calcular distribuição de contribuintes
#' 
#' @param design Objeto survey processado
#' @return Tabela com totais e proporções de contribuintes
#' @export
calc_contribuinte <- function(design) {
  design |> 
    srvyr::group_by(VD4007, V4019, VD4012) |>
    srvyr::summarize(
      total = srvyr::survey_total(vartype = "ci"), 
      proportion = srvyr::survey_prop()
    )
}


#' Calcular tabela de MEI
#' 
#' @param design Objeto survey processado
#' @return Tabela com totais e proporções de MEI
#' @export
calc_tabela_mei <- function(design) {
  design |> 
    srvyr::group_by(mei) |>
    srvyr::summarize(
      total = srvyr::survey_total(), 
      proportion = srvyr::survey_prop()
    )
}


#' Função genérica para contar variável
#' 
#' @param design Objeto survey processado
#' @param variable Variável a ser contada
#' @return Tabela com totais e proporções
#' @export
coount <- function(design, variable) {
  design |> 
    srvyr::group_by({{ variable }}) |>
    srvyr::summarize(
      total = srvyr::survey_total(), 
      proportion = srvyr::survey_prop()
    )
}


# Gráficos ----------------------------------------------------------------

#' Plotar proporção de MEI ao longo do tempo
#' 
#' @param design Objeto survey processado com variável Ano
#' @return Gráfico ggplot
#' @export
plot_proporcao_mei <- function(design) {
  design |> 
    srvyr::group_by(Ano, mei) |>
    srvyr::summarize(
      total = srvyr::survey_total(), 
      proportion = srvyr::survey_prop()
    ) |> 
    ggplot2::ggplot(ggplot2::aes(Ano, proportion, fill = mei)) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(proportion, accuracy = 0.1)), 
      vjust = 0, 
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3.25,
      color = "white",
      fontface = "bold"
    ) +
    ggplot2::labs(
      title = "Distribuição de MEI ao longo do tempo",
      x = "Ano",
      y = "Proporção",
      fill = "Categoria"
    ) +
    ggplot2::theme_minimal()
}


#' Plotar características socioeconômicas do MEI
#' 
#' @param design Objeto survey processado
#' @param variable Variável socioeconômica a ser analisada
#' @return Gráfico ggplot
#' @export
plt_mei_socioecon <- function(design, variable) {
  design |> 
    srvyr::group_by(mei, {{ variable }}) |>
    srvyr::summarize(
      total = srvyr::survey_total(), 
      proportion = srvyr::survey_prop()
    ) |> 
    ggplot2::ggplot(ggplot2::aes(mei, proportion, fill = {{ variable }})) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_x_discrete(
      labels = function(x) stringr::str_wrap(x, width = 15)
    ) +
    ggplot2::scale_fill_discrete(
      labels = function(x) stringr::str_wrap(x, width = 15)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(proportion, accuracy = 0.1)),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3,
      color = "white",
      fontface = "bold"
    ) +
    ggplot2::labs(
      x = "Categoria de Trabalhador",
      y = "Proporção"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)
    )
}


#' Criar gráfico individual para variável socioeconômica
#' 
#' @param design Objeto survey processado
#' @param var_name Nome da variável (string)
#' @param var_label Rótulo para o gráfico
#' @return Gráfico ggplot
#' @export
create_socioeconomic_plot <- function(design, var_name, var_label) {
  design |> 
    srvyr::group_by(mei, .data[[var_name]]) |>
    srvyr::summarize(
      total = srvyr::survey_total(), 
      proportion = srvyr::survey_prop(),
      .groups = "drop"
    ) |> 
    ggplot2::ggplot(ggplot2::aes(mei, proportion, fill = .data[[var_name]])) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_x_discrete(
      labels = function(x) stringr::str_wrap(x, width = 15)
    ) +
    ggplot2::scale_fill_discrete(
      labels = function(x) stringr::str_wrap(x, width = 15)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(proportion, accuracy = 0.1)),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3,
      color = "white",
      fontface = "bold"
    ) +
    ggplot2::labs(
      title = paste("Distribuição de MEI por", var_label),
      x = "Categoria de Trabalhador",
      y = "Proporção",
      fill = var_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
}
