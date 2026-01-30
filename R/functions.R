get_data <- function(file){
  readr::read_csv2(file,
    show_col_types = FALSE
  ) |> 
    janitor::clean_names()
}

secoes <- function(){
  tibble(
    divisao = c(
      "01","02","03",
      "05","06","07","08","09",
      "10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33",
      "35",
      "36","37","38","39",
      "41","42","43",
      "45","46","47",
      "49","50","51","52","53",
      "55","56",
      "58","59","60","61","62","63",
      "64","65","66",
      "68",
      "69","70","71","72","73","74","75",
      "77","78","79","80","81","82",
      "84",
      "85",
      "86","87","88",
      "90","91","92","93",
      "94","95","96",
      "97",
      "99",
      "00"
    ),
    secao = c(
      rep("A", 3),
      rep("B", 5),
      rep("C", 24),
      "D",
      rep("E", 4),
      rep("F", 3),
      rep("G", 3),
      rep("H", 5),
      rep("I", 2),
      rep("J", 6),
      rep("K", 3),
      "L",
      rep("M", 7),
      rep("N", 6),
      "O",
      "P",
      rep("Q", 3),
      rep("R", 4),
      rep("S", 3),
      "T",
      "U",
      "V"
    )
  )
}

associar_secao_cnae <- function(df, coluna_cnae, secoes) {
  df |>
    mutate(
      divisao = stringr::str_sub({{ coluna_cnae }}, 1, 2)
    ) |>
    left_join(secoes, by = "divisao")
}

rotular_secao_cnae <- function(df){
  rotulo_secao <- tibble(
    secao = c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V"),
    nome_secao = c(
      "Agricultura, Pecuaria, Produção Florestal, Pesca e Aquicultura",
      "Industrias Extrativas",
      "Industrias de Transformacao",
      "Eletricidade e Gas",
      "Agua, Esgoto, Gestao de Residuos e Descontaminacao",
      "Construcao",
      "Comercio; Reparacao de Veiculos Automotores e Motocicletas",
      "Transporte, Armazenagem e Correio",
      "Alojamento e Alimentacao",
      "Informacao e Comunicacao",
      "Atividades Financeiras, de Seguros e Serviços Relacionados",
      "Atividades Imobiliarias",
      "Atividades Profissionais, Cientificas e Tecnicas",
      "Atividades Administrativas e Serviços Complementares",
      "Administração Publica, Defesa e Seguridade Social",
      "Educacao",
      "Saude Humana e Serviços Sociais",
      "Artes, Cultura, Esporte e Recreacao",
      "Outras Atividades de Servicos",
      "Servicos Domesticos",
      "Organismos Internacionais e Outras Instituicoes Extraterritoriais",
      "Atividades Mal Definidas"
    )
  )
  
  df |>
    left_join(rotulo_secao, by = "secao")|> 
    group_by(nome_secao) |> 
    summarise(total = sum(total, na.rm = TRUE),
              total_masculino = sum(total_masculino, na.rm = TRUE),
              total_feminino = sum(total_feminino, na.rm = TRUE))
}

write_data <- function(df){
  write_csv(df, "data/mei_secao.csv")
}


