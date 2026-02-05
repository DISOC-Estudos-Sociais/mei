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

# "V1022" - situacao do domicilio
# "V1023" - tipo de área
# "V2007" - sexo
# "V2010" - cor ou raça
# 
# "V4009" - quantos trabalhos ... tinha na semana de ... a ... (semana de referência)
# "V4010" - código da ocupação (cargo ou função)
# 
# "V4012" - nesse trabalho, ... era:
# "V4013" - código da principal atividade desse negócio/empresa
# "V4019" - esse negócio/empresa era registrado no cadastro nacional da pessoa jurídica - cnpj
# "V4020" - em que tipo de local funcionava esse negócio/empresa
# "V4022" - então onde ... exercia normalmente esse trabalho
# "V4032" - ... era contribuinte de instituto de previdência oficial por esse trabalho
# "V403312" - qual era o rendimento bruto/retirada mensal que ... recebia/fazia normalmente nesse trabalho ? (valor em dinheiro)
# "V403412" - qual foi o rendimento bruto/retirada que ... recebeu/fez nesse trabalho, no mês de referência ? (valor em dinheiro)
# 
# "V4043" - nesse trabalho secundário, ... era
# "V4044" - código da principal atividade desse negócio/empresa
# "V4046" - esse negócio/empresa era registrado no cadastro nacional da pessoa jurídica - cnpj
# "V4049" - era contribuinte de instituto de previdência oficial por esse trabalho secundário?
# "V405012" - valor em dinheiro do rendimento mensal que recebia normalmente nesse trabalho secundário
# "V405112" - valor em dinheiro do rendimento mensal que recebeu nesse trabalho secundário no mês de referência
# 
# "V5002A" -  No mês de ... (mês de referência), ... recebeu rendimentos de Programa Auxílio Brasil (Antigo Bolsa Família)?
# "V5002A2" - valor efetivamente recebido
# "V5005A" -  No mês de ... (mês de referência), ... recebeu rendimentos de seguro-desemprego, seguro-defeso?
# "V5005A2" - valor efetivamente recebido
# 
# "VD2002" - condição no domicílio
# "VD2006" - faixas etárias utilizadas no processo de calibração
# "VD3004" - nível de instrução mais elevado alcançado (pessoas de 5 anos ou mais de idade) padronizado para o ensino fundamental com duração de 9 anos
# 
# "VD4002" - condição de ocupação na semana de referência para pessoas de 14 anos ou mais de idade
# "VD4007" - posição na ocupação no trabalho principal da semana de referência para pessoas de 14 anos ou mais de idade
# "VD4010" - grupamentos de atividade principal do empreendimento do trabalho principal da semana de referência para pessoas de 14 anos ou mais de idade
# "VD4011" - grupamentos ocupacionais do trabalho principal da semana de referência para pessoas de 14 anos ou mais de idade
# "VD4012" - contribuição para instituto de previdência em qualquer trabalho da semana de referência para pessoas de 14 anos ou mais de idade
# "VD4013" - faixa das horas HABITUALMENTE trabalhadas por semana em todos os trabalhos para pessoas de 14 anos ou mais de idade
# "VD4014" - faixa das horas EFETIVAMENTE trabalhadas na semana de referência em todos os trabalhos  para pessoas de 14 anos ou mais de idade
# "VD4016" - rendimento mensal HABITUAL do trabalho PRINCIPAL para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias no trabalho principal)
# "VD4017" - rendimento mensal EFETIVO do trabalho PRINCIPAL para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias no trabalho principal)
# "VD4019" - rendimento mensal HABITUAL de TODOS os trabalhos para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias em qualquer trabalho)
# "VD4020" - rendimento mensal EFETIVO de TODOS os trabalhos para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias em qualquer trabalho)



get_data_pnadc <- function(){
  PNADcIBGE::get_pnadc(year = 2024,
                       vars = c("V1022", "V1023", "V2007", "V2010", "V4009", "V4010",
                                "V4012", "V4013", "V4019", "V4020", "V4022", "V4032",
                                "V403312", "V403412", "V4043", "V4044", "V4046", "V4049",
                                "V405012", "V405112", "V5002A", "V5002A2", "V5005A", "V5005A2",
                                "VD2002", "VD2006", "VD3004", "VD4002", "VD4007", "VD4010",
                                "VD4011", "VD4012", "VD4013", "VD4014", "VD4016", "VD4017",
                                "VD4019", "VD4020"),
                       interview =  1,
                       design = TRUE,
                       deflator = TRUE)
}

process_pnadc <- function(design){
  design |> 
    srvyr::as_survey(design) |> 
    srvyr::filter(UF == "Ceará") |> 
    srvyr::filter(VD4002 == "Pessoas ocupadas") |> 
    srvyr::mutate(VD4016_real = VD4016*CO1,
                  VD4017_real = VD4017*CO1e,
                  VD4019_real = VD4019*CO1,
                  VD4020_real = VD4020*CO1e)
}

# 85.000 (2,3%) de oessoas ocupadas possuem mais de um trabalho. Dessas, somente 3.852 (4,6%) possuem cnpj.

calc_job_number <- function(design){
  design |> 
    srvyr::group_by(V4009) |>
    srvyr::summarize(total = srvyr::survey_total(), proportion = srvyr::survey_prop())
}

filter_one_job <- function(design){
  process_pnadc(design) |>
    srvyr::filter(V4009 == "Um")
}

# dentre os conta própria com CNPJ, os que contribuem para o INSS são 90,8 mil (62,8%) enquanto os que não contribuem são 53,8 mil (37,2%)

calc_contribuinte <- function(design){
  design |> 
    srvyr::group_by(VD4007, V4019, VD4012) |>
    srvyr::summarize(total = srvyr::survey_total(vartype = "ci"), proportion = srvyr::survey_prop())
}

coount <- function(design, variable){
  design |> 
    srvyr::group_by({{variable}}) |>
    srvyr::summarize(total = srvyr::survey_total(), proportion = srvyr::survey_prop())
}

# clean_data_pnadc |>
#   srvyr::mutate(mei = dplyr::case_when(
#     VD4007 == "Conta própria" & V4019 == "Sim"))

# coount(clean_data_pnadc, V4019)
# coount(clean_data_pnadc, VD4007)
# coount(clean_data_pnadc, VD4012)
# 
# 
# 
# 
# coount(clean_data_pnadc, V1022)
# coount(clean_data_pnadc, V1023)
# coount(clean_data_pnadc, V2007)
# coount(clean_data_pnadc, V2010)
