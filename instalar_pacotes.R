# Instalação de Pacotes para Análise de MEI
#
# Execute este script para instalar todos os pacotes necessários

# Pacotes principais
pacotes <- c(
  # Pipeline
  "targets",
  "tarchetypes",
  
  # Dados
  "PNADcIBGE",
  "srvyr",
  
  # Manipulação
  "dplyr",
  "tidyr",
  "purrr",
  
  # Visualização
  "ggplot2",
  "scales",
  "stringr",
  
  # I/O
  "readr",
  "qs",
  
  # Utilitários
  "here"
)

# Verificar quais pacotes já estão instalados
pacotes_instalados <- installed.packages()[, "Package"]
pacotes_faltantes <- pacotes[!pacotes %in% pacotes_instalados]

# Instalar pacotes faltantes
if (length(pacotes_faltantes) > 0) {
  message("Instalando pacotes faltantes: ", paste(pacotes_faltantes, collapse = ", "))
  install.packages(pacotes_faltantes)
} else {
  message("Todos os pacotes já estão instalados!")
}

# Verificar instalação
todos_instalados <- all(pacotes %in% installed.packages()[, "Package"])

if (todos_instalados) {
  message("\n✓ Todos os pacotes foram instalados com sucesso!")
  message("\nPara iniciar a análise, execute:")
  message("  targets::tar_make()")
} else {
  message("\n✗ Alguns pacotes não foram instalados corretamente.")
  message("Verifique as mensagens de erro acima.")
}
