# Guia do Pipeline com {targets}

## 🎯 O que é o {targets}?

O pacote `targets` é uma ferramenta de pipeline que:
- **Rastreia dependências** entre etapas da análise
- **Evita reprocessamento** desnecessário (só roda o que mudou)
- **Documenta** automaticamente o fluxo de trabalho
- **Paraleliza** tarefas quando possível

## 📊 Estrutura do Pipeline

```
Dados Brutos (PNAD)
        ↓
  Processamento
        ↓
    ┌───┴───┬───────────┬──────────┐
    ↓       ↓           ↓          ↓
 Tabelas  Gráfico1  Gráfico2  ... Gráfico6
    ↓       ↓           ↓          ↓
 Salvar  Salvar     Salvar     Salvar
```

## 🚀 Comandos Principais

### 1. Visualizar o Pipeline

```r
# Carregar o targets
library(targets)

# Visualizar estrutura de dependências
tar_visnetwork()

# Ver lista de todos os targets
tar_manifest()
```

### 2. Executar o Pipeline

```r
# Executar tudo
tar_make()

# Executar em paralelo (mais rápido)
tar_make_future(workers = 4)

# Ver o que precisa ser atualizado
tar_outdated()

# Ver progresso
tar_progress()
```

### 3. Trabalhar com Resultados

```r
# Carregar um target específico
tabela <- tar_read(tabela_mei)
grafico <- tar_read(grafico_sexo)

# Carregar múltiplos targets
tar_load(c(tabela_mei, grafico_sexo))

# Ver metadados
tar_meta()
```

## 📝 Targets Disponíveis

### Dados

- `dados_brutos`: Dados originais da PNAD Contínua 2024
- `dados_processados`: Dados filtrados e processados do Ceará
- `dados_um_trabalho`: Dados apenas de pessoas com um trabalho

### Tabelas

- `tabela_numero_trabalhos`: Distribuição por número de trabalhos
- `tabela_contribuintes`: Análise de contribuição previdenciária
- `tabela_mei`: **Tabela principal** com totais e proporções de MEI

### Gráficos

- `grafico_situacao_domicilio`: Por situação do domicílio
- `grafico_tipo_area`: Por tipo de área
- `grafico_sexo`: Por sexo
- `grafico_cor_raca`: Por cor/raça
- `grafico_instrucao`: Por nível de instrução
- `grafico_rendimento`: Por faixa de rendimento

### Outputs

- `salvar_tabela_mei`: Exporta tabela para CSV
- `salvar_graficos`: Exporta todos os gráficos para PNG

## 🔄 Fluxo de Trabalho Típico

### Primeira Execução

```r
# 1. Instalar pacotes
source("instalar_pacotes.R")

# 2. Visualizar pipeline
targets::tar_visnetwork()

# 3. Executar tudo
targets::tar_make()

# 4. Ver resultados
targets::tar_read(tabela_mei)
```

### Atualizações

```r
# Se você modificar uma função em R/funcoes_mei.R:

# 1. Ver o que será reprocessado
targets::tar_outdated()

# 2. Executar apenas o necessário
targets::tar_make()

# 3. Verificar mudanças
targets::tar_progress()
```

### Análise Exploratória

```r
# Carregar dados processados para explorar
dados <- targets::tar_read(dados_processados)

# Fazer análises ad-hoc
library(dplyr)
dados |> 
  group_by(mei, VD3004) |> 
  summarize(n = survey_total())
```

## 🎨 Personalizar Gráficos

Se quiser modificar um gráfico específico:

1. Edite a função `create_socioeconomic_plot()` em `R/funcoes_mei.R`
2. Execute `tar_make()` - apenas os gráficos serão recriados!

Exemplo de modificação:

```r
# Em R/funcoes_mei.R, adicionar tema personalizado:
create_socioeconomic_plot <- function(design, var_name, var_label) {
  # ... código existente ...
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),  # Modificado
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)  # Novo
  )
}
```

## 📦 Adicionar Novos Targets

Para adicionar uma nova análise:

1. Adicione a função em `R/funcoes_mei.R`
2. Adicione o target em `_targets.R`:

```r
# Exemplo: adicionar análise de setores
tar_target(
  name = tabela_setores,
  command = calcular_setores(dados_processados)
)
```

## 🔍 Debugging

Se algo der errado:

```r
# Ver erros
tar_meta(fields = error)

# Limpar tudo e recomeçar
tar_destroy()
tar_make()

# Ver warnings
tar_meta(fields = warnings)
```

## 💾 Cache e Armazenamento

- Os resultados ficam em `_targets/objects/`
- Dados grandes usam formato `qs` (comprimido)
- Gráficos e tabelas pequenas usam `rds`
- Outputs finais vão para `outputs/`

## ⚡ Dicas de Performance

```r
# Usar paralelização
tar_make_future(workers = 4)

# Ver quanto tempo cada target leva
tar_meta(fields = c(name, seconds))

# Invalidar apenas um target
tar_invalidate(grafico_sexo)
```

## 🆘 Problemas Comuns

### "Target is outdated"
Significa que algo mudou. Execute `tar_make()` para atualizar.

### "Could not find function"
Verifique se `source("R/funcoes_mei.R")` está no `_targets.R`.

### Dados muito grandes
Os dados brutos da PNAD são grandes (~500MB). O formato `qs` ajuda a comprimir.

### Gráficos não aparecem
Use `tar_read(grafico_sexo)` ou `print(tar_read(grafico_sexo))`.

## 📚 Recursos

- [Manual do targets](https://books.ropensci.org/targets/)
- [Exemplos](https://github.com/ropensci/targets/tree/main/inst/examples)
- [Cheatsheet](https://github.com/ropensci/targets/blob/main/inst/cheatsheet.pdf)
