# Análise de Microempreendedores Individuais (MEI) no Ceará

Projeto de análise de dados da PNAD Contínua 2024 para caracterização dos MEI no estado do Ceará.

## 📋 Estrutura do Projeto

```
.
├── _targets.R              # Pipeline principal do projeto
├── R/
│   └── funcoes_mei.R       # Funções de análise
├── outputs/                # Tabelas e gráficos gerados
└── README.md              # Este arquivo
```

## 🎯 Objetivos

1. Coletar dados da PNAD Contínua 2024
2. Processar e classificar trabalhadores por categoria (MEI, conta própria, etc.)
3. Gerar tabelas descritivas sobre MEI no Ceará
4. Criar visualizações da distribuição socioeconômica dos MEI

## 📦 Pacotes Necessários

Instale os pacotes necessários executando:

```r
install.packages(c(
  "targets",
  "tarchetypes",
  "PNADcIBGE",
  "srvyr",
  "dplyr",
  "ggplot2",
  "scales",
  "stringr",
  "readr",
  "qs"
))
```

## 🚀 Como Executar

### Opção 1: Executar todo o pipeline

```r
# Visualizar o pipeline
targets::tar_visnetwork()

# Executar o pipeline completo
targets::tar_make()

# Ver status dos targets
targets::tar_progress()
```

### Opção 2: Executar targets específicos

```r
# Apenas coletar dados
targets::tar_make(dados_brutos)

# Apenas processar dados
targets::tar_make(dados_processados)

# Gerar apenas a tabela de MEI
targets::tar_make(tabela_mei)

# Gerar apenas os gráficos
targets::tar_make(starts_with("grafico_"))
```

### Opção 3: Carregar resultados já processados

```r
# Carregar a tabela de MEI
tabela <- targets::tar_read(tabela_mei)

# Carregar um gráfico específico
grafico <- targets::tar_read(grafico_sexo)

# Ver o gráfico
print(grafico)
```

## 📊 Outputs Gerados

### Tabelas

- `tabela_mei.csv`: Tabela com totais e proporções de MEI vs outras categorias

### Gráficos

- `grafico_situacao_domicilio.png`: Distribuição por situação do domicílio
- `grafico_tipo_area.png`: Distribuição por tipo de área
- `grafico_sexo.png`: Distribuição por sexo
- `grafico_cor_raca.png`: Distribuição por cor/raça
- `grafico_instrucao.png`: Distribuição por nível de instrução
- `grafico_rendimento.png`: Distribuição por faixa de rendimento

## 🔍 Principais Variáveis

### Classificação de MEI

Os trabalhadores são classificados em:

- **MEI**: Conta própria com CNPJ e contribuição social
- **Conta própria**: Conta própria sem CNPJ ou sem contribuição
- **Empregado sem carteira**: Empregado no setor privado sem carteira
- **Outros**: Demais categorias

### Variáveis Socioeconômicas Analisadas

- `V1022`: Situação do domicílio (urbano/rural)
- `V1023`: Tipo de área
- `V2007`: Sexo
- `V2010`: Cor ou raça
- `VD3004`: Nível de instrução
- `VD5009`: Faixa de rendimento domiciliar per capita

## 📈 Principais Funções

### Coleta e Processamento

- `get_data_pnadc()`: Baixa dados da PNAD Contínua 2024
- `process_pnadc()`: Processa dados e cria variável de MEI
- `filter_one_job()`: Filtra apenas pessoas com um trabalho

### Análise

- `calc_tabela_mei()`: Calcula totais e proporções de MEI
- `calc_contribuinte()`: Analisa contribuição previdenciária
- `create_socioeconomic_plot()`: Cria gráficos de distribuição

## 🔄 Atualizar Análise

O pipeline do `targets` só reprocessa o que mudou:

```r
# Modifica uma função em R/funcoes_mei.R
# Executa novamente
targets::tar_make()
# Apenas os targets afetados serão recalculados!
```

## 💡 Dicas

- Use `tar_visnetwork()` para visualizar as dependências
- Use `tar_outdated()` para ver o que precisa ser atualizado
- Use `tar_manifest()` para ver todos os targets disponíveis
- Os dados brutos são armazenados em formato `qs` para economia de espaço

## 📝 Notas

- A análise considera apenas pessoas ocupadas no estado do Ceará
- Valores monetários são deflacionados usando o deflator da PNAD
- A classificação de MEI segue os critérios: conta própria + CNPJ + contribuição social

## 📞 Suporte

Para questões sobre a PNAD Contínua, consulte:
- [Documentação do PNADcIBGE](https://cran.r-project.org/web/packages/PNADcIBGE/)
- [IBGE - PNAD Contínua](https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html)
