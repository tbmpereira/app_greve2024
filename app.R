library(tidyverse)
library(shiny)
library(shinydashboard)
library(wesanderson)

# Calculo da folha salarial mensal atual a partir da tabela do PCCTAE e dados enviados pelo MEC
base_mec <- read.csv("base_mec.csv")
contagens <- base_mec %>%
  group_by(CO_CLASSE, CO_PADRAO, PERCENT_IQ) %>%
  summarize(QTD_SERV = sum(QTD_SERV, na.rm = TRUE))
tabela <- read.csv("./tabela_pcctae2.csv")
contagens <- contagens %>%
  mutate(CO_CLASSE = if_else(CO_CLASSE == 'E-M', 'F', CO_CLASSE),
         nivel_classe = paste(CO_CLASSE, CO_PADRAO, sep = ""))
tabela <- tabela %>%
  rename(vencimento = `Vencimento.Básico`) %>%
  separate_longer_delim(X, delim = ", ") %>%
  select(-X.1)
contagens <- contagens %>%
  left_join(tabela, by = c('nivel_classe' = 'X'))
contagens <- contagens %>%
  mutate(PERCENT_IQ = ifelse(is.na(PERCENT_IQ), 0, PERCENT_IQ),
         custo_total = vencimento * (1 + PERCENT_IQ/100) * QTD_SERV)
custo_folha_atual <- sum(contagens$custo_total)

# Calculo da proposta do Luiz/SINDIFES

# Ano 1

ano1 <- read.csv("sindifes_ano1.csv")
ano1 <- ano1 %>%
  rename(vencimento = `Vencimento.Básico`) %>%
  separate_longer_delim(X, delim = ", ") %>%
  mutate(vencimento = as.numeric(gsub(",", ".", vencimento)))
contagens <- contagens %>%
  left_join(ano1, by = c('nivel_classe' = 'X'))
contagens <- contagens %>%
  mutate(ano1 = vencimento.y * (1 + PERCENT_IQ/100) * QTD_SERV)
custo_ano1 <- sum(contagens$ano1)
orcamento_ano1 <- (custo_ano1 - custo_folha_atual) * 13.33

# Ano 2

ano2 <- read.csv("sindifes_ano2.csv")
ano2 <- ano2 %>%
  rename(vencimento = `Vencimento.Básico`) %>%
  separate_longer_delim(X, delim = ", ") %>%
  mutate(vencimento = as.numeric(gsub(",", ".", vencimento)))
contagens <- contagens %>%
  left_join(ano2, by = c('nivel_classe' = 'X'))
contagens <- contagens %>%
  mutate(ano2 = vencimento * (1 + PERCENT_IQ/100) * QTD_SERV)
custo_ano2 <- sum(contagens$ano2)
orcamento_ano2 <- (custo_ano2 - custo_ano1) * 13.33

# Ano 3

ano3 <- read.csv("sindifes_ano3.csv")
ano3 <- ano3 %>%
  select(1, 2, 4) %>%
  rename(vencimento = `Vencimento.Básico`) %>%
  separate_longer_delim(A, delim = ", ") %>%
  mutate(vencimento = as.numeric(gsub(",", ".", vencimento)))
contagens <- contagens %>%
  left_join(ano3, by = c('nivel_classe' = 'A'))
contagens <- contagens %>%
  mutate(ano3 = vencimento.y.y * (1 + PERCENT_IQ/100) * QTD_SERV)
custo_ano3 <- sum(contagens$ano3)
orcamento_ano3 <- (custo_ano3 - custo_ano2) * 13.33


# Ano 4

ano4 <- read.csv("sindifes_ano4.csv")
ano4 <- ano4 %>%
  select(1, 3, 'X.20') %>%
  rename(vencimento = `Vencimento.Básico`) %>%
  separate_longer_delim(X.20, delim = ", ") %>%
  mutate(vencimento = as.numeric(gsub(",", ".", vencimento)))
contagens <- contagens %>%
  left_join(ano4, by = c('nivel_classe' = 'X.20'))
contagens <- contagens %>%
  mutate(ano4 = vencimento * (1 + PERCENT_IQ/100) * QTD_SERV)
custo_ano4 <- sum(contagens$ano4)
orcamento_ano4 <- (custo_ano4 - custo_ano3) * 13.33

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Proposta Geral", tabName = "proposta", icon = icon("chart-simple")),
    menuItem("Simulação", tabName = "simula", icon = icon("wallet"))
  ))

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "simula",
            fluidRow(
              box(selectInput("classe", label = "Classe",
                              choices = c("A", "B", "C", "D", "E", "F"), 
                              selected = "D"), width = 2),
              box(sliderInput("capac", "Capacitação", min = 1, max = 4, value = 1, step = 1,
                              ticks = TRUE), width = 4),
              box(sliderInput("merit", "Avaliação de Desempenho", min = 1, max = 16, 
                              value = 1, step = 1, ticks = TRUE), width = 4),
              box(actionButton("show_values", "Mostrar Valores"), width = 2)),
            fluidRow(
              box(title = "Reajustes no vencimento básico conforme proposta", 
                  plotOutput("bar_plot"), solidHeader = TRUE, status = "primary",
                  width = 12)
            ),
            fluidRow(
              box(title = "Vencimento Atual", textOutput("vencimento_atual"), 
                  solidHeader = TRUE),
              box(title = "Proposta Ano 1", textOutput("proposta_sindifes_ano1"), 
                  solidHeader = TRUE),
              box(title = "Proposta Ano 2", textOutput("proposta_sindifes_ano2"), 
                  solidHeader = TRUE),
              box(title = "Proposta Ano 3", textOutput("proposta_sindifes_ano3"), 
                  solidHeader = TRUE),
              box(title = "Proposta Ano 4", textOutput("proposta_sindifes_ano4"), 
                  solidHeader = TRUE)
            )
    ),
    tabItem(tabName = "proposta",
            fluidRow(
              box(title = "Resumo da Proposta", "Texto resumindo a proposta",
                  solidHeader = TRUE, width = 12),
              box(title = "Evolução da Proposta",
                  img(src = "slideshow.gif", 
                      style = "display:block; margin:auto; max-width:100%; height:auto;"),
                  solidHeader = TRUE, width = 12)
            )
    )),
  tags$footer(
    style = "text-align: center; margin-top: 20px;",
    "WebApp Desenvolvido por Marcelo Pereira - abril de 2024- Entre em contato: ",
    tags$a(href = "mailto:mapereira@ufmg.br", "mapereira@ufmg.br")
  )
)

# Define a UI for the Shiny app
ui <- dashboardPage(
  dashboardHeader(title = "Simulações proposta greve 2024"),
  sidebar,
  body
)


# Define a server logic para o Shiny app
server <- function(input, output) {
  observeEvent(input$show_values, {
    req(input$classe)
    req(input$capac)
    req(input$merit)
    
    nivel_classe_user <- paste(input$classe, input$capac, 
                               ifelse(input$merit < 10, paste0("0", input$merit), 
                                      input$merit), sep = "")    
    
    # Verifique se há correspondência para a classe selecionada
    if (sum(tabela$X == nivel_classe_user) == 0 ||
        sum(ano1$X == nivel_classe_user) == 0 ||
        sum(ano2$X == nivel_classe_user) == 0 ||
        sum(ano3$A == nivel_classe_user) == 0 ||
        sum(ano4$X.20 == nivel_classe_user) == 0) {
      showNotification("Não há dados disponíveis para a classe selecionada", type = "warning")
      return(NULL)
    }
    
    # Exibir os valores correspondentes
    dados_filtrados <- tibble( datas = factor(c("vencimento atual", "ano 1", "ano 2", 
                                         "ano 3", "ano 4"), levels = c("vencimento atual", 
                                                                       "ano 1", "ano 2", "ano 3", "ano 4")),
                               valores = c(filter(tabela, X == nivel_classe_user)$vencimento,
                                           filter(ano1, X == nivel_classe_user)$vencimento,
                                           filter(ano2, X == nivel_classe_user)$vencimento,
                                           filter(ano3, A == nivel_classe_user)$vencimento,
                                           filter(ano4, X.20 == nivel_classe_user)$vencimento)
    )
    
    # Verifique se há valores na variável 'dados_filtrados$valores'
    if (sum(is.na(dados_filtrados$valores)) > 0) {
      showNotification("Não há dados disponíveis para a classe selecionada", type = "warning")
      return(NULL)
    }
    
    # Gráfico de barras
    output$bar_plot <- renderPlot({
      ggplot(dados_filtrados, aes(x = datas, y = valores, label = paste0("R$", valores))) +
        geom_bar(stat = "identity", fill = wes_palette(n = 5, name = "Darjeeling1")) +
        geom_text(position = position_stack(vjust = 0.5), color = "white",
                  fontface = "bold", size = 6) +
        labs(y = "", x = "", title = "") +
        theme_minimal() +
        theme(
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(size = 14), 
              axis.text.y = element_blank(),
              panel.grid = element_blank())
    })    
    
    # Renderizar os textos correspondentes aos valores
    output$vencimento_atual <- renderText({
      paste("R$", dados_filtrados$valores[1])
    })
    output$proposta_sindifes_ano1 <- renderText({
      paste("R$", dados_filtrados$valores[2])
    })
    output$proposta_sindifes_ano2 <- renderText({
      paste("R$", dados_filtrados$valores[3])
    })
    output$proposta_sindifes_ano3 <- renderText({
      paste("R$", dados_filtrados$valores[4])
    })
    output$proposta_sindifes_ano4 <- renderText({
      paste("R$", dados_filtrados$valores[5])
    })
  })
}

# Roda o aplicativo Shiny
shinyApp(ui = ui, server = server)

