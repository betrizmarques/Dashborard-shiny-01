library(shiny)
library(bs4Dash)
library(dplyr)
library(readxl)
library(plotly)
library(leaflet)
library(DT)
require(scales)
require(tidyr)
library(hrbrthemes)

dados <- read_xlsx("dados_limpos.xlsx")  
dados_brasil <- read_xlsx("dados__brasil_limpos.xlsx")


dados <- dados %>%
  filter(Estado!= "Brasil")
  
dados_brasil <- dados_brasil %>% 
  filter(Estado != "Brasil")

dados_sc <- dados %>% 
  filter(Estado =="SC")

dados_pr <- dados %>% 
  filter(Estado =="PR")

dados_rs <- dados %>% 
  filter(Estado =="RS")

  
media_taxa_mortalidade_brasil <- mean(dados_brasil$`Taxa bruta de mortalidade`, na.rm = T)
media_taxa_mortalidade_infantil <- mean(dados_brasil$`Taxa de mortalidade infantil`, na.rm = T)
n_municipios <- length(unique(dados_brasil$Territorialidades))

media_taxa_mortalidade_sc <- mean(dados_sc$`Taxa bruta de mortalidade`, na.rm = T)
media_taxa_mortalidade_infantil_sc<- mean(dados_sc$`Taxa de mortalidade infantil`, na.rm = T)
n_municipios_sc <- length(unique(dados_sc$Territorialidades))

media_taxa_mortalidade_pr <- mean(dados_pr$`Taxa bruta de mortalidade`, na.rm = T)
media_taxa_mortalidade_infantil_pr <- mean(dados_pr$`Taxa de mortalidade infantil`, na.rm = T)
n_municipios_pr <- length(unique(dados_pr$Territorialidades))

media_taxa_mortalidade_rs <- mean(dados_rs$`Taxa bruta de mortalidade`, na.rm = T)
media_taxa_mortalidade_infantil_rs <- mean(dados_rs$`Taxa de mortalidade infantil`, na.rm = T)
n_municipios_rs <- length(unique(dados_rs$Territorialidades))


ui <- dashboardPage(
  help = NULL, 
  fullscreen = TRUE,
  
  title = "Análise de Saúde Pública",
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Análise Saúde Pública",
      image = "http://web.leg.ufpr.br/img/logo-leg-circle.png"
    ), 
    rightUi = dropdownMenu(
      badgeStatus = "info", 
      type = "notifications",
      notificationItem(
        text = "Success",
        status = "success",
        icon = icon("circle-check"),
      ),
      notificationItem(
        text = "Warning",
        status = "warning",
        icon = icon("circle-exclamation")
      ),
      notificationItem(
        text = "Error",
        status = "danger",
        icon = icon("circle-xmark")
      )
    )
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem(
        "Início",
        tabName = "inicio",
        icon = icon("home")
      ),
      menuItem(
        "Brasil", 
        tabName = "brasil", 
        icon = icon("globe")
      ),
      menuItem(
        "Santa Catarina", 
        tabName = "santacatarina", 
        icon = icon("bar-chart")
      ),
      menuItem(
        "Paraná", 
        tabName = "parana", 
        icon = icon("bar-chart")
      ),
      menuItem(
        "Rio Grande do Sul", 
        tabName = "riogrande", 
        icon = icon("bar-chart")
      )
    )
  ),
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(),
  body = dashboardBody(
    tags$head(
        tags$style(HTML("
        .nav-pills .nav-link.active,
        .nav-pills .show>.nav-link {
          background-color: #007bff !important;
          color: white !important;
        }
        
        .nav-sidebar .nav-item>.nav-link.active {
          background-color: #3c9c74 !important;
          color: white !important;
        }
      "))
      ),
      
    tabItems(
      tabItem(
        tabName = "inicio",
        
        jumbotron(
          title = "Análise de Saúde Pública",
          status = "info",
          lead = "Visualização e análise de fatores de saúde pública em municípios do sul do Brasil em 2017",
          btnName = "Download",
          href = "http://www.atlasbrasil.org.br/consulta/planilha",
          "Você pode fazer o download da base de dados, acessando o link abaixo:"
          ), 
        
        fluidRow(
          
          userBox(
            collapsible = FALSE,
            title = userDescription(
              title = "Ana Beatriz Marques",
              subtitle = "Acadêmica de Estatística e Ciência de Dados",
              image = "https://lh3.googleusercontent.com/a/ACg8ocJXupLgbeTL-AOpOW-PoVXclKJ-2yd5REwxF5o5mGIYBNq9zjB_=s360-c-no",
              type = 1
            ), 
            status = "olive",
            "VISUALIZAÇÃO DE DADOS APLICADA"
          ),
          box(title = 'Introdução',
              width = 6,
              collapsible = FALSE,
              blockQuote(
                "A análise e visualização de dados apresentada utiliza informações de saúde pública referentes aos estados do Sul do Brasil (Paraná, Santa Catarina e Rio Grande do Sul) no ano de 2017. O objetivo principal é explorar os padrões regionais e identificar possíveis disparidades ou tendências nos indicadores de saúde pública.",
                color = "olive"
                
              )
          )
        )
        
      ),
    
    
      tabItem(
        tabName = "brasil",
        
        fluidRow(
          
          column(
          width = 4,
          infoBox(
            
            width = 12, 
            title = "Taxa média de mortalidade",
            value = round(media_taxa_mortalidade_brasil, digits = 2),
            icon = icon("skull"),
            color = "gray-dark"
          )
        ),
        
        
        column(
          width = 4,
          infoBox(
            
            width = 12, 
            title = "Taxa média de mortalidade Infantil",
            value = round(media_taxa_mortalidade_infantil, digits = 2),
            icon = icon("child"),
            color = "gray-dark"
          )
        ),
        
        column(
          width = 4,
          infoBox(
            
            width = 12, 
            title = "Total de Municípios",
            value = n_municipios,
            icon = icon("location-dot"),
            color = "gray-dark"
        
        )
      )
     ),
     
     fluidRow(
       sortable(
         width = 6,
         
         box(
           width = 12,
           title = "Scatterplot",
           plotlyOutput("g4_br")
         ),
         
         box(
           width = 12,
           title = "Boxplot",
           plotlyOutput("boxplot")
         )
         
       ),
       
       sortable(
         width = 6,
         
         box(
           width = 12,
           title = "Boxplot",
           
           plotlyOutput("bar_plot_brasil")
         ),
         
         box(
           width = 12,
           title = "Histograma",
           
           plotlyOutput("histograma_brasil")
         )
       )
       
       
       
       
     )
     
), 
tabItem(
  tabName = "santacatarina",
  
  fluidRow(
    
    column(
      width = 4,
      infoBox(
        
        width = 12, 
        title = "Taxa média de mortalidade",
        value = round(media_taxa_mortalidade_sc, digits = 2),
        icon = icon("skull"),
        color = "gray-dark"
      )
    ),
    
    
    column(
      width = 4,
      infoBox(
        
        width = 12, 
        title = "Taxa média de mortalidade Infantil",
        value = round(media_taxa_mortalidade_infantil_sc, digits = 2),
        icon = icon("child"),
        color = "gray-dark"
      )
    ),
    
    column(
      width = 4,
      infoBox(
        
        width = 12, 
        title = "Total de Municípios",
        value = n_municipios_sc,
        icon = icon("location-dot"),
        color = "gray-dark"
        
      )
    )
  ), 
  
  fluidRow(
    sortable(
      width = 6,
      
      box(
        width = 12,
        title = "Scatterplot",
        
        plotlyOutput("g4_sc")
      ),
      
      box(
        width = 12,
        title = "Histograma",
        
        plotlyOutput("histograma_sc")
      )
      
    ),
    
    sortable(
      width = 6,
      
      box(
        width = 12,
        title = "Boxplot",
        
        plotlyOutput("boxplot_sc")
      
      ),
      
      box(
        width = 12,
        title = "Scatterplot",
        
        plotlyOutput("infantil_sc")
      )
    )
    
    
    
    
  )
),

tabItem(
  tabName = "parana",
  
  fluidRow(
    
    column(
      width = 4,
      infoBox(
        
        width = 12, 
        title = "Taxa média de mortalidade",
        value = round(media_taxa_mortalidade_pr, digits = 2),
        icon = icon("skull"),
        color = "gray-dark"
      )
    ),
    
    
    column(
      width = 4,
      infoBox(
        
        width = 12, 
        title = "Taxa média de mortalidade Infantil",
        value = round(media_taxa_mortalidade_infantil_pr, digits = 2),
        icon = icon("child"),
        color = "gray-dark"
      )
    ),
    
    column(
      width = 4,
      infoBox(
        
        width = 12, 
        title = "Total de Municípios",
        value = n_municipios_pr,
        icon = icon("location-dot"),
        color = "gray-dark"
        
      )
    )
  ), 
  fluidRow(
    sortable(
      width = 6,
      
      box(
        width = 12,
        title = "Scatterplot",
        
        plotlyOutput("g4_pr")
      ),
      
      box(
        width = 12,
        title = "Histograma",
        plotlyOutput("histograma_pr")
      )
      
    ),
    
    sortable(
      width = 6,
      
      box(
        width = 12,
        title = "Boxplot", 
        plotlyOutput("boxplot_pr")
      ),
      
      box(
        width = 12,
        title = "Scatterplot",
        
        plotlyOutput("infantil_pr")
      )
    )
    
    
    
    
  )
),
tabItem(
  tabName = "riogrande",
  
  fluidRow(
    
    column(
      width = 4,
      infoBox(
        
        width = 12, 
        title = "Taxa média de mortalidade",
        value = round(media_taxa_mortalidade_rs, digits = 2),
        icon = icon("skull"),
        color = "gray-dark"
      )
    ),
    
    
    column(
      width = 4,
      infoBox(
        
        width = 12, 
        title = "Taxa média de mortalidade Infantil",
        value = round(media_taxa_mortalidade_infantil_rs, digits = 2),
        icon = icon("child"),
        color = "gray-dark"
      )
    ),
    
    column(
      width = 4,
      infoBox(
        
        width = 12, 
        title = "Total de Municípios",
        value = n_municipios_rs,
        icon = icon("location-dot"),
        color = "gray-dark"
        
      )
    )
  ),
  
  fluidRow(
    sortable(
      width = 6,
      
      box(
        width = 12,
        title = "Scatterplot",
        
        plotlyOutput("g4_rs")
      ),
      
      box(
        width = 12,
        title = "Histograma",
        
        plotlyOutput("histograma_rs")
      )
      
    ),
    
    sortable(
      width = 6,
      
      box(
        width = 12,
        title = "Boxplot",
        
        plotlyOutput("boxplot_rs")
      ),
      
      box(
        width = 12,
        title = "Scatterplot",
        
        plotlyOutput("infantil_rs")
       
      )
    )
    
    
    
    
  )
)
)
)
)

server <- function(input, output) {
  
  output$g4_pr <- renderPlotly({
    
    plot_ly(
      data = dados_pr,
      x = ~ `% de pessoas cobertas por planos de saúde suplementar`,
      y = ~`% de internações por condições sensíveis à atenção primária` ,
      text = ~Territorialidades, 
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 5, color = '#3c9c74'),
      hoverinfo = 'text' 
    ) %>% 
      layout(
        xaxis = list(
          title = list(
            text = "% de Pessoas cobertas por planos de saúde suplementar",
            font = list(size = 12)  
          )
        ),
        yaxis = list(
          title = list(
            text = " % de Internações por condições sensíveis à atenção primária",
            font = list(size = 12)  
          )
        )
      )
    
  })
  
  output$g4_sc <- renderPlotly({
    
    plot_ly(
      data = dados_sc,
      x = ~ `% de pessoas cobertas por planos de saúde suplementar`,
      y = ~`% de internações por condições sensíveis à atenção primária` ,
      text = ~Territorialidades, 
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 5, color = '#3c9c74'),
      hoverinfo = 'text' 
    ) %>% 
      layout(
        xaxis = list(
          title = list(
            text = "% de Pessoas cobertas por planos de saúde suplementar",
            font = list(size = 12)  
          )
        ),
        yaxis = list(
          title = list(
            text = " % de Internações por condições sensíveis à atenção primária",
            font = list(size = 12)  
          )
        )
      )
    
    
  })
  
  output$g4_rs <- renderPlotly({
    
    plot_ly(
      data = dados_rs,
      x = ~ `% de pessoas cobertas por planos de saúde suplementar`,
      y = ~`% de internações por condições sensíveis à atenção primária` ,
      text = ~Territorialidades, 
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 5, color = '#3c9c74'),
      hoverinfo = 'text' 
    ) %>% 
      layout(
        xaxis = list(
          title = list(
            text = "% de Pessoas cobertas por planos de saúde suplementar",
            font = list(size = 12)  
          )
        ),
        yaxis = list(
          title = list(
            text = " % de Internações por condições sensíveis à atenção primária",
            font = list(size = 12)  
          )
        )
      )
    
  })
  
  
  
  output$g4_br <- renderPlotly({
    
    plot_ly(
      data = dados_brasil,
      x = ~ `% de pessoas cobertas por planos de saúde suplementar`,
      y = ~`% de internações por condições sensíveis à atenção primária` ,
      text = ~Territorialidades, 
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 5, color = '#3c9c74'),
      hoverinfo = 'text' 
    ) %>% 
      layout(
        xaxis = list(
          title = list(
            text = "% de Pessoas cobertas por planos de saúde suplementar",
            font = list(size = 12)  
          )
        ),
        yaxis = list(
          title = list(
            text = " % de Internações por condições sensíveis à atenção primária",
            font = list(size = 12)  
          )
        )
      )
    
  })
  
  output$histograma_pr <- renderPlotly({
    plot_ly(
      data = dados_pr, 
      x = ~`% de internações por doenças relacionadas ao saneamento ambiental inadequado`, 
      type = 'histogram', 
      marker = list(color = '#3c9c74', line = list(color = '#3c9c74', width = 1))
    ) %>%
      layout(
        title = NULL,
        xaxis = list(
          title = "% de internações por doenças relacionadas ao saneamento ambiental inadequado",
          titlefont = list(size = 12)  
        ),
        yaxis = list(
          title = "Frequência",
          titlefont = list(size = 12)  
        ),
        bargap = 0.2 
      )
  })
  
  output$histograma_sc <- renderPlotly({
    plot_ly(
      data = dados_sc, 
      x = ~`% de internações por doenças relacionadas ao saneamento ambiental inadequado`, 
      type = 'histogram', 
      marker = list(color = '#3c9c74', line = list(color = '#3c9c74', width = 1))
    ) %>%
      layout(
        title = NULL,
        xaxis = list(
          title = "% de internações por doenças relacionadas ao saneamento ambiental inadequado",
          titlefont = list(size = 12) 
        ),
        yaxis = list(
          title = "Frequência",
          titlefont = list(size = 12) 
        ),
        bargap = 0.2 
      )
  })
  
  
  
  output$bar_plot_brasil <- renderPlotly({
    p <- ggplot(dados_brasil) +
      aes(x = Estado, y = `Taxa bruta de mortalidade`, fill = Estado) +
      geom_boxplot(alpha = 0.6) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Distribuição da Taxa Bruta de Mortalidade por Estado")
    
    ggplotly(p, tooltip = "Estado")
    
  })
  
  

  output$histograma_rs <- renderPlotly({
    plot_ly(
      data = dados_rs, 
      x = ~`% de internações por doenças relacionadas ao saneamento ambiental inadequado`, 
      type = 'histogram', 
      marker = list(color = '#3c9c74', line = list(color = '#3c9c74', width = 1))
    ) %>%
      layout(
        title = NULL,
        xaxis = list(
          title = "% de internações por doenças relacionadas ao saneamento ambiental inadequado",
          titlefont = list(size = 12)  
        ),
        yaxis = list(
          title = "Frequência",
          titlefont = list(size = 12) 
        ),
        bargap = 0.2 
      )
  })
  
  output$histograma_brasil <- renderPlotly({
    plot_ly(
      data = dados_brasil, 
      x = ~`% de internações por doenças relacionadas ao saneamento ambiental inadequado`, 
      type = 'histogram', 
      marker = list(color = '#3c9c74', line = list(color = '#3c9c74', width = 1))
    ) %>%
      layout(
        title = NULL,
        xaxis = list(
          title = "% de internações por doenças relacionadas ao saneamento ambiental inadequado",
          titlefont = list(size = 12) 
        ),
        yaxis = list(
          title = "Frequência",
          titlefont = list(size = 12) 
        ),
        bargap = 0.2 
      )
  })
  
  
  output$infantil_br <- renderPlotly({
    
    plot_ly(
      data = dados_brasil,
      x = ~ `% de nascidos vivos com pelo menos sete consultas de pré-natal`,
      y = ~ `% de nascidos vivos com baixo peso ao nascer`,
      text = ~Territorialidades,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 5, color = '#3c9c74'),
      hoverinfo = 'text'
    ) %>%
      layout(
        xaxis = list(title = "% de nascidos vivos com pelo menos sete consultas de pré-natal", titlefont = list(size = 12)),
        yaxis = list(title = "% de nascidos vivos com baixo peso ao nascer", titlefont = list(size = 12))
      )
    
  })
  
  output$infantil_pr <- renderPlotly({
    
    plot_ly(
      data = dados_pr,
      x = ~ `% de nascidos vivos com pelo menos sete consultas de pré-natal`,
      y = ~ `% de nascidos vivos com baixo peso ao nascer`,
      text = ~Territorialidades,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 5, color = '#3c9c74'),
      hoverinfo = 'text'
    ) %>%
      layout(
        xaxis = list(title = "% de nascidos vivos com pelo menos sete consultas de pré-natal", titlefont = list(size = 12)),
        yaxis = list(title = "% de nascidos vivos com baixo peso ao nascer", titlefont = list(size = 12))
      )
    
  })
  
  output$infantil_sc <- renderPlotly({
    
    plot_ly(
      data = dados_sc,
      x = ~ `% de nascidos vivos com pelo menos sete consultas de pré-natal`,
      y = ~ `% de nascidos vivos com baixo peso ao nascer`,
      text = ~Territorialidades,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 5, color = '#3c9c74'),
      hoverinfo = 'text'
    ) %>%
      layout(
        xaxis = list(title = "% de nascidos vivos com pelo menos sete consultas de pré-natal", titlefont = list(size = 12)),
        yaxis = list(title = "% de nascidos vivos com baixo peso ao nascer", titlefont = list(size = 12))
      )
    
  })
  
  output$infantil_rs <- renderPlotly({
    
    plot_ly(
      data = dados_rs,
      x = ~ `% de nascidos vivos com pelo menos sete consultas de pré-natal`,
      y = ~ `% de nascidos vivos com baixo peso ao nascer`,
      text = ~Territorialidades,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 5, color = '#3c9c74'),
      hoverinfo = 'text'
    ) %>%
      layout(
        xaxis = list(title = "% de nascidos vivos com pelo menos sete consultas de pré-natal", titlefont = list(size = 12)),
        yaxis = list(title = "% de nascidos vivos com baixo peso ao nascer", titlefont = list(size = 12))
      )
    
  })
  
  
  output$boxplot <- renderPlotly({
    dados_long <- dados_brasil %>%
      gather(key = "variable", value = "value", 
             c( "Taxa de mortalidade de mulheres por câncer da mama", "Taxa de mortalidade por câncer de próstata", "Taxa de mortalidade por acidente de trânsito", "Taxa de mortalidade por suicídio"))  
    1
    
    p <- ggplot(dados_long, aes(x = variable, y = value, fill = variable)) +
      geom_boxplot(alpha = 0.6) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Taxa de mortalidade por outros fatores", x = NULL, y = "Valor")+
      scale_x_discrete(labels = c("Taxa de mortalidade de mulheres por câncer da mama" = "Câncer\nde mama", 
                                  "Taxa de mortalidade por câncer de próstata" = "Câncer de\npróstata", 
                                  "Taxa de mortalidade por suicídio" = "Suicídio", 
                                  "Taxa de mortalidade por acidente de trânsito" = "Acidente\nde trânsito"))+
      scale_fill_manual(values = c(
        "Taxa de mortalidade de mulheres por câncer da mama" = "#3CB371", 
        "Taxa de mortalidade por câncer de próstata" = "#3CB371",         
        "Taxa de mortalidade por acidente de trânsito" = "#3CB371",       
        "Taxa de mortalidade por suicídio" = "#3CB371"                   
      ))
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white", 
          font = list(size = 12)  
        )
      )
    
  })
  
  
  output$boxplot_pr <- renderPlotly({
    dados_long <- dados_pr %>%
      gather(key = "variable", value = "value", 
             c( "Taxa de mortalidade de mulheres por câncer da mama", "Taxa de mortalidade por câncer de próstata", "Taxa de mortalidade por acidente de trânsito", "Taxa de mortalidade por suicídio"))  
    
    p <- ggplot(dados_long, aes(x = variable, y = value, fill = variable)) +
      geom_boxplot(alpha = 0.6) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Taxa de mortalidade por outros fatores", x = NULL, y = "Valor")+
      scale_x_discrete(labels = c("Taxa de mortalidade de mulheres por câncer da mama" = "Câncer\nde mama", 
                                  "Taxa de mortalidade por câncer de próstata" = "Câncer de\npróstata", 
                                  "Taxa de mortalidade por suicídio" = "Suicídio", 
                                  "Taxa de mortalidade por acidente de trânsito" = "Acidente\nde trânsito"))+
      scale_fill_manual(values = c(
        "Taxa de mortalidade de mulheres por câncer da mama" = "#3CB371", 
        "Taxa de mortalidade por câncer de próstata" = "#3CB371",         
        "Taxa de mortalidade por acidente de trânsito" = "#3CB371",       
        "Taxa de mortalidade por suicídio" = "#3CB371"                   
      ))
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white", 
          font = list(size = 12)  
        )
      )
    
  })
  
  output$boxplot_sc <- renderPlotly({
    dados_long <- dados_sc %>%
      gather(key = "variable", value = "value", 
             c( "Taxa de mortalidade de mulheres por câncer da mama", "Taxa de mortalidade por câncer de próstata", "Taxa de mortalidade por acidente de trânsito", "Taxa de mortalidade por suicídio"))  
    
    p <- ggplot(dados_long, aes(x = variable, y = value, fill = variable)) +
      geom_boxplot(alpha = 0.6) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Taxa de mortalidade por outros fatores", x = NULL, y = "Valor")+
      scale_x_discrete(labels = c("Taxa de mortalidade de mulheres por câncer da mama" = "Câncer\nde mama", 
                                  "Taxa de mortalidade por câncer de próstata" = "Câncer de\npróstata", 
                                  "Taxa de mortalidade por suicídio" = "Suicídio", 
                                  "Taxa de mortalidade por acidente de trânsito" = "Acidente\nde trânsito"))+
      scale_fill_manual(values = c(
        "Taxa de mortalidade de mulheres por câncer da mama" = "#3CB371", 
        "Taxa de mortalidade por câncer de próstata" = "#3CB371",         
        "Taxa de mortalidade por acidente de trânsito" = "#3CB371",       
        "Taxa de mortalidade por suicídio" = "#3CB371"                    
      ))
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white", 
          font = list(size = 12)  
        )
      )
    
  })
  output$boxplot_rs <- renderPlotly({
    dados_long <- dados_rs %>%
      gather(key = "variable", value = "value", 
             c( "Taxa de mortalidade de mulheres por câncer da mama", "Taxa de mortalidade por câncer de próstata", "Taxa de mortalidade por acidente de trânsito", "Taxa de mortalidade por suicídio"))  
    
    p <- ggplot(dados_long, aes(x = variable, y = value, fill = variable)) +
      geom_boxplot(alpha = 0.6) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Taxa de mortalidade por outros fatores", x = NULL, y = "Valor")+
      scale_x_discrete(labels = c("Taxa de mortalidade de mulheres por câncer da mama" = "Câncer\nde mama", 
                                  "Taxa de mortalidade por câncer de próstata" = "Câncer de\npróstata", 
                                  "Taxa de mortalidade por suicídio" = "Suicídio", 
                                  "Taxa de mortalidade por acidente de trânsito" = "Acidente\nde trânsito"))+
      scale_fill_manual(values = c(
        "Taxa de mortalidade de mulheres por câncer da mama" = "#3CB371", 
        "Taxa de mortalidade por câncer de próstata" = "#3CB371",         
        "Taxa de mortalidade por acidente de trânsito" = "#3CB371",       
        "Taxa de mortalidade por suicídio" = "#3CB371"                    
      ))
    
    
    ggplotly(p) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",  
          font = list(size = 12)  
        )
      )
    
  })
  
}


  shinyApp(ui, server)
      
  