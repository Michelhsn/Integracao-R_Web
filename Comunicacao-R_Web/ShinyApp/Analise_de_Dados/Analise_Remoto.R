## Autor: Michel H. S. Nascimento
## Email: mhsn@a.recife.ifpe.edu.br

# Criação de aplicação para análises estatísticas
# de um servidor local baseado em conceitos IoT
library("RMySQL")

# Permissão Mysql remoto para o IP - mysqld.cnf
# Dar os privilégios REVOKE ALL PRIVILEGES ON *.* FROM 'usuario'@'ip'; GRANT ALL PRIVILEGES ON *.* TO 'usuario'@'ip' REQUIRE NONE WITH GRANT OPTION MAX_QUERIES_PER_HOUR 
# 0 MAX_CONNECTIONS_PER_HOUR 0 MAX_UPDATES_PER_HOUR 0 MAX_USER_CONNECTIONS 0;
mydb = dbConnect(MySQL(), user='user', password='password', dbname='dbname', host='localhost')
# Lista as tabelas na base de dados e a desc
dbListTables(mydb)
dbListFields(mydb, 'ESTACAO_001')

# Executar Querry
estacao1 = dbSendQuery(mydb, "select * from ESTACAO_001")

# Do Mysql ao R
dados = fetch(estacao1, n=-1)

# Dados da tabela
dados<-dbReadTable(mydb, "ESTACAO_001")

# Criando dataframe de estudo
df.estacao1<-dbGetQuery(mydb, "SELECT * FROM ESTACAO_001 ")
df.estacao2<-dbGetQuery(mydb, "SELECT * FROM ESTACAO_002 ")

library(shiny)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Painel de Análises"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("dataset", "Escolha a Estação:",
                  choices = c("Sala F-12", "Campo")),
      # Input: Select the random distribution type ----
     # radioButtons("dist", "Distribution type:",
      #             c("Normal" = "norm",
       #              "Uniform" = "unif",
        #             "Log-normal" = "lnorm",
         #            "Exponential" = "exp")),
     conditionalPanel(
       condition = "input.dataset == 'Sala F-12'",
      radioButtons("analise", "Sala F-12",
                   c("Temperatura" = "Temperatura",
                     "Umidade" = "Umidade", "Luminosidade" = "Luminosidade2"))),
     conditionalPanel(
       condition = "input.dataset == 'Campo'",
       radioButtons("analise1", "Campo",
                    c("CO" = "CO",
                      "UV" = "UV", "Luminosidade" = "Luminosidade"))),
      
      # br() element to introduce extra vertical spacing ----
      br()
      
      # Input: Slider for the number of observations to generate ----
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Histograma", plotOutput("plot")),
                  tabPanel("Resumo", verbatimTextOutput("summary"), plotOutput("decompose")),
                  tabPanel("Dados", tableOutput("table")),
                  tabPanel("Gráficos", plotOutput("graf"), plotOutput("linha"), plotOutput("boxplot")),
                  tabPanel("Previsão", verbatimTextOutput("prevsum"), plotOutput("prev24"), plotOutput("prev2"))
                  
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  #d <- reactive({
   # dist <- switch(input$dist,
    #               norm = rnorm,
     #              unif = runif,
      #             lnorm = rlnorm,
       #            exp = rexp,
        #           rnorm)
    
    #dist(input$n)
  #})
  a <- reactive({
    analise <- switch(input$analise,
                   Temperatura = df.estacao1$TEMPERATURA,
                   Umidade = df.estacao1$UMIDADE,
                   Luminosidade2 = df.estacao1$LUMINOSIDADE
                   
                  )
  })
  b <- reactive({
  analise1 <- switch(input$analise1,
                     CO = df.estacao2$CO,
                     UV = df.estacao2$UV,
                     Luminosidade = df.estacao2$LUMINOSIDADE
  )
  })
  dadosreact<-reactiveValues()
  observeEvent(input$analise,{
    dadosreact$click<-input$analise
  })
  dadosreact1<-reactiveValues()
  observeEvent(input$analise1,{
    dadosreact$click<-input$analise1
  })
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    analise1 <- input$analise1
    analise <- input$analise
    n <- input$n
    if (dadosreact$click == analise){
    hist(a(),
         main = analise,
         col = "#75AADB", border = "white")
      }
    if (dadosreact$click == "CO" || dadosreact$click == "UV" || dadosreact$click == "Luminosidade"){
      hist(b(),
           main = analise1,
           col = "#75AADB", border = "white")
    }
  })
  
  # Resumo dos dados
  output$summary <- renderPrint({
    analise1 <- input$analise1
    analise <- input$analise
    n <- input$n
    if (dadosreact$click == "Temperatura" || dadosreact$click == "Umidade" || dadosreact$click == "Luminosidade2"){
      summary(a())
    }
    else{
      summary(b())
    }
  })
  
  output$decompose <- renderPlot({
    analise <- input$analise
    n <- input$n
    
    Temperatura.ts<-ts(a(), frequency = 144)
    dec<-decompose(Temperatura.ts)
    temp1.ts<-ts(b(), frequency = 144)
    dec1<-decompose(temp1.ts)
    
    if (dadosreact$click == "Temperatura" || dadosreact$click == "Umidade" || dadosreact$click == "Luminosidade2"){
      plot(dec)
    }
    else{
      plot(dec1)
    }
  })
  
  
  output$table <- renderTable({
    if (dadosreact$click == "Temperatura" || dadosreact$click == "Umidade" || dadosreact$click == "Luminosidade2"){
      da<-data.frame(ID=1:(length(a())),Timestamp = df.estacao1$TIMESTAMP, ValorMedido = a())
      tail(da, 20)
      }
    else{
      da<-data.frame(ID=1:(length(b())),Timestamp = df.estacao2$TIMESTAMP, ValorMedido = b())
      tail(da, 20)
    }
    
    #print(da)
})
  
  output$graf <- renderPlot({
    analise <- input$analise
    n <- input$n
    
    Temperatura.ts<-ts(a(), frequency = 144)
    temp1.ts<-ts(b(), frequency = 144)
    # Plotando
    if (dadosreact$click == "Temperatura" || dadosreact$click == "Umidade" || dadosreact$click == "Luminosidade2"){
      plot(Temperatura.ts, main = "Dispersão", xlab = "Dias", ylab = "Valores", type = "p", pch = 20, col = "blue")
    }
    else{
      plot(temp1.ts, main = "Dispersão", xlab = "Dias", ylab = "Valores", type = "p", pch = 20, col = "blue")
    }
  })
  
  output$linha <- renderPlot({
    analise <- input$analise
    n <- input$n
    
    Temperatura.ts<-ts(a(), frequency = 144)
    temp1.ts<-ts(b(), frequency = 144)
    # Plotando
    if (dadosreact$click == "Temperatura" || dadosreact$click == "Umidade" || dadosreact$click == "Luminosidade2"){
      plot(Temperatura.ts, main = "Linha", xlab = "Dias", ylab = "Valores", type = "l", pch = 20, col = "green")
    }
    else{
      plot(temp1.ts, main = "Linha", xlab = "Dias", ylab = "Valores", type = "l", pch = 20, col = "green")
    }
  })
  
    output$boxplot <- renderPlot({
    analise <- input$analise
    n <- input$n
    
    Temperatura.ts<-ts(a(), frequency = 144)
    temp1.ts<-ts(b(), frequency = 144)
    
    if (dadosreact$click == "Temperatura" || dadosreact$click == "Umidade" || dadosreact$click == "Luminosidade2"){
      boxplot(Temperatura.ts~cycle(Temperatura.ts), main = "BoxPlot", xlab = "Dias", ylab = "Valores", xlim = c(1,30))
    }
    else{
      boxplot(temp1.ts~cycle(Temperatura.ts), main = "BoxPlot", xlab = "Dias", ylab = "Valores", xlim = c(1,30))
    }
     })
  
    output$prev24 <- renderPlot({
    analise <- input$analise
    n <- input$n
    
    Temperatura.ts<-ts(a(), frequency = 144)
    temp1.ts<-ts(b(), frequency = 144)
    
    Temperatura.hw<-HoltWinters(Temperatura.ts)
    Temperatura.pred24<-predict(Temperatura.hw, 144)
    temp1.hw<-HoltWinters(temp1.ts)
    temp1.pred24<-predict(temp1.hw, 144)
    
    plot(Temperatura.pred24, main = "Previsão")
    
    if (dadosreact$click == "Temperatura" || dadosreact$click == "Umidade" || dadosreact$click == "Luminosidade2"){
      plot(Temperatura.pred24, main = "Previsão")
    }
    else{
      plot(temp1.pred24, main = "Previsão")
    }
    
    })
  
    output$prevsum <- renderPrint({
    analise <- input$analise
    n <- input$n
    
    Temperatura.ts<-ts(a(), frequency = 144)
    temp1.ts<-ts(b(), frequency = 144)
    
    Temperatura.hw<-HoltWinters(Temperatura.ts)
    Temperatura.pred48<-predict(Temperatura.hw, 144)
    temp1.hw<-HoltWinters(temp1.ts)
    temp1.pred48<-predict(temp1.hw, 144)
    
    if (dadosreact$click == "Temperatura" || dadosreact$click == "Umidade" || dadosreact$click == "Luminosidade2"){
      summary(Temperatura.pred48)
    }
    else{
      summary(temp1.pred48)
    }
    
    })
  
    output$prev2 <- renderPlot({
    analise <- input$analise
    n <- input$n
    
    Temperatura.ts<-ts(a(), frequency = 144)
    
    Temperatura.hw<-HoltWinters(Temperatura.ts)
    Temperatura.pred24<-predict(Temperatura.hw, 144)
    Temperatura1.ts<-ts(b(), frequency = 144)
    
    Temperatura1.hw<-HoltWinters(Temperatura1.ts)
    Temperatura1.pred24<-predict(Temperatura1.hw, 144)
    
    if (dadosreact$click == "Temperatura" || dadosreact$click == "Umidade" || dadosreact$click == "Luminosidade2"){
      plot(Temperatura.ts, col = "orange", main = "", xlim = c(1, ((length(a()))/144)+3))
      lines(Temperatura.pred24, col = "blue")    }   
    else{
      plot(Temperatura1.ts, col = "orange", main = "", xlim = c(1, ((length(b()))/144)+3))
      lines(Temperatura1.pred24, col = "blue")    }
    
  })
  
}
## Evitar máximo de conexões
dbDisconnect(mydb)
# Create Shiny app ----
shinyApp(ui, server)

