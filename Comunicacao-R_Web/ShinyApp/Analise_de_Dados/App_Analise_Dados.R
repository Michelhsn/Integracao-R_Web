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
dbListFields(mydb, 'MONITORAMENTO_TUL')

# Executar Querry
estacao1 = dbSendQuery(mydb, "select * from MONITORAMENTO_TUL")

# Do Mysql ao R
dados = fetch(estacao1, n=-1)

# Dados da tabela
dados<-dbReadTable(mydb, "MONITORAMENTO_TUL")

# Criando dataframe de estudo
df.estacao1<-dbGetQuery(mydb, "SELECT * FROM MONITORAMENTO_TUL ")


library(shiny)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Painel de Análises"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
     # radioButtons("dist", "Distribution type:",
      #             c("Normal" = "norm",
       #              "Uniform" = "unif",
        #             "Log-normal" = "lnorm",
         #            "Exponential" = "exp")),
      radioButtons("analise", "Sala F-12",
                   c("Temperatura" = "temp",
                     "Umidade" = "umid", "Luminosidade" = "lum")),
      
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
                   temp = df.estacao1$TEMPERATURA,
                   umid = df.estacao1$UMIDADE,
                   lum = df.estacao1$LUMINOSIDADE
                  )
    
    
  })
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    analise <- input$analise
    n <- input$n
    
    hist(a(),
         main = analise,
         col = "#75AADB", border = "white")
  })
  
  # Resumo dos dados
  output$summary <- renderPrint({
    
    summary(a())
  })
  
  output$decompose <- renderPlot({
    analise <- input$analise
    n <- input$n
    
    temp.ts<-ts(a(), frequency = 144)
    dec<-decompose(temp.ts)
    plot(dec)
    
  })
  
  
  output$table <- renderTable({
    da<-data.frame(ID=1:(length(a())),Timestamp = df.estacao1$TIMESTAMP, ValorMedido = a())
    #print(da)
})
  
  output$graf <- renderPlot({
    analise <- input$analise
    n <- input$n
    
    temp.ts<-ts(a(), frequency = 144)
    # Plotando
    plot(temp.ts, main = "Dispersão", xlab = "Dias", ylab = "Valores", xlim = c(1,10), type = "p", pch = 20, col = "blue")
    
  })
  
  output$linha <- renderPlot({
    analise <- input$analise
    n <- input$n
    
    temp.ts<-ts(a(), frequency = 144)
    # Plotando
    plot(temp.ts, main = "Linha", xlab = "Dias", ylab = "Valores", type = "l", pch = 20, col = "green")
    
  })
  
    output$boxplot <- renderPlot({
    analise <- input$analise
    n <- input$n
    
    temp.ts<-ts(a(), frequency = 144)
    boxplot(temp.ts~cycle(temp.ts), main = "BoxPlot", xlab = "Dias", ylab = "Valores", xlim = c(1,30))
     })
  
    output$prev24 <- renderPlot({
    analise <- input$analise
    n <- input$n
    
    temp.ts<-ts(a(), frequency = 144)
    
    temp.hw<-HoltWinters(temp.ts)
    temp.pred24<-predict(temp.hw, 144)
    plot(temp.pred24, main = "Previsão")
    
    })
  
    output$prevsum <- renderPrint({
    analise <- input$analise
    n <- input$n
    
    temp.ts<-ts(a(), frequency = 144)
    
    # Plotando
    temp.hw<-HoltWinters(temp.ts)
    temp.pred48<-predict(temp.hw, 144)
    summary(temp.pred48)
    
    })
  
    output$prev2 <- renderPlot({
    analise <- input$analise
    n <- input$n
    
    temp.ts<-ts(a(), frequency = 144)
    
    temp.hw<-HoltWinters(temp.ts)
    temp.pred24<-predict(temp.hw, 144)
    plot(temp.ts, col = "orange", main = "", xlim = c(1, ((length(a()))/144)+3))
    lines(temp.pred24, col = "blue")
    
    
  })
  
}
## Evitar máximo de conexões
dbDisconnect(mydb)
# Create Shiny app ----
shinyApp(ui, server)
