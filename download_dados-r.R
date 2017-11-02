## Autor: Michel H. S. Nascimento
## Email: mhsn@a.recife.ifpe.edu.br

# Página de Download da aplicação Capibalabs em R

library("RMySQL")

# Permissão Mysql remoto para o IP - mysqld.cnf
# Dar os privilégios REVOKE ALL PRIVILEGES ON *.* FROM 'usuario'@'ip'; GRANT ALL PRIVILEGES ON *.* TO 'usuario'@'ip' REQUIRE NONE WITH GRANT OPTION MAX_QUERIES_PER_HOUR 
# 0 MAX_CONNECTIONS_PER_HOUR 0 MAX_UPDATES_PER_HOUR 0 MAX_USER_CONNECTIONS 0;
mydb = dbConnect(MySQL(), user='root', password='root', dbname='capiba_monitoramento', host='localhost')

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

# UI
ui <- fluidPage(
  
  # título
  titlePanel("Download de Dados"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose dataset ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("Sala F-12", "Campo")),
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tableOutput("table")
      
    )
    
  )
)

# Define server logic to display and download selected file ----
server <- function(input, output) {
  
  # Valores de React
  datasetInput <- reactive({
    switch(input$dataset,
           "Sala F-12" = df.estacao1)
  })
  
  # Tabela da base de dados selecionada
  output$table <- renderTable({
    datasetInput()
  })
  
  # Download csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}

# Shiny App
shinyApp(ui, server)