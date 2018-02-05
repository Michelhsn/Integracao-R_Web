library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(plotly)
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



# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)


function(input, output, session) {

  data=data.frame(x=c(-34.951057, -34.950171), y=c(-8.060030,-8.060684), id=c("Sala_F-12", "Campo"))
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    leaflet() %>% 
      setView(lng=-34.950171 , lat =-8.060684, zoom=17) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data=data, ~x , ~y, layerId=~id, popup=~id)
  })
  
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  # Make a barplot or scatterplot depending of the selected point
  output$plot1=renderPlotly({
    my_place=data_of_click$clickedMarker$id
    if(is.null(my_place)){my_place="Sala_F-12"}
    if(my_place=="Sala_F-12"){

      t <- as.POSIXct(df.estacao1$TIMESTAMP)
      Temperatura <- df.estacao1$TEMPERATURA
      plot_ly(x = ~t, y = ~Temperatura, mode = 'lines')  %>%
      layout(
        title = "Temperatura")
      
      

    }else{
      barplot(rnorm(10), col=rgb(0.1,0.4,0.9,0.3))
    }    
  })

  output$plot2=renderPlotly({
    my_place=data_of_click$clickedMarker$id
    if(is.null(my_place)){my_place="Sala_F-12"}
    if(my_place=="Sala_F-12"){

      t <- as.POSIXct(df.estacao1$TIMESTAMP)
      Umidade <- df.estacao1$UMIDADE
      plot_ly(x = ~t, y = ~Umidade, mode = 'lines') %>%
      layout(
       title = "Umidade")
      

    }else{
      barplot(rnorm(10), col=rgb(0.1,0.4,0.9,0.3))
    }    
  })
  br()

  output$plot3=renderPlotly({
    my_place=data_of_click$clickedMarker$id
    if(is.null(my_place)){my_place="Sala_F-12"}
    if(my_place=="Sala_F-12"){

      t <- as.POSIXct(df.estacao1$TIMESTAMP)
      Luminosidade <- df.estacao1$LUMINOSIDADE
      plot_ly(x = ~t, y = ~Luminosidade, mode = 'lines') %>%
      layout(
        title = "Luminosidade")
      

    }else{
      barplot(rnorm(10), col=rgb(0.1,0.4,0.9,0.3))
    }    
  })
}
