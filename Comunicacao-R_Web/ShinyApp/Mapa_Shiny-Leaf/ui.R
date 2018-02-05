library(leaflet)

    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
     bootstrapPage( absolutePanel(id = "controls", style = "overflow-y:scroll; max-height: 800px", class = "panel panel-default", fixed = FALSE,
        draggable = TRUE, top = 5, left = "auto", right = "auto", bottom = "auto",
        width = "100%", height = "auto",
        
        HTML('<button id = "mostrar" class = "btn btn-default"style="background-color: #2A74DC; border: none; color: white; padding: 10px 25px; text-align: center; text-decoration: none; display: inline-block; font-size: 16px; margin: 4px 2px; cursor: pointer" data-toggle="collapse" data-target="#demo">Mostrar Medições</button>'),
        tags$br(),
         tags$div(id = 'demo',  class="collapse",
          plotlyOutput("plot1", height = 200), 
          plotlyOutput("plot2", height = 200),
          plotlyOutput("plot3", height = 200)
          

        
      ))),

  conditionalPanel("false", icon("crosshair"))
)
