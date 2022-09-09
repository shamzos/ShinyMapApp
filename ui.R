library(leaflet)
library(shiny)

# Choices for drop-downs

deciles = paste("D_0",c(1:9),sep = "")
deciles = c(deciles,"D_10")
navbarPage("Mapping", id="nav",

  tabPanel("Usage",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("mymap", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Cells Option"),
        dateInput("ChoiceD", label=h3("Please Select your month! "),
                  value ="2018-12-01",
                  format = "yyyy-mm",
                  startview = "year",
                  weekstart = 0,
                  language = "fr"),
        
        radioButtons("choiceF", label = h3("Please Select Data Type!"),
                     choices = list("Data"="data","Outgoing" = "Out", "Incoming" = "Inc" )),

        radioButtons("choice", label = h3("Please Select what Deciles!"),
                     choices = list("All Deciles" = "All", "Some Deciles" = "Some" )),
        selectInput(inputId = 'inp',
                    selected = "D_01",
                    'Deciles', 
                    choices = unique(deciles),
                    multiple=TRUE,
                    selectize=TRUE)

        #plotOutput("histCentile", height = 200),
        #plotOutput("scatterCollegeIncome", height = 250)
      ),

      tags$div(id="cite","Distribution usage voice (Outgoing/Incoming), Data (MO) par site par decile."
      )
    )
  ),

  conditionalPanel("false", icon("crosshair"))
)
