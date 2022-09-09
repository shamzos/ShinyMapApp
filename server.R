library(leaflet)
library(shiny)


function(input, output, session){
  
  Outgoing <- reactive({
    y <- paste0(strsplit(as.character(input$ChoiceD),split = "")[[1]][c(3,4)],collapse = "")
    m <- paste0(strsplit(as.character(input$ChoiceD),split = "")[[1]][c(6,7)],collapse = "")
    K <- read.csv(paste("input/Outgoing_",y,"_",m,".txt",sep = ""),
                  sep = "\t",
                  dec = ",",
                  na.strings = "?",
                  stringsAsFactors = FALSE)
    K$Usage <- paste0(format(round(K$Usage),decimal.mark=",", big.mark=" ",  small.interval=3) ,
                             " ",K$Unity)
    K$Usage_2G <- paste0(format(round(K$Usage_2G),decimal.mark=",", big.mark=" ",  small.interval=3),
                                " ",K$Unity," ","(",K$Ratio_Usage_2G,"%",")")
    K$Usage_3G <- paste0(format(round(K$Usage_3G),decimal.mark=",", big.mark=" ",  small.interval=3),
                                " ",K$Unity," ","(",K$Ratio_Usage_3G,"%",")")
    K$Users_2G <- paste0(format(round(K$Users_2G),decimal.mark=",", big.mark=" ",  small.interval=3),
                                " ","(",K$Ratio_Users_2G,"%",")")
    K$Users_3G_4G <- paste0(format(round(K$Users_3G_4G),decimal.mark=",", big.mark=" ",  small.interval=3),
                                   " ","(",K$Ratio_Users_3G_4G,"%",")")
    K$Users <- format(round(K$Users),decimal.mark=",", big.mark=" ",  small.interval=3)
    K
    
    })
  
  
  Incoming <- reactive({
    y <- paste0(strsplit(as.character(input$ChoiceD),split = "")[[1]][c(3,4)],collapse = "")
    m <- paste0(strsplit(as.character(input$ChoiceD),split = "")[[1]][c(6,7)],collapse = "")
    K <- read.csv(paste("input/Incoming_",y,"_",m,".txt",sep = ""),
                                 sep = "\t"
                                 ,dec=",",
                                 na.strings = "?",
                                 stringsAsFactors = FALSE)
    K$Usage <- paste0(format(round(K$Usage),decimal.mark=",", big.mark=" ",  small.interval=3),
                      " ",K$Unity)
    K$Usage_2G <- paste0(format(round(K$Usage_2G),decimal.mark=",", big.mark=" ",  small.interval=3),
                         " ",K$Unity," ","(",K$Ratio_Usage_2G,"%",")")
    K$Usage_3G <- paste0(format(round(K$Usage_3G),decimal.mark=",", big.mark=" ",  small.interval=3),
                         " ",K$Unity," ","(",K$Ratio_Usage_3G,"%",")")
    K$Users_2G <- paste0(format(round(K$Users_2G),decimal.mark=",", big.mark=" ",  small.interval=3),
                         " ","(",K$Ratio_Users_2G,"%",")")
    K$Users_3G_4G <- paste0(format(round(K$Users_3G_4G),decimal.mark=",", big.mark=" ",  small.interval=3),
                            " ","(",K$Ratio_Users_3G_4G,"%",")")
    K$Users <- format(round(K$Users),decimal.mark=",", big.mark=" ",  small.interval=3)
    K
    })
  

  Data <-  reactive({
    
    y <- paste0(strsplit(as.character(input$ChoiceD),split = "")[[1]][c(3,4)],collapse = "")
    m <- paste0(strsplit(as.character(input$ChoiceD),split = "")[[1]][c(6,7)],collapse = "")
    
    K<- read.csv(paste("input/Data_",y,"_",m,".txt",sep = ""),
                    sep = "\t"
                    ,dec=",",
                    na.strings = "?",
                    stringsAsFactors = FALSE)
    K$Usage <- paste0(format(round(K$Usage),decimal.mark=",", big.mark=" ",  small.interval=3),
                         " ",K$Unity)
    K$Usage_2G <- paste0(format(round(K$Usage_2G),decimal.mark=",", big.mark=" ",  small.interval=3),
                            " ",K$Unity," ","(",K$Ratio_Usage_2G,"%",")")
    K$Usage_3G <- paste0(format(round(K$Usage_3G),decimal.mark=",", big.mark=" ",  small.interval=3),
                            " ",K$Unity," ","(",K$Ratio_Usage_3G,"%",")")
    K$Usage_4G <- paste0(format(round(K$Usage_4G),decimal.mark=",", big.mark=" ",  small.interval=3),
                            " ",K$Unity," ","(",K$Ratio_Usage_4G,"%",")")
    K$Users_2G <- paste0(format(round(K$Users_2G),decimal.mark=",", big.mark=" ",  small.interval=3),
                            " ","(",K$Ratio_Users_2G,"%",")")
    K$Users_3G <- paste0(format(round(K$Users_3G),decimal.mark=",", big.mark=" ",  small.interval=3), 
                            " ","(",K$Ratio_Users_3G,"%",")")
    K$Users_4G <- paste0(format(round(K$Users_4G),decimal.mark=",", big.mark=" ",  small.interval=3), 
                            " ","(",K$Ratio_Users_4G,"%",")")
    K$Users <- format(round(K$Users),decimal.mark=",", big.mark=" ",  small.interval=3)
    K
    
    })
  
  
  df <- reactive({
    if(input$choiceF=="Out"){
      g <- Outgoing()
    }else if(input$choiceF=="Inc"){
      g <- Incoming()
    }else{
      g <- Data()
    }
    if(input$choice=="All"){
    g
  }else{
    g[g$Decile %in% input$inp,]
  }
    
  })
  
  
  
  output$mymap <- renderLeaflet({
    data <- df()
    
    
    cellIcon <- iconList(
      D_01 = makeIcon("icons/1.png", 30, 60,iconAnchorX = 20,iconAnchorY = 50),
      D_02 = makeIcon("icons/2.png", 30, 60,iconAnchorX = 20,iconAnchorY = 50),
      D_03 = makeIcon("icons/3.png", 30, 60,iconAnchorX = 20,iconAnchorY = 50),
      D_04 = makeIcon("icons/4.png", 30, 60,iconAnchorX = 20,iconAnchorY = 50),
      D_05 = makeIcon("icons/5.png", 30, 60,iconAnchorX = 20,iconAnchorY = 50),
      D_06 = makeIcon("icons/6.png", 30, 60,iconAnchorX = 20,iconAnchorY = 50),
      D_07 = makeIcon("icons/7.png", 30, 60,iconAnchorX = 20,iconAnchorY = 50),
      D_08 = makeIcon("icons/8.png", 30, 60,iconAnchorX = 20,iconAnchorY = 50),
      D_09 = makeIcon("icons/9.png", 30, 60,iconAnchorX = 20,iconAnchorY = 50),
      D_10 = makeIcon("icons/10.png", 30, 60,iconAnchorX = 20,iconAnchorY = 50)
    )
    if(input$choiceF %in% c("Inc","Out")){
      leaflet(data = data) %>% addTiles() %>%
        addMarkers(~X, ~Y,icon = ~cellIcon[Decile],clusterOptions = markerClusterOptions(),
                   popup = paste("Wilaya  : ", data$Wilaya, "<br>",
                                 "Decile  : ", data$Decile, "<br>",
                                 "Couverture 3G: ",data$Couverture_3G, "<br>",
                                 "Usage   : ",data$Usage, "<br>",
                                 "Usage 2G: ",data$Usage_2G,"<br>",
                                 "Usage 3G: ",data$Usage_3G,"<br>",
                                 "Users   : ",data$Users, "<br>",
                                 "Users 2G: ",data$Users_2G, "<br>",
                                 "Users 3G_4G : ",data$Users_3G_4G),
                   label = ~as.character(Site))
      
    }else{
      leaflet(data = data) %>% addTiles() %>%
        addMarkers(~X, ~Y,icon = ~cellIcon[Decile],clusterOptions = markerClusterOptions(),
                   popup = paste("Wilaya: ", data$Wilaya, "<br>",
                                 "Decile: ", data$Decile, "<br>",
                                 "Couverture 3G: ",data$Couverture_3G, "<br>",
                                 "Couverture 4G: ",data$Couverture_4G, "<br>",
                                 "Usage: ",data$Usage, "<br>",
                                 "Usage 2G: ",data$Usage_2G,"<br>",
                                 "Usage 3G: ",data$Usage_3G,"<br>",
                                 "Usage 4G: ",data$Usage_4G,"<br>",
                                 "Users: ",data$Users, "<br>",
                                 "Users 2G: ",data$Users_2G, "<br>",
                                 "Users 3G: ",data$Users_3G, "<br>",
                                 "Users 4G: ",data$Users_4G),
                   label = ~as.character(Site))
    }

  })
} 

