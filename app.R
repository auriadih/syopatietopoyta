###########################################################
########################  SERVER  #########################
#
# Luetaan data sisään ja puretaan tallennetut
# aggregaatit omiksi taulukoiksi.
# Tehdään graafit, jotka tulevat syöpätietopöytään.
#
# OBS! jos lisäät tähän osioon graafin, muista lisätä
# graafi myös ui.R koodin body-osioon.
###########################################################

library(hrbrthemes)
library(scales)
library(plotly)
library(dplyr)
library(RColorBrewer)

# Data ----
##Luetaan data ja puretaan aggregaatit

datalist <- readRDS("dummy_dashboard_data.rds")

for (nm in names(datalist)) {
  assign(nm, datalist[[nm]])
}
rm(list = c("nm", "datalist"))


## tehdään eri aggregaattien graafit, jotka luetaan sisään
## ui.R koodin body-osiossa 

server <- function(input, output) {
  
  # _________________________ -----  
  ## Tunnusluvut välilehti ----
  
  
  ##A. Ensidiagnoosi ----
  output$plot5 <- renderPlotly({
    plot_ly(ensiDgn, x = ~vuosi, y = ~N, 
            type = 'scatter', mode = 'lines+markers',
            line = list(color = "#5FACD6", dash = 'dash'), marker = list(color = "#5FACD6"),
            hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>% 
      add_trace(ensiDgn, x = ~vuosi[1:(length(vuosi)-1)], y = ~N[1:(length(N)-1)], 
                type = 'scatter', mode = 'lines+markers', 
                line = list(shape ='linear', color = "#0e73a9", dash = 'solid'), marker = list(color = "#0e73a9"),
                hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>%
      layout(xaxis = list(title = "", showgrid =F, autotick = F, dtick = 1),yaxis = list(title = "", rangemode = "tozero"), 
             showlegend = FALSE)%>%
      #customize modebar
      config(modeBarButtons = list(list("toImage")), displaylogo = FALSE, toImageButtonOptions = list(filename = "plotOutput.png"))
  })
  
  output$ensiDgnTämäVuosi <- renderValueBox({
    valueBox(gsub("(?!^)(?=(?:\\d{3})+$)", " ", ensiDgn$N[length(ensiDgn$N)], perl=T),
             subtitle = paste0("Kuluvan vuoden potilasmäärä"),  
             icon = icon("hospital-user"), color = "light-blue")
  })
  
  output$ensiDgnViimeVuosi <- renderValueBox({
    valueBox(gsub("(?!^)(?=(?:\\d{3})+$)", " ", ensiDgn$N[length(ensiDgn$N)-1], perl=T),
             subtitle = paste0("vuoden ",ensiDgn$vuosi[length(ensiDgn$vuosi)-1], " potilasmäärä"), 
             icon = icon("hospital-user"), color = "light-blue")
  })
  
  ##B. käynnit ----
  output$plot8 <- renderPlotly({
    plot_ly(käynnit_lkm, x = ~vuosi, y =~N, 
            type = 'scatter', mode = 'lines+markers',
            line = list(color = "#BEBCEC", dash = 'dash'), marker = list(color = "#BEBCEC"),
            hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>% 
      add_trace(käynnit_lkm, x = ~vuosi[1:(length(vuosi)-1)], y =~N[1:(length(N)-1)], 
                type = 'scatter', mode = 'lines+markers',
                line = list(shape ='linear', color = "#605ca8", dash = 'solid'), marker = list(color = "#605ca8"),
                hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>% 
      layout(xaxis = list(title = "", showgrid =F, autotick = F, dtick = 1),yaxis = list(title = "miljoonaa käyntiä", rangemode = "tozero" ), 
             showlegend = FALSE) %>% 
      config(modeBarButtons = list(list("toImage")), displaylogo = FALSE, toImageButtonOptions = list(filename = "plotOutput.png"))
    
  })
  
  ## väli erottamaan tuhansia
  ss <- gsub("(?!^)(?=(?:\\d{3})+$)", " ", käynnit_lkm$N[length(käynnit_lkm$N)], perl=T)
  
  output$käynnitTämäVuosi <- renderValueBox({
    valueBox(ss,
             #käynnit_lkm$N[length(käynnit_lkm$N)],
             subtitle = paste0("Kuluvan vuoden käyntien lukumäärä"),
             icon = icon("file-medical-alt"), color = "purple")
    
  })
  
  output$käynnitViimeVuosi <- renderValueBox({
    valueBox(gsub("(?!^)(?=(?:\\d{3})+$)", " ", käynnit_lkm$N[length(käynnit_lkm$N)-1], perl=T),
             subtitle = paste0("vuoden ",käynnit_lkm$vuosi[length(käynnit_lkm$vuosi)-1], " käyntimäärä"), 
             icon = icon("file-medical-alt"), color = "purple")
  })
  
  ##C. sädehoitopotilaiden määrä ----
  output$plot2 <- renderPlotly({
    plot_ly(sadeHtPotilaat, x = ~vuosi, y =~N, 
            type = 'scatter', mode = 'lines+markers',
            line = list(color = "#9FECC1", dash = 'dash'), marker = list(color = "#9FECC1"),
            hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>% 
      add_trace(sadeHtPotilaat, x = ~vuosi[1:(length(vuosi)-1)], y =~N[1:(length(N)-1)], 
                type = 'scatter', mode = 'lines+markers',
                line = list(shape ='linear', color = "#2ca25f", dash = 'solid'), marker = list(color = "#2ca25f"),
                hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>% 
      layout(xaxis = list(title = "", showgrid =F, autotick = F, dtick = 1),
             yaxis = list(title = "", rangemode = 'tozero'), showlegend = FALSE) %>% 
      config(modeBarButtons = list(list("toImage")), displaylogo = FALSE, toImageButtonOptions = list(filename = "plotOutput.png"))
  })
  
  output$sadeHtPotTämäVuosi <- renderValueBox({
    valueBox(gsub("(?!^)(?=(?:\\d{3})+$)", " ", sadeHtPotilaat$N[length(sadeHtPotilaat$N)], perl=T),
             subtitle = paste0("Kuluvan vuoden potilasmäärä"),  
             icon = icon("x-ray"), color = "olive")
  })
  
  output$sadeHtPotViimeVuosi <- renderValueBox({
    valueBox(gsub("(?!^)(?=(?:\\d{3})+$)", " ", sadeHtPotilaat$N[length(sadeHtPotilaat$N)-1], perl=T),
             subtitle = paste0("vuoden ",sadeHtPotilaat$vuosi[length(sadeHtPotilaat$vuosi)-1], " potilasmäärä"), 
             icon = icon("x-ray"), color = "olive")
  })
  
  ##D. kemoterapia ----
  ## ** taulukot, joista puuttuu kuluvan vuoden tieto.
  ## Tarvitaan yhtenäisen viivan piirtämiseen kuvaajaan. 
  syto_pot1 <- sytostResepti_pot_lkm[sytostResepti_pot_lkm$lahde == "Kemokur-hoitokuuri",]
  syto_pot1 <- syto_pot1[1:length(syto_pot1$N)-1,]
  syto_pot2 <- sytostResepti_pot_lkm[sytostResepti_pot_lkm$lahde == "Resepti - L01*",]
  syto_pot2 <- syto_pot2[1:length(syto_pot2$N)-1,]
  
  output$plot4 <- renderPlotly({
    plot_ly()%>% 
      add_trace(sytostResepti_pot_lkm, x = sytostResepti_pot_lkm$vuosi, y =sytostResepti_pot_lkm$N, 
                type='scatter', mode='lines+markers', 
                color=sytostResepti_pot_lkm$lahde, colors =c("#FECEA5", "#FDE2AC"), 
                line = list(dash = 'dash'), showlegend = FALSE) %>%
      add_trace(syto_pot1, x = syto_pot1$vuosi, y =syto_pot1$N, 
                type='scatter',mode='lines+markers', 
                line = list(shape ='linear', dash = 'solid', color ="#F87405"),
                marker = list(color = "#F87405"), name = 'Kemokur-hoitokuuri',
                hovertemplate = paste(paste0('<extra>Kemokur-hoitokuuri</extra> %{y}\n'))) %>%
      add_trace(syto_pot2, x = syto_pot2$vuosi, y =syto_pot2$N, 
                type='scatter',mode='lines+markers', 
                line = list(shape ='linear', dash = 'solid', color ="#F4A60A"),
                marker = list(color = "#F4A60A"), name = 'Resepti - L01*',
                hovertemplate = paste(paste0('<extra>Resepti - L01*</extra> %{y}\n'))) %>%
      layout(xaxis = list(title = "", showgrid =F, autotick = F, dtick = 1),
             yaxis = list(title = "", rangemode = 'tozero')) %>% 
      config(modeBarButtons = list(list("toImage")), displaylogo = FALSE, 
             toImageButtonOptions = list(filename = "plotOutput.png"))
  })
  
#_________________________ -----  
  ## Hoitojaksot välilehti  ----
  
  ## A. Osastohoitojaksojen lukumäärä ----
  output$plot6 <- renderPlotly({
    plot_ly(osHtJakso_lkm, x = ~vuosi, y = ~N, type = 'scatter', mode = 'lines+markers',
            line = list(color = "#5FACD6", dash = 'dash'), marker = list(color = "#5FACD6"),
            hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>% 
      add_trace(osHtJakso_lkm, x = ~vuosi[1:(length(vuosi)-1)], y = ~N[1:(length(N)-1)], 
                type = 'scatter', mode = 'lines+markers', 
                line = list(shape ='linear', color = "#0e73a9", dash = 'solid'), marker = list(color = "#0e73a9"),
                hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>%
      layout(xaxis = list(title = "", showgrid =F, autotick = F, dtick = 1), 
             yaxis = list(title = "", tickformat = "digits", rangemode = "tozero"), showlegend = FALSE) %>% 
      config(modeBarButtons = list(list("toImage")), displaylogo = FALSE, toImageButtonOptions = list(filename = "plotOutput.png"))
  })
  
  ##B. Osastohoitopäivien lukumäärä ----
  output$plot7 <- renderPlotly({
    plot_ly(osHtpvt_lkm, x = ~vuosi, y = ~sum, type = 'scatter', mode = 'lines+markers',
            line = list(color = "#5FACD6", dash = 'dash'), marker = list(color = "#5FACD6"),
            hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>% 
      add_trace(osHtpvt_lkm, x = ~vuosi[1:(length(vuosi)-1)], y = ~sum[1:(length(sum)-1)], 
                type = 'scatter', mode = 'lines+markers', 
                line = list(shape ='linear', color = "#0e73a9", dash = 'solid'), marker = list(color = "#0e73a9"),
                hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>%
      layout(xaxis = list(title = "", showgrid =F, autotick = F, dtick = 1),
             yaxis = list(title = "", tickformat = "digits", rangemode = "tozero"), showlegend = FALSE) %>% 
      config(modeBarButtons = list(list("toImage")), displaylogo = FALSE, toImageButtonOptions = list(filename = "plotOutput.png"))
  })
  
  ##C.  Sädehoitojaksojen lukumäärä ----
  output$plot1 <- renderPlotly({
    plot_ly(sadeHtJakso, x = ~vuosi, y =~N, type = 'scatter', mode = 'lines+markers',
            line = list(color = "#9FECC1", dash = 'dash'), marker = list(color = "#9FECC1"),
            hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>% 
      add_trace(sadeHtJakso, x = ~vuosi[1:(length(vuosi)-1)], y =~N[1:(length(N)-1)], 
                type = 'scatter', mode = 'lines+markers',
                line = list(shape ='linear', color = "#2ca25f", dash = 'solid'), marker = list(color = "#2ca25f"),
                hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>% 
      layout(xaxis = list(title = "", showgrid =F, tickangle = 45, autotick = F, dtick = 1),
             yaxis = list(title = "", rangemode = 'tozero'), showlegend = FALSE) %>% 
      config(modeBarButtons = list(list("toImage")), displaylogo = FALSE, toImageButtonOptions = list(filename = "plotOutput.png"))
    
  })
  
  ##D. Sädehoitofraktioiden lukumäärä ----
  output$plot3 <- renderPlotly({
    plot_ly(sadeHtFraktiot, x = ~vuosi, y =~summ, type = 'scatter', mode = 'lines+markers',
            line = list(color = "#9FECC1", dash = 'dash'), marker = list(color = "#9FECC1"),
            hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>% 
      add_trace(sadeHtFraktiot, x = ~vuosi[1:(length(vuosi)-1)], y =~summ[1:(length(summ)-1)], 
                type = 'scatter', mode = 'lines+markers',
                line = list(shape ='linear', color = "#2ca25f", dash = 'solid'), marker = list(color = "#2ca25f"),
                hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>% 
      layout(xaxis = list(title = "", showgrid =F, tickangle = 45, autotick = F, dtick = 1),
             yaxis = list(title = "", tickformat = "digits", rangemode = 'tozero'), showlegend = FALSE) %>% 
      config(modeBarButtons = list(list("toImage")), displaylogo = FALSE, toImageButtonOptions = list(filename = "plotOutput.png"))
    
  })
  
  # _________________________ -----  
  ## Kuvantaminen välilehti ----
  
  ##A. Kuvantamistutkimusten määrä ----
  output$plot9 <- renderPlotly({
    plot_ly(kuvant_lkm, x = ~vuosi, y =~N)%>% 
      add_trace(type='scatter', mode='lines+markers', color=~tutkimusryhman_selite) %>%
      layout(xaxis = list(title = "", showgrid =F),
             yaxis = list(title = "", tickformat = "digits", rangemode = 'tozero')) %>% 
      config(modeBarButtons = list(list("toImage"), list("hoverClosestCartesian"), list("hoverCompareCartesian")), displaylogo = FALSE, 
             toImageButtonOptions = list(filename = "plotOutput.png"))
  })

  
  ##B. Molekyylipatologian tutkimusten määrä ----
  
  molp <- MolPatTutk_lkm#[MolPatTutk_lkm$laboratorio == "Patologia"]
  molp1 <- molp[1:length(molp$N)-1,]
  
  output$plot15 <- renderPlotly({
    plot_ly(molp, x = ~vuosi, y =~N, type = 'scatter', mode = 'lines+markers',
            line = list(color = "#78b4b8", dash = 'dash'), marker = list(color = "#78b4b8"),
            hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>% 
      add_trace(molp1, x = molp1$vuosi, y = molp1$N, 
                type = 'scatter', mode = 'lines+markers', 
                line = list(shape ='linear', color = "#5c9ca0", dash = 'solid'), marker = list(color = "#5c9ca0"),
                hovertemplate = paste(paste0('<extra></extra> %{y}\n'))) %>%
      layout(xaxis = list(title = "", showgrid =F, autotick = F, dtick = 1),
             yaxis = list(title = "", tickformat = "digits", rangemode = "tozero"), 
             showlegend = FALSE) %>% 
      config(modeBarButtons = list(list("toImage")), displaylogo = FALSE, 
             toImageButtonOptions = list(filename = "plotOutput.png"))
    
  })
  
  
}


###########################################################
##########################  UI  ###########################
#
# Tehdään käyttöliittymä, jossa input datana käytetään 
# server.R koodissa tehtyjä output$plot# graafeja
# 
#Määritellään header, sidebar ja body osiot
#
###########################################################

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(plotly)



###### HEADER ######

header <- dashboardHeader(
  title = "Syöpätietopöytä",
  titleWidth = 350
)

###### SIDEBAR ######

sidebar <- dashboardSidebar(
  shinyjs::useShinyjs(),
  width = 350,
  
  sidebarMenu(
    menuItem(strong("Tunnusluvut", style = "font-family: 'verdana'"), tabName = "Tunnusluvut"),
    menuItem(strong("Hoitojaksot", style = "font-family: 'verdana'"), tabName = "Hoitojaksot"),
    menuItem(strong("Kuvantaminen", style = "font-family: 'verdana'"), tabName = "Kuvantaminen"),
    menuItem(strong("Tutkimusnäyteet", style = "font-family: 'verdana'"), tabName = "Tutkimusnäyteet")
  )
)

###### BODY ######


body <- dashboardBody(
  
  tags$head(tags$style(HTML('
  .skin-blue .main-header .logo {
  font-family: "Verdana";
  font-weight: bold;
  }
  
  /* main sidebar */
  .skin-blue .main-sidebar {
  background-color: #3c3c3c;
  }
                            '))),
  
  tabItems(
    tabItem(tabName = "Tunnusluvut",
            h2(" Kuvaajissa näytetään testidataa, ei oikeaa dataa!"),
            br(),
            fluidRow(
              box(width = 9, title = "Ensidiagnoosin saaneet potilaat", color = "gray", solidHeader = FALSE,
                  collapsible = FALSE,
                  dropdownButton(
                    tags$div(
                      div("Laita tähän infoteksti")),
                    icon = icon('info'), size = 'xs'),
                  plotlyOutput("plot5", height = 300)),
              
              column(width = 3,
                     valueBoxOutput("ensiDgnTämäVuosi",
                                    width = NULL)),
              column(width = 3,
                     valueBoxOutput("ensiDgnViimeVuosi",
                                    width = NULL)),
            ),
            
            fluidRow(
              box(width = 9, title = "Käyntien lukumäärä", color = "gray", solidHeader = FALSE,
                  collapsible = FALSE,
                  dropdownButton(
                    tags$div(
                      div("Laita tähän infoteksti")),
                    icon = icon('info'), size = 'xs'),
                  plotlyOutput("plot8", height = 300)),
              
              column(width = 3,
                     valueBoxOutput("käynnitTämäVuosi",
                                    width = NULL)),
              column(width = 3,
                     valueBoxOutput("käynnitViimeVuosi",
                                    width = NULL)),
            ),
            
            fluidRow(
              box(width = 9, title = "Sädehoito - potilaiden määrä", color = "gray", solidHeader = FALSE,
                  collapsible = FALSE,
                  dropdownButton(
                    tags$div(
                      div("Laita tähän infoteksti")),
                    icon = icon('info'), size = 'xs'),
                  plotlyOutput("plot2", height = 300)),
              
              column(width = 3,
                     valueBoxOutput("sadeHtPotTämäVuosi",
                                    width = NULL)),
              column(width = 3,
                     valueBoxOutput("sadeHtPotViimeVuosi",
                                    width = NULL)),
            ),
            
            fluidRow(
              box(width = 12, title = "Kemoterapia - potilaiden määrä", color = "gray", solidHeader = FALSE,
                  collapsible = FALSE,
                  dropdownButton(
                    tags$div(
                      div("Laita tähän infoteksti")),
                    icon = icon('info'), size = 'xs'),
                  plotlyOutput("plot4", height = 350))
            )
    ),
    tabItem(tabName = "Hoitojaksot",
            h2(" Kuvaajissa näytetään testidataa, ei oikeaa dataa!"),
            br(),
            fluidRow(
              box(width = 6, title = "Osastohoitojaksojen lukumäärä", color = "gray", solidHeader = FALSE,
                  collapsible = FALSE,
                  dropdownButton(
                    tags$div(
                      div("Laita tähän infoteksti")),
                    icon = icon('info'), size = 'xs'),
                  plotlyOutput("plot6", height = 300)),
              
              box(width = 6, title = "Osastohoitopäivien lukumäärä", color = "gray", solidHeader = FALSE,
                  collapsible = FALSE,
                  dropdownButton(
                    tags$div(
                      div("Laita tähän infoteksti")),
                    icon = icon('info'), size = 'xs'),
                  plotlyOutput("plot7", height = 300))
            ),
            fluidRow(
              box(width = 6, title = "Sädehoitojaksot", color = "gray", solidHeader = FALSE,
                  collapsible = FALSE,
                  dropdownButton(
                    tags$div(
                      div("Laita tähän infoteksti")),
                    icon = icon('info'), size = 'xs'),
                  plotlyOutput("plot1", height = 300)),
              
              box(width = 6, title = "Sädehoitofraktiot", color = "gray", solidHeader = FALSE,
                  collapsible = FALSE,
                  dropdownButton(
                    tags$div(
                      div("Laita tähän infoteksti")),
                    icon = icon('info'), size = 'xs'),
                  plotlyOutput("plot3", height = 300))
            )
    ),
    tabItem(tabName = "Kuvantaminen",
            h2(" Kuvaajissa näytetään testidataa, ei oikeaa dataa!"),
            br(),
            fluidRow(
              box(width = 12, title = "Kuvantamistutkimusten määrä", color = "gray", solidHeader = FALSE,
                  collapsible = FALSE,
                  dropdownButton(
                    tags$div(
                      div("Laita tähän infoteksti")),
                    icon = icon('info'), size = 'xs'),
                  plotlyOutput("plot9", height = 350))),
            
    ),
    tabItem(tabName = "Tutkimusnäyteet",
            h2(" Kuvaajissa näytetään testidataa, ei oikeaa dataa!"),
            br(),
              
              box(width = 12, title = "Molekyylipatologian tutkimukset", color = "gray", solidHeader = FALSE,
                  collapsible = FALSE,
                  dropdownButton(
                    tags$div(
                      div(strong("INFO:"),
                          br(),
                          "Laita tähän infoteksti")),
                    icon = icon('info'), size = 'xs'),
                  plotlyOutput("plot15", height = 350))
              
            )
    )
  )
  



ui <- dashboardPage(header, sidebar, body)

shinyApp(ui=ui, server=server)