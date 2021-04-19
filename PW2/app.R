library(shiny)
library(knitr)
library(tidyverse)
library(DT)
#library(patchwork)
library(readxl)
#library(sp)
#library(tmap)  
library(viridis)
library(leaflet)
library(tigris)
library(plotly)
library(shinyWidgets)
library(table1)
library(htmlTable)
library(scales)
library(flexdashboard)
library(RColorBrewer)
library(grid)
library(Dict)
library(lubridate)
library(zipcodeR)
#library(writexl)
library(htmltools)
### URL

url1 = "https://twitter.com/intent/tweet?text=Hello%20world&url=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url2 = "https://www.facebook.com/sharer.php?u=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url3 = "https://www.instagram.com/columbiapublichealth/"
url4 = "https://www.linkedin.com/shareArticle?mini=true&url=https://msph.shinyapps.io/nyc-neighborhoods-covid/&title=&summary=&source="
url5 = "mailto:NYCN-COVID@cumc.columbia.edu?&subject=&body=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url6 = "whatsapp://send?text=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url7 = "https://service.weibo.com/share/share.php?url=https://msph.shinyapps.io/nyc-neighborhoods-covid/&title="

### DATA import
leafletSizingPolicy <- function(
  defaultWidth = "100%",
  defaultHeight = 400,
  padding = 0,
  browser.fill = TRUE,
  ...
  # not adding extra arguments as htmlwidgets::sizingPolicy can change their own args
) {
  htmlwidgets::sizingPolicy(
    defaultWidth = defaultWidth,
    defaultHeight = defaultHeight,
    padding = padding,
    browser.fill = browser.fill,
    ...
  )
}

zip1 = zipcodeR::zip_code_db
cdata_1 = read_excel("./cdata3_1.xlsx")#[,-1]
cws_1 = read_excel("./cws3_1.xlsx")#[,-1]
counties <- rgdal::readOGR("geo.json")


cd1 = read_excel("./cd.xlsx")

c_1 = function(data){
  
  k = 5:26
  
  for(i in k){
    m = data[,i]%>% as.matrix() %>% as.numeric()
    m[m == "999999"] = 0
    m = m %>% as.matrix() %>% as.numeric()
    m[m == 0] = "Inadequate data"
    data[,i] = m
  }
  
  
  return(data)
  
}


c_12 = function(name, nnn1){
  d1 = strsplit(name, 2)[[1]][1]
  m = nnn1%>% as.matrix() %>% as.numeric()
  if(d1 == "As."){
    m[m >= 10] = 10
  }
  
  #Sb
  if(d1 == "Sb."){
    m[m >= 6] = 6
  }
  
  #Ba
  if(d1 == "Ba."){
    m[m >= 200] = 200
  }
  
  #Be
  if(d1 == "Be."){
    m[m >= 4] = 4
  }
  
  #Cd
  if(d1 == "Cd."){
    m[m >= 5] = 5
  }
  
  #Cr
  if(d1 == "Cr."){
    m[m >= 100] = 100
  }
  
  #Hg
  if(d1 == "Hg."){
    m[m >= 2] = 2
  }
  
  #Se
  if(d1 == "Se."){
    m[m >= 50] = 50
  }
  
  #Ti
  if(d1 == "Tl."){
    m[m >= 2] = 2
  }
  
  #CN
  if(d1 == "CN."){
    m[m >= 200] = 200
  }
  
  #U
  if(d1 == "U."){
    m[m >= 30] = 30
  }
  
  return(m)
  
}


leafletSizingPolicy <- function(
  defaultWidth = "100%",
  defaultHeight = 400,
  padding = 0,
  browser.fill = TRUE,
  ...
  # not adding extra arguments as htmlwidgets::sizingPolicy can change their own args
) {
  htmlwidgets::sizingPolicy(
    defaultWidth = defaultWidth,
    defaultHeight = defaultHeight,
    padding = padding,
    browser.fill = browser.fill,
    ...
  )
}

cd2 = c_1(cd1)

df = geo_join(counties, cd2, "GEO_ID", "id")
metals = colnames(cd2[,5:26])   %>% as.factor()%>% as.vector()
cws2 = read_excel("./cws.xlsx")
#metals2 = colnames(cws2[,8:30]) %>% as.vector()

cc1 = cws2 %>% distinct(CountyFIPS)
county_c = cc1$CountyFIPS %>% as.vector()

zip = zipcodeR::zip_code_db
test1 = left_join(zip, cws2, by = "zipcode") %>% filter(PWSID != "AK2260595")
icons <- makeIcon("ico2.png", iconWidth = 24, iconHeight = 24)

pc = function(df1,nnn, df2){
  nnn1 = df1@data %>% pull(nnn)
  
  nnn2 = c_12(nnn, nnn1)
  m = leaflet(df1) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) #%>% addPolygons()
  
  
  pal <- colorNumeric(c("#f0f9e8","#fed98e","#fe9929","#d95f0e","#993404"), domain= nnn2%>% as.numeric())
  
  
  popup_sb <- paste0("<b> State: </b>", df1$State.Code,
                     "<br>",
                     "<b> County: </b>",df1$GEOID," " , df1$NAME_2,
                     "<br>",
                     "<b> Region: </b> ", df1$Region,
                     "<br>", 
                     "<b> Metal Concentration: </b>",nnn1," ug/L")
  
  popup_sb2 <- paste0(
    "PWSID:", df2$PWSID,
    " ,", df2$PWSID.Name,
    " ,Metal Concentration:",df2 %>% pull(nnn)," ug/L")
  
  
  m %>% addPolygons(
    fillColor = ~pal(nnn2 %>% as.numeric()),
    weight = 0.2,
    opacity = 1,
    color = "black",
    #dashArray = "3",
    fillOpacity = 0.7,
    popup = ~popup_sb)  %>%
    addLegend(pal = pal, 
              values =  nnn2 %>% as.numeric(), 
              position = "bottomright", 
              title = "Concentrations",
              labFormat = labelFormat(suffix = "+ ug/L"),
              na.label = "NA")%>%
    addMarkers( 
      clusterOptions = markerClusterOptions(),
      clusterId = "quakesCluster",
      ~df2$lng, ~df2$lat,
      icon = icons,
      #~as.character(nn1),
      #color = "red",
      label = ~popup_sb2#,
      #fillOpacity = 0.005,
      #radius = 0.000001
    )  %>% 
    #addMiniMap() %>%
    addEasyButton(easyButton(
      states = list(
        easyButtonState(
          stateName="unfrozen-markers",
          icon="ion-toggle",
          title="Freeze Clusters",
          onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
        ),
        easyButtonState(
          stateName="frozen-markers",
          icon="ion-toggle-filled",
          title="UnFreeze Clusters",
          onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")
        )
      )
    ))
}








### UI

ui <- navbarPage(
  #theme = "shiny.css",
  title = div(img(src='logo.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
  windowTitle = " Columbia University Public Drinking Water Contaminant Exposure Estimates Dashboard",
  id = 'menus',
  tabPanel('Interactive Map',
           shinyjs::useShinyjs(),
           fluidRow(
             #column(width = 5, offset = 1, div(img(src = "newlogo3.png", height = "100%",width = "100%"),
             #style="text-align: center;")),
             #column(width = 5,  div(img(src = "HomePagepic.png", height = "100%",width = "100%"),
             #style="text-align: center;"))
           ),
           br(),
           fluidRow(column(width = 10, offset = 1, span(" Columbia University Public Drinking Water Contaminant Exposure Estimates Dashboard", style="font-size: 30px;line-height:150%"))),           
           
           br(),
           fluidRow(column(width = 8, offset = 1, span(htmlOutput("Hometext"), style="font-size: 15px;line-height:150%"))),
           br(),
           fluidRow(
             column(width = 10, offset = 1, h2("")),
             column(width = 10, offset = 1, h4("Metals Concentrations Distribution by County Level")),
             column(width = 8, offset = 1, span(htmlOutput("Hometext5"), style="font-size: 15px;line-height:150%")),
             
             #,position = c("left","right")
             
           ),
           br(),
           fluidRow(
             column(2, offset = 1,
                    column(12, pickerInput(inputId = "p1_m", 
                                           label = "Choose Metals & Year Range:", 
                                           choices = metals,
                                           selected = metals[1]
                    )))
             ,
             column(width = 7,leafletOutput(outputId = "p1",width="75%",height="400px"))),
           br(),
           fluidRow(column(width = 8, offset = 1, helpText("Full datasets at the community water system- and county-level are available through GitHub"))),
           br(),
           fluidRow(column(width = 8, offset = 1, span(htmlOutput("Hometext2"), style="font-size: 15px;line-height:150%"))),
           br(),
           fluidRow(column(width = 8, offset = 1, span(htmlOutput("Hometext3"), style="font-size: 15px;line-height:150%"))),
           br(),
           
           fluidRow(align="center",
                    span(htmlOutput("bannertext", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
                    #span(htmlOutput("sharetext", style="color:white")),
                    #br(),
                    #img(src='bottomlogo.png', height="20%", width="20%"),
                    h5("Share on", style="color:white;font-size:12px"),
                    actionButton("twitter_index",
                                 label = "",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url1),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("fb_index",
                                 label = "",
                                 icon = icon("facebook"),
                                 onclick = sprintf("window.open('%s')", url2),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    #actionButton("ins_index",
                    #             label = "",
                    #             icon = icon("instagram"),
                    #             onclick = sprintf("window.open('%s')", url3),
                    #             style = "border-color: #FFFFFF;"),
                    actionButton("linkedin_index",
                                 label = "",
                                 icon = icon("linkedin"),
                                 onclick = sprintf("window.open('%s')", url4),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("whats_index",
                                 label = "",
                                 icon = icon("whatsapp"),
                                 onclick = sprintf("window.open('%s')", url6),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("email_index",
                                 label = "",
                                 icon = icon("envelope"),
                                 onclick = sprintf("window.open('%s')", url5),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
                    
           )
           
  ),
  
  
  tabPanel(
    # Application title
    title= "Community water system estimates",
    # Sidebar with a slider input for number of bins 
    fluidRow(
      column(width = 10, offset = 1, h2("Community water system contaminant levels")),
      column(width = 10, offset = 1, h4("Tracking  Inequalities in Public Water Metals Concentrations in Community Water Systems across the United States
")),        
      column(2, offset = 1,
             selectInput("m_t1",
                         "Metal:",
                         c("All",
                           unique(as.character(cws_1$Metal)))),
             selectInput("yr_t1",
                         "Year Range:",
                         c("All",
                           unique(as.character(cws_1$`Year Range`))))),
      #column(2, offset = 1,
      #       selectInput("yr_t1",
      #                   "Year Range:",
      #                  c("All",
      #                    unique(as.character(cws_1$`Year Range`))))),
      column(2, offset = 1,
             selectInput("p_t1",
                         "PWSID:",
                         c("All",
                           unique(as.character(cws_1$PWSID)))),
             selectInput("pn_t1",
                         "PWSID Name:",
                         c("All",
                           unique(as.character(cws_1$`PWSID Name`))))),
      column(2, offset = 1,
             selectInput("cf_t1",
                         "County FIPS:",
                         c("All",
                           unique(as.character(cws_1$`County FIPS`)))),
             selectInput("ac_t1",
                         "Administrative City:",
                         c("All",
                           unique(as.character(cws_1$`Administrative City`))))),
      # Create a new row for the table.
      column(width = 10, offset = 1, align="center", DT::dataTableOutput("t_1")),           
      
      column(width = 10, offset = 1, helpText("Full datasets at the community water system- and county-level are available through GitHub"))),
    br(),
    fluidRow(column(width = 8, offset = 1, span(htmlOutput("t1text"), style="font-size: 15px;line-height:150%"))),
    br(),
    fluidRow(align="center",
             span(htmlOutput("bannertext1", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
             #span(htmlOutput("sharetext", style="color:white")),
             #br(),
             #img(src='bottomlogo.png', height="20%", width="20%"),
             h5("Share on", style="color:white;font-size:12px"),
             actionButton("twitter_index",
                          label = "",
                          icon = icon("twitter"),
                          onclick = sprintf("window.open('%s')", url1),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             actionButton("fb_index",
                          label = "",
                          icon = icon("facebook"),
                          onclick = sprintf("window.open('%s')", url2),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             #actionButton("ins_index",
             #             label = "",
             #             icon = icon("instagram"),
             #             onclick = sprintf("window.open('%s')", url3),
             #             style = "border-color: #FFFFFF;"),
             actionButton("linkedin_index",
                          label = "",
                          icon = icon("linkedin"),
                          onclick = sprintf("window.open('%s')", url4),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             actionButton("whats_index",
                          label = "",
                          icon = icon("whatsapp"),
                          onclick = sprintf("window.open('%s')", url6),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             actionButton("email_index",
                          label = "",
                          icon = icon("envelope"),
                          onclick = sprintf("window.open('%s')", url5),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
             
    )
  ),
  
  tabPanel(
    # Application title
    title= "County-level community water system contaminant levels",
    # Sidebar with a slider input for number of bins 
    fluidRow(
      column(width = 10, offset = 1, h2("Public Water Tracker")),
      column(width = 10, offset = 1, h4("Tracking  Inequalities in Public Water Metals Concentrations in Counties across the United States
")),
      column(2, offset = 1,
             selectInput("m_t2",
                         "Metal:",
                         c("All",
                           unique(as.character(cdata_1$Metal)))),
             selectInput("yr_t2",
                         "Year Range:",
                         c("All",
                           unique(as.character(cdata_1$`Year Range`))))),
      #column(2, offset = 1,
      #       selectInput("yr_t1",
      #                   "Year Range:",
      #                  c("All",
      #                    unique(as.character(cws_1$`Year Range`))))),
      column(2, offset = 1,
             selectInput("cf_t2",
                         "County FIPS:",
                         c("All",
                           unique(as.character(cdata_1$`County FIPS`)))),
             selectInput("cn_t1",
                         "County Name:",
                         c("All",
                           unique(as.character(cdata_1$`County Name`))))),
      column(2, offset = 1,
             selectInput("sc_t2",
                         "State Code:",
                         c("All",
                           unique(as.character(cdata_1$`State Code`))))),
      # Create a new row for the table.
      column(width = 10, offset = 1, align="center", DT::dataTableOutput("t_2")),           
      
      column(width = 10, offset = 1, helpText("Full datasets at the community water system- and county-level are available through GitHub"))
    ),
    br(),
    fluidRow(column(width = 8, offset = 1, span(htmlOutput("t2text"), style="font-size: 15px;line-height:150%"))),
    br(),
    fluidRow(align="center",
             span(htmlOutput("bannertext2", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
             #span(htmlOutput("sharetext", style="color:white")),
             #br(),
             #img(src='bottomlogo.png', height="20%", width="20%"),
             h5("Share on", style="color:white;font-size:12px"),
             actionButton("twitter_index",
                          label = "",
                          icon = icon("twitter"),
                          onclick = sprintf("window.open('%s')", url1),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             actionButton("fb_index",
                          label = "",
                          icon = icon("facebook"),
                          onclick = sprintf("window.open('%s')", url2),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             #actionButton("ins_index",
             #             label = "",
             #             icon = icon("instagram"),
             #             onclick = sprintf("window.open('%s')", url3),
             #             style = "border-color: #FFFFFF;"),
             actionButton("linkedin_index",
                          label = "",
                          icon = icon("linkedin"),
                          onclick = sprintf("window.open('%s')", url4),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             actionButton("whats_index",
                          label = "",
                          icon = icon("whatsapp"),
                          onclick = sprintf("window.open('%s')", url6),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             actionButton("email_index",
                          label = "",
                          icon = icon("envelope"),
                          onclick = sprintf("window.open('%s')", url5),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
             
    )
  ),  
  
  
  tabPanel("About Us",
           fluidRow(column(10, offset = 1, h2("About Us")),
                    
                    #column(10, offset = 1,helpText("MSPH photo source: https://globalcenters.columbia.edu/content/yusuf-hamied-fellowships-program")),
                    column(10, offset = 1,span(uiOutput("ab1",style = "font-size: 15px; line-height:150%"))),
                    column(10, offset = 1,span(uiOutput("ab2",style = "font-size: 15px; line-height:150%"))),
                    column(10, offset = 1,span(uiOutput("ab3",style = "font-size: 15px; line-height:150%"))),
                    column(10, offset = 1, div(img(src = "cc.JPG", height = "55%",width = "70%"),
                                               style="text-align: center;"))
           ),
           br(),
           fluidRow(align="center",
                    span(htmlOutput("bannertext5", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
                    #span(htmlOutput("sharetext", style="color:white")),
                    #br(),
                    #img(src='bottomlogo.png', height="20%", width="20%"),
                    h5("Share on", style="color:white;font-size:12px"),
                    actionButton("twitter_index",
                                 label = "",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url1),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("fb_index",
                                 label = "",
                                 icon = icon("facebook"),
                                 onclick = sprintf("window.open('%s')", url2),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    #actionButton("ins_index",
                    #             label = "",
                    #             icon = icon("instagram"),
                    #             onclick = sprintf("window.open('%s')", url3),
                    #             style = "border-color: #FFFFFF;"),
                    actionButton("linkedin_index",
                                 label = "",
                                 icon = icon("linkedin"),
                                 onclick = sprintf("window.open('%s')", url4),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("whats_index",
                                 label = "",
                                 icon = icon("whatsapp"),
                                 onclick = sprintf("window.open('%s')", url6),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("email_index",
                                 label = "",
                                 icon = icon("envelope"),
                                 onclick = sprintf("window.open('%s')", url5),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
                    
           ))
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  shinyjs::addClass(id = "menus", class = "navbar-right")
  
  output$bannertext = renderText({
    return(
      
      "<b> Columbia <b> University<b> Public<b> Drinking<b> Water<b> Contaminant <b>Exposure <b>Estimates <b>Dashboard"
    )
  })
  
  output$bannertext1 = renderText({
    return(
      "<b> Columbia <b> University<b> Public<b> Drinking<b> Water<b> Contaminant <b>Exposure <b>Estimates <b>Dashboard"
    )
  })
  
  output$bannertext2 = renderText({
    return(
      "<b> Columbia <b> University<b> Public<b> Drinking<b> Water<b> Contaminant <b>Exposure <b>Estimates <b>Dashboard"
    )
  })
  
  output$bannertext3 = renderText({
    return(
      "<b> Columbia <b> University<b> Public<b> Drinking<b> Water<b> Contaminant <b>Exposure <b>Estimates <b>Dashboard"
    )
  })
  
  output$bannertext4 = renderText({
    return(
      "<b> Columbia <b> University<b> Public<b> Drinking<b> Water<b> Contaminant <b>Exposure <b>Estimates <b>Dashboard"
    )
  })
  
  output$bannertext5 = renderText({
    return(
      "<b> Columbia <b> University<b> Public<b> Drinking<b> Water<b> Contaminant <b>Exposure <b>Estimates <b>Dashboard"
    )
  })
  
  output$bannertext6 = renderText({
    return(
      "<b> Columbia <b> University<b> Public<b> Drinking<b> Water<b> Contaminant <b>Exposure <b>Estimates <b>Dashboard"
    )
  })
  
  output$sharetext = renderText({
    return(
      "<b> Share on </b> "
    )
  })
  
  
  
  output$Hometext = renderUI({
    syr = a("Six Year Review of Drinking Water Standards", href = "https://www.epa.gov/dwsixyearreview", target="_blank")
    rn2020 = a("Nigra et al. 2020", href = "https://ehp.niehs.nih.gov/doi/full/10.1289/EHP7313", target="_blank")
    
    tagList(
      "The Columbia University Public Drinking Water Contaminant Exposure Estimates Dashboard is a tracker and data visualization tool of contaminant exposure estimates in community water systems across the US for the both researchers and the public. The Dashboard is an accompaniment to the manuscript Ravalli et al. 2021, “Uranium and metal concentrations in community water systems across the United States, 2006-2011.” Estimates are derived from the US Environmental Protection Agency’s dataset in support of the", syr,". 
      
      See Ravalli et al. 2021 and ", rn2020 ," for detailed methodologic information."
    )
  })
  
  output$t1text = renderUI({
    
    tagList(
      "CWS average contaminant concentrations that were estimated below the EPA’s maximum method detection limit were replaced by “<LOD” for display in this table. Datasets available for download via GitHub retain the original estimated or imputed value, even if the imputed value reflects a system with concentrations measured below the detection limit. Method detection limits in ug/L were 0.5 (As), 0.4 (Sb), 0.8 (Ba), 0.2 (Be), 0.05 (Cd), 0.08 (Cr), 0.2 (Hg), 0.6 (Se), 0.3 (Tl), and 5.0 (CN). Because EPA did not publish a method detection limit for U in the SYR3 documentation, we assumed a value of 0.5 ug/L for U."
    )
  })
  
  output$t2text = renderUI({
    
    tagList(
      "Individual monitoring record values measured below the limit of detection were replaced by the limit of detection divided by the square root of two prior to averaging concentrations. Low estimated county-level averages may therefore reflect water systems which did not measure concentrations of a particular contaminant above the limit of detection. Method detection limits in ug/L were 0.5 (As), 0.4 (Sb), 0.8 (Ba), 0.2 (Be), 0.05 (Cd), 0.08 (Cr), 0.2 (Hg), 0.6 (Se), 0.3 (Tl), and 5.0 (CN). Because EPA did not publish a method detection limit for U in the SYR3 documentation, we assumed a value of 0.5 ug/L for U."
    )
  })
  
  output$Hometext2 = renderUI({
    
    tagList(
      "Individual community water systems appear as points randomly jittered within the administrative zip-code associated with that water system. At the county-level, 
        average concentrations were weighted by the population served by each community water system to estimate the county-level weighted average community water system concentrations. Counties which were not represented by any community water systems in the Six Year Review database are labeled as “No data available.” Counties with “Inadequate data”  did not have community water system data representing at least 50% of the public water reliant population."
    )
  })
  
  output$Hometext3 = renderUI({
    
    tagList(
      "A searchable table of community water system-level exposure estimates is available on the Community water system estimates page. A searchable table of county-level exposure estimates is available on the County level estimates page."
    )
  })
  
  output$Hometext4 = renderUI({
    datalink = a("here", href = "https://github.com/annenigra/US-PublicWaterSystem-Metal-Estimates", target="_blank")
    tagList(
      
      "Full datasets at the community water system- and county-level are available through GitHub", datalink, "."
    )
  }) 
  
  output$Hometext5 = renderUI({
    
    tagList(
      "Individual monitoring record values measured below the limit of detection were replaced by the limit of detection divided by the square root of two prior to averaging concentrations. Low estimated county-level averages may therefore reflect water systems which did not measure concentrations of a particular contaminant above the limit of detection. Method detection limits in ug/L were 0.5 (As), 0.4 (Sb), 0.8 (Ba), 0.2 (Be), 0.05 (Cd), 0.08 (Cr), 0.2 (Hg), 0.6 (Se), 0.3 (Tl), and 5.0 (CN). Because EPA did not publish a method detection limit for U in the SYR3 documentation, we assumed a value of 0.5 ug/L for U."
    )
  })
  
  output$ab1 = renderUI({
    cumsph = a("Columbia University Mailman School of Public Health", href = "https://www.publichealth.columbia.edu/", target="_blank")
    ehs = a("Environmental Health Sciences", href = "https://www.publichealth.columbia.edu/academics/departments/environmental-health-sciences-ehs", target="_blank")
    bios = a("Biostatistics", href = "https://www.publichealth.columbia.edu/academics/departments/biostatistics", target="_blank")
    cura = a("Columbia University Superfund Research Program", href = "https://www.publichealth.columbia.edu/research/columbia-superfund-research-program", target="_blank")
    
    tagList(
      "The Columbia University Public Drinking Water Contaminant Exposure Estimates Dashboard is an interdisciplinary project housed in the",cumsph , "Departments of", ehs,"  and", bios," , and the", cura,"."
    )
  })
  
  
  output$ab2 = renderUI({
    yyz = a("Yuanzhi Yu", href = "https://yy3019.github.io/", target="_blank")
    fr = a("Filippo Ravalli", href = "https://www.linkedin.com/public-profile/in/filippo-ravalli/", target="_blank") 
    an = a("Dr. Anne Nigra", href = "https://annenigra.github.io/", target="_blank")
    ana = a("Dr. Ana Navas-Acien", href = "https://www.publichealth.columbia.edu/people/our-faculty/an2737", target="_blank")    
    
    tagList(
      "The Dashboard was developed by", yyz,",", fr,",", an,",", ana,"."
    )
  })
  
  output$ab3 = renderUI({
    pp =  a("Nigra et al. 2020", href = "https://ehp.niehs.nih.gov/doi/full/10.1289/EHP7313", target="_blank")   
    
    tagList(
      "If you believe there is an error on our site, please feel free to contact us. See Ravalli et al. 2021 and", pp," for detailed methodologic information."
    )
  })
  
  
  #    output$map = renderLeaflet({
  
  #       plot = switch (input$outcome_selection,
  #                      positive = positive,
  #                      death_count = death_count,
  #                       case_rate = case_rate,
  #                      death_rate = death_rate,
  #                      newcase = newcase,
  #                      incidencerate = incidencerate
  #       )
  
  #       plot(input$date_choice)
  #   })
  
  
  
  
  #  fig <- plot_ly(sort = FALSE)
  
  ###################### Table
  
  
  output$t_1 <- DT::renderDataTable(DT::datatable({
    data <- cws_1
    if (input$m_t1 != "All") {
      data <- data[data$Metal == input$m_t1,]
    }
    if (input$yr_t1 != "All") {
      data <- data[data$`Year Range` == input$yr_t1,]
    }
    if (input$p_t1 != "All") {
      data <- data[data$PWSID == input$p_t1,]
    }
    if (input$pn_t1 != "All") {
      data <- data[data$`PWSID Name` == input$pn_t1,]
    }
    if (input$cf_t1 != "All") {
      data <- data[data$`County FIPS` == input$cf_t1,]
    }
    if (input$ac_t1 != "All") {
      data <- data[data$`Administrative City` == input$ac_t1,]
    }
    data
  }))
  
  output$t_2 <- DT::renderDataTable(DT::datatable({
    data <- cdata_1
    if (input$m_t2 != "All") {
      data <- data[data$Metal == input$m_t2,]
    }
    if (input$yr_t2 != "All") {
      data <- data[data$`Year Range` == input$yr_t2,]
    }
    if (input$sc_t2 != "All") {
      data <- data[data$`State Code` == input$sc_t2,]
    }
    if (input$cf_t2 != "All") {
      data <- data[data$`County FIPS` == input$cf_t2,]
    }
    if (input$cn_t1 != "All") {
      data <- data[data$`County Name` == input$cn_t1,]
    }
    data
  }))    
  
  output$p1 <- renderLeaflet({
    df1 <- df
    df2 = test1
    pc(df1, input$p1_m, df2)
    
    
  })   
  
  
  
  
}



shinyApp(ui, server)




