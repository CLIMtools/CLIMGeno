library(shiny)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(shinyjqui)###need to upload package to server

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title    
  tags$head(
    tags$link(rel="stylesheet", type="text/css",href="style2.css"),
    tags$head(includeScript("google-analytics.js")),
    tags$script(type="text/javascript", src = "md5.js"),
    tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')
    
  ),
  useShinyjs(),
  uiOutput("app"),
  
  
  headerPanel(
    list(tags$head(tags$style("body {background-color: white; }")),
         "  CLIMGeno", HTML('<img src="picture2.png", height="100px",  
                        style="float:left"/>','<p style="color:orange">Climate x Genetic association GWAS browser </p>' ))
         ),
  theme = shinytheme("cerulean"),  
  
  # Sidebar with a slider input for the number of bins
  jqui_draggable( sidebarPanel(
        #    wellPanel(
        #      uiOutput("datasets")
        #    ),
        #uiOutput("ui_Manage")
        uiOutput("ui_All"),
        width=3,       wellPanel(a(h4('Please cite us in any publication that utilizes information from  Arabidopsis CLIMtools:'),  href = "https://www.nature.com/articles/s41559-018-0754-5", h6('Ferrero-Serrano, Á & Assmann SM. Phenotypic and genome-wide association with the local environment of Arabidopsis. Nature Ecology & Evolution. doi: 10.1038/s41559-018-0754-5 (2019)' ))),
wellPanel(tags$a(img(src='climgenowarning.png', h3("Considerations before using this tool"), height="120px"),href="myfile.pdf"),align="center"), wellPanel(tags$a(img(src='FDR.png', h3("Explore FDR of any ExG association"), height="120px"),href="https://rstudio.aws.science.psu.edu:3838/aaf11/FDRCLIM/"),align="center"), wellPanel(a("Tweets by @ClimTools", class="twitter-timeline"
                                , href = "https://twitter.com/ClimTools"), style = "overflow-y:scroll; max-height: 1000px"
        ),
        wellPanel( h6('Contact us: clim.tools.lab@gmail.com'),wellPanel(tags$a(div(
          img(src = 'github.png',  align = "middle"), style = "text-align: center;"
        ), href = "https://github.com/CLIMtools/CLIMGeno")))

        ###################################################
  )                 
  
),
 
                 
                 
                 ###################################################
 
  
      mainPanel( 
   tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        
        
          tagList( # The four core files: 3 JS files and 1 CSS file --
          #      singleton(tags$head(tags$script(src='js/highcharts.js',type='text/javascript'))),
          tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");'),
          singleton(tags$head(tags$script(src='/datatables/js/jquery.dataTables.js',type='text/javascript'))),
          singleton(tags$head(tags$script(src='/tabletools/js/TableTools.js',type='text/javascript'))),
          singleton(tags$head(tags$script(src='/tabletools/js/ZeroClipboard.js',type='text/javascript'))),
          singleton(tags$head(tags$link(href='/tabletools/css/TableTools.css',rel='stylesheet',type='text/css')))
          #singleton(tags$head(tags$script(src='http://code.highcharts.com/highcharts.js',type='text/javascript')))
        ),    
        
        #progressInit(),    
      wellPanel( uiOutput("ui_data_tabs"),
                 width=9,
        tableOutput('contents')
      ))
    ))
