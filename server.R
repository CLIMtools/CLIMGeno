library(shiny)
library(shinyjs)
library(shinyURL)
library(leaflet.extras)

shinyServer(function(input, output, session) {
  

 

  options("stringsAsFactors" = FALSE)
  #Load any saved datasets
  values <- reactiveValues()
  dataPath <- "./www/config/data/"
  dataFiles <- list.files(dataPath,recursive=T)
  for(i in dataFiles){
     values[[i]] <- read.table(paste0(dataPath,i),sep=",",stringsAsFactors=FALSE,head=TRUE)     
  }  
  values$datasetlist <- dataFiles
#  values[["ionomics"]] <- aggTable
#  values$datasetlist <- dataFiles
  
  #handles what displays in the sidebar based on what tab is selected
  output$ui_All <- renderUI({
    list(
      conditionalPanel(condition = "input.datatabs == 'Manage'",
         wellPanel(
           uiOutput("datasets")
         ),                       
        wellPanel(
           conditionalPanel(condition = "input.dataType != 'examples'",
                            
                            uiOutput("organism"),
                            fileInput('uploadfile', '', multiple=TRUE)
           ),      
           conditionalPanel(condition = "input.dataType == 'examples'",
                            actionButton('loadExampleData', 'Load examples')
           )
         
         )
      ),#end conditional Manage
      conditionalPanel(condition = "input.datatabs == 'Table'",
         wellPanel(
           uiOutput("columns"),
           tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(), #add some space between selection columns and subset search
           # uiOutput("view_order"), checkboxInput("view_order_desc", "DESC", value = FALSE),
           returnTextInput("dv_select", "Subset (e.g., score > 6 & Effect == 'missense variant')", '')
         ),
         helpModal('Data Table View','view',includeMarkdown("tools/manage.md"))      
      ),#end conditional Table
      #the second part of the statement is what is allowing the detection of changing panels due to a click event, i couldn't figure out how to update input$datatabs with javascript
      conditionalPanel(condition = "input.datatabs == 'panel1' || input.datatabs == 'panel2' || $('li.active a').first().html()==='Chromosome View'",
         helpText(h5(p("Interactive Graphs for GWAS Data"))),
         wellPanel(
            uiOutput("traitColBoxes"),
            uiOutput("legend"),
            uiOutput("overlaps"),
            conditionalPanel(condition = "input.overlaps==true",
                             uiOutput("overlapSize"),
                             uiOutput("numOverlapTraits")
                             )#end conditional for plotting only overlaps
         )
         #submitButton("Update View"),

      ),#end conditional for traitCols (genome view and chromosomeview)
      conditionalPanel(condition = "input.datatabs == 'panel2' || $('li.active a').first().html()==='Chromosome View'",
         wellPanel(
            uiOutput("selectChr")
         )
      ),#end conitional for chromsomeview (panel2)
      conditionalPanel(condition = "input.datatabs == 'Annot' || input.datatabs == 'panel2' || $('li.active a').first().html()==='Chromosome View'",
         wellPanel(
            h5("Annotation window options:"),
            h6("Click a point or type a basepair value:"),
            uiOutput("selectedOut"),
            uiOutput("windowOut")                        
         )
      ),#end conditional panel for Annotation plot and Table
      conditionalPanel(condition = "input.datatabs == 'Annot'",
         helpText(h5(p(paste("Download a CSV of the annotations in the selected window.")))),
         wellPanel(                        
           downloadButton('downloadAnnot','Download')
         )
      ),#end conditional panel for Annotation Table
      conditionalPanel(condition = "input.datatabs == 'panel1' || input.datatabs == 'panel2' || $('li.active a').first().html()==='Chromosome View'",      
        helpModal('Browser Help','browser',includeMarkdown("tools/manage.md"))        
      )#add help button for browser tabs
    )#end list
  }) #end ui_All
  outputOptions(output, "ui_All", suspendWhenHidden=FALSE)
  # find the appropriate UI
  output$ui_finder <- renderUI({
#    if(is.null(input$datatabs)){      
#      get("ui_All")()
#    }else{
#      get(paste0('ui_',input$datatabs))()
#    }
#     if(input$tool == "data") {
#       if(!is.null(input$datatabs)) get(paste0('ui_',input$datatabs))()
#     } else {
#       if(!is.null(input$tool)) get(paste0('ui_',input$tool))()
#     }
  })  
  outputOptions(output, "ui_finder", suspendWhenHidden=FALSE)
  output$datasets <- renderUI({   
    inFile <- input$uploadfile
    if(!is.null(inFile)) {
      # iterating through the files to upload
      isolate({
        for(i in 1:(dim(inFile)[1])) {
          loadUserData(inFile[i,'name'], inFile[i,'datapath'])
          # unlink(inFile[i,'datapath'], recursive = FALSE, force = TRUE)
        }
      })
      val <- values$datasetlist[1]
    }else{
      val <- "sigGWASsnpsCombinedIterations.longhorn.allLoc.csv"
    }
        
    # Drop-down selection of data set
    # selectInput(inputId = "datasets", label = "Datasets:", choices = datasets, selected = datasets[1], multiple = FALSE)
    selectInput(inputId = "datasets", label = "Datasets:", choices = values$datasetlist, selected = values$datasetlist[values$datasetlist==val], multiple = FALSE, selectize=FALSE)
  })
  
  reactiveAnnotTable <- reactive({
    if(is.null(input$datasets)) return()
    centerBP <- as.numeric(input$selected[[1]])
    winHigh <- centerBP+input$window[1]
    winLow <- centerBP-input$window[1]
    if(winLow < 0){winLow <- 0}    
    thisChrAnnot <- subset(annotGeneLoc[input$organism][[1]],chromosome==input$chr)    
    thisAnnot <- thisChrAnnot[thisChrAnnot$transcript_start >= winLow & thisChrAnnot$transcript_end <= winHigh,]    
    thisAnnot  
  })
  
  #Returns the nicely formatted preview table
  output$htmlDataExample <- reactive({
    if(is.null(input$datasets)) return()
    
    dat <- getdata()

    # necessary when deleting a dataset
    if(is.null(dat)) return()
    
    # Show only the first 25 rows
    nr <- min(25,nrow(dat))
    dat <- data.frame(dat[1:nr,, drop = FALSE])
    
    #dat <- date2character_dat(dat) #may be needed to print table if there is a data column
    
    html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
    html <- sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)
    Encoding(html) <- 'UTF-8'
    html
    
  })
  output$nrowDataset <- reactive({
    if(is.null(input$datasets)) return()
    dat <- getdata()
    if(is.null(dat)) return()
    nr <- nrow(dat)
    
    if(nr>2500){
      paste0('<p>First 25 rows shown of ',nr,' rows. See Data Table tab for details.<br>More than 2500 rows found, only the top 2500 will be plotted.</p>')
    }else{
      paste0('<p>First 25 rows shown of ',nr,' rows. See Data Table tab for details.</p>')
    }
  })
  output$ui_data_tabs <- renderUI({   # htmlOutput("htmlDataExample")})
    tabsetPanel(id = "datatabs",    type = "pills",  
      tabPanel(title="Manage",value="Manage",htmlOutput("htmlDataExample"),
              wellPanel(htmlOutput("nrowDataset")),
               tags$div(
                class = "container",
                
                #tags$p(tags$br()),
                row(
                  col(3, tags$br()),
                  col(7,  wellPanel(
                    p("This is a Shiny implementation originally adapted from the", a("Zbrowse", href="http://www.baxterlab.org/untitled-cqi0")," viewer created by the",a("Baxter laboratory", href="http://www.baxterlab.org/"), " intended to explore the genetic variation associated with the environment."),
                    p("Navigation through the graphs and tables in CLIMGeno is done using the tabs at the top of the page, adjusting options on the sidebar panel, or clicking points on the plots. The user interface is designed to be intuitive and allow the user
                      to quickly zoom into a point of interest anywhere on the genome."),
                    
                    p("To use this application choose the ExG association of interest from the left panel. The table on this tab provides a summary of the 25 variants with the strongest associations to the selected variable:"),
                    p("1. The Data Table tab provides a full description of the associated variants for the selected environmental variable. The user can easily manage the columns to be shown, rank based on score or q-value, or search and retrieve information for any associated variant. "),
                    p("2. The whole genome view tab provides an interactive Manhattan plot with the variants associated with the environmental variable of interest. The may manage the genetic variants on this plot based on its predicted effect (missense, synonymous variants, etc.). Scrolling over the variants in the plot retrieves the specific information on that variant. Clicking on it, automatically renders the plot shown in the next tab (chromosome view). Alternatively, the user can click and drag over a region of interest."),
                    p("3. In the chromosome view tab, the user obtains an amplified view of the genetic region of interest. Clicking any variant in the top plot will automatically render in the bottom plot, a window size determined by the user on the left panel. This feature allows for the exploration of the region within linkage disequilibrium for the selected variant. The user can then explore nearby genes as well as other variants associated with the environmental variable of interest within the same genetic region."),
                    p("4. The annotation table tab, provides an interactive table with the information in the genetic region selected in the previous tab. "),
                    p("This tool provides information on the q-values for all associated variants for the user to impose a particular FDR if desired. We recommend the exploration of the FDR parameters for these ExG association using our FDRCLIM tool."),
                    p("We recommend the user of CLIMGeno to become familiar with the limitations inherent to genome-wide association studies, for which a description is available in the left panel."),
                    p("For a more detailed description of the logic behind the Zbrowse viewer in this tool visit the" ,a("user manual", href="Zbrowse.pdf"), "by Greg Ziegler.")
                    
                    )),
                  
                  col(7, wellPanel(
                    p("-", a("Zbrowse", href="http://www.baxterlab.org/untitled-cqi0"), "Ziegler, Greg R., Ryan H. Hartsock, and Ivan Baxter. Zbrowse: an interactive GWAS results browser PeerJ Computer Science 1 (2015): e3."),
                    
                    
                    div(tags$a(img(src='shiny.png',  height="100px"),href="https://shiny.rstudio.com/"),
                        tags$a(img(src='rstudio.png',  height="100px"),href="https://www.rstudio.com/"),
                        tags$a(img(src='bioconductor.png',  height="100px"),href="https://www.bioconductor.org/"),
                        tags$a(img(src='1001.png',  height="100px"),href="http://1001genomes.org/"), 
                        tags$a(img(src='highcharts.png',  height="100px"),href="https://www.highcharts.com/"),
                        tags$a(img(src='rCharts.png',  height="100px"),href="https://ramnathv.github.io/rCharts/"),
                        align="middle", style="text-align: center;"),
                    
                    h3(''),
                    h3(''),
                    tags$a(div(img(src='climtools.png',  align="middle"), style="text-align: center;"), href="http://www.personal.psu.edu/sma3/CLIMtools.html"),
                    tags$a(div(img(src='climtools logo.png',  align="middle"), style="text-align: center;"), href="http://www.personal.psu.edu/sma3/CLIMtools.html"),
                    
                    h3(''),
                    tags$a(div(img(src='assmann_lab.png',  align="middle"), style="text-align: center;"), href="http://www.personal.psu.edu/sma3/index.html")
                    
                    
                  )),
                  col(7, h4('Select appropriate columns to be used for plotting.'))
                #HTML('<h4>Select appropriate columns to be used for plotting.</h4>'),
                ),
                tags$hr(),
                row(
                   #col(2, tags$br()),
                   col(2,uiOutput("chrColumn"),uiOutput("bpColumn")),
                   col(2,uiOutput("plotAll"),uiOutput("traitColumns")),
                   col(2,uiOutput("yAxisColumn"),uiOutput("logP")),
                   col(2,uiOutput("axisLimBool"),uiOutput("axisLim")),
                   col(2,actionButton("SubmitColsButton","Submit"))
                ),
                tags$hr(),
                row(
                  col(7, uiOutput("supportInterval"))#
                ),
                row(
                  col(2, uiOutput("SIbpStart")),
                  col(2, uiOutput("SIyAxisColumn")),
                  col(2, uiOutput("SIaxisLimBool"),uiOutput("SIaxisLim"))
                )
              )
#                   col(
#                     4,
#                     uiOutput("traitColumns"),
#                     uiOutput("yAxisColumn")
#                   )
#                 )                                                
# #               #HTML(dataDescriptionOutput())
#                )
      ),
      tabPanel(title="1. Data Table",value="Table",dataTableOutput("dataviewer")),
      tabPanel(title="2. Whole Genome View",value="panel1",showOutput("gChart", "highcharts")),#showOutput("gChart","highcharts"))
      tabPanel(title="3. Chromosome View",value="panel2",showOutput("pChart", "highcharts"),showOutput("zChart", "highcharts"),
               tags$script('Shiny.addCustomMessageHandler("customMsg", function(bandOpts){
                                            chartXAxis = $("#pChart").highcharts().xAxis[0]
                                            chartXAxis.removePlotBand()
                                            chartXAxis.addPlotBand(bandOpts)
                                          })'
               )),
      tabPanel(title="4. Annotations Table",value="Annot",dataTableOutput("annotViewer")),
tabPanel(title = "Description of climate variables",  mainPanel(fixedRow(
  width = 12,
  withSpinner(DT::dataTableOutput("a"))
)))

    )#end tabsetPanel
  })#end data tabs
  descriptiondataset <-read.csv("www/datadescription.csv")
  
  output$a <- DT::renderDataTable(descriptiondataset, filter = 'top', options = list(
    pageLength = 25, autoWidth = TRUE))
  outputOptions(output, "ui_data_tabs", suspendWhenHidden=FALSE)
  output$annotViewer <- renderDataTable({
#    if(is.null(input$datasets)) return()
#    centerBP <- as.numeric(input$selected[[1]])
#    winHigh <- centerBP+input$window[1]
#    winLow <- centerBP-input$window[1]
#    if(winLow < 0){winLow <- 0}    
#    thisChrAnnot <- subset(annotGeneLoc[input$organism][[1]],chromosome==input$chr)
#    thisAnnot <- thisChrAnnot[thisChrAnnot$transcript_start >= winLow & thisChrAnnot$transcript_end <= winHigh,]        
#    thisAnnot
    reactiveAnnotTable()
  }, options = list(orderClasses = TRUE, bCaseInsensitive = TRUE,
                    lengthMenu = c(15, 50, 100, 200, 500), pageLength = 15,
                    "dom" = 'T<"clear">lfrtip',
                    "oTableTools" = list(
                      "sSwfPath" = "/tabletools/swf/copy_csv_xls_pdf.swf",
                      "aButtons" = list(
                        "copy",
                        "print",
                        list("sExtends" = "collection",
                             "sButtonText" = "Save",
                             "aButtons" = c("csv","xls","pdf")
                        )
                      )
                    )                    
          )
  )#end annotation table
  output$dataviewer <-renderDataTable({    
    if(is.null(input$datasets) || is.null(input$columns)) return()
    
    dat <- getdata()
    #dat <- date2character()
    
    if(!all(input$columns %in% colnames(dat))) return()
    
    if(input$dv_select != '') {
      selcom <- input$dv_select
      selcom <- gsub(" ", "", selcom)
      
      seldat <- try(do.call(subset, list(dat,parse(text = selcom))), silent = TRUE)
      
      if(!is(seldat, 'try-error')) {
        if(is.data.frame(seldat)) {
          dat <- seldat
          seldat <- NULL
        }
      }
    }
    ##
    observe({
      lapply(input$columns, function(i){
        if("Select All" %in% input[[i]]){
          selected_choices <- sort(unique(values[[input$datasets]][,i]))
          updateSelectizeInput(session, i, selected = selected_choices)
        }
      })
    })
    
    dat <- data.frame(dat[, input$columns, drop = FALSE])
    dat
    
    # html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
    # html <- sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)
    # html
    
  }, options = list(orderClasses = TRUE, bCaseInsensitive = TRUE,
                    lengthMenu = c(15, 50, 100, 200, 500), pageLength = 15,
                      "dom" = 'T<"clear">lfrtip',
                      "oTableTools" = list(
                          "sSwfPath" = "/tabletools/swf/copy_csv_xls_pdf.swf",
                          "aButtons" = list(
                            "copy",
                            "print",
                            list("sExtends" = "collection",
                                "sButtonText" = "Save",
                                "aButtons" = c("csv","xls","pdf")
                              )
                         )
                    )                    
                )
  )#end dataviewer
  
  output$downloadAnnot <- downloadHandler(
    filename = function() {paste0("AnnotationsAround.chr",input$chr,".",input$selected[[1]],"bp.",input$organism,".csv")},
    content = function(file) {write.csv(reactiveAnnotTable(),file,row.names=F)}
  )
  

  
  output$columns <- renderUI({
    cols <- varnames()    
    lapply(input$traitColumns, function(i) {      
      traits <- c("Select All",sort(unique(values[[input$datasets]][,i])))
      selectizeInput(inputId=i, label=paste0("Select ",i),cols,
                     selected="Select All",
                     multiple=TRUE, options = list(dropdownParent="body",plugins=list("remove_button")))
    })
    selectizeInput("columns", "Click on columns to show/omit:", choices = as.list(cols), selected = c("Chr","MAF", "Position", "score","q_value", "MAF","MAC", "Locus_ID","Locus_description","REF","ALT","Effect"), multiple = TRUE, options = list(dropdownParent="body",plugins=list("remove_button")))
    
  })
  
  
  output$axisLimBool <- renderUI({
    if(is.null(input$datasets)){return()}
    if(input$datasets %in% datasetProp()$dataset){
      val = datasetProp()$axisLim[datasetProp()$dataset == input$datasets]
    }else{
      val = FALSE}
    checkboxInput('axisLimBool', 'Set Y-axis Limits?', val)
  })
  
  output$logP <- renderUI({
    if(is.null(input$datasets)){return()}
    if(input$datasets %in% datasetProp()$dataset){
      val = datasetProp()$logP[datasetProp()$dataset == input$datasets]
    }else{
      val = FALSE}
    checkboxInput('logP', 'Take -log10 of column?', val)
  })

  output$organism <- renderUI({
    if(is.null(input$datasets)){return()}
    cols <- varnames()
    if(input$datasets %in% datasetProp()$dataset){
      selected = datasetProp()$organism[datasetProp()$dataset == input$datasets]
    }else{
      selected = "Corn"
    }
    # = c("Corn","Soybean","Arabidopsis","Sorghum")
    choices<-list()
    files<-list.files(path="./organisms/")
    for(i in 1:length(files)){
      if(tools::file_ext(files[i]) == "txt"){
        filename=""
        filename=paste("./organisms/",files[i],sep="")
        conn=file(filename,open="r")
        data<-readLines(conn)
        choices[[i]] <- data[1]
        
        close(conn)
      }
    }
    selectizeInput("organism", "Dataset Organism:", unlist(choices), selected = "Arabidopsis", multiple = FALSE, options = list(dropdownParent="body"))
  })
  
  output$chrColumn <- renderUI({
    if(is.null(input$datasets)){return()}
    cols <- varnames()    
    if(input$datasets %in% datasetProp()$dataset){
      selected = cols[datasetProp()$chrColumn[datasetProp()$dataset == input$datasets]]
    }else{
      selected = names(cols[1])
    }    
    selectizeInput("chrColumn", "Chromosome Column:", choices = as.list(cols), selected = "Chr", multiple = FALSE, options = list(dropdownParent="body"))
  })

  output$bpColumn <- renderUI({
    if(is.null(input$datasets)){return()}
    cols <- varnames()
    if(input$datasets %in% datasetProp()$dataset){
      selected = cols[datasetProp()$bpColumn[datasetProp()$dataset == input$datasets]]
    }else{
      selected = names(cols[2])
    }
    #selectInput("bpColumn", "Base Pair Column:", choices = as.list(cols), selected = selected, multiple = FALSE, selectize = TRUE)
    selectizeInput("bpColumn", "Base Pair Column:", choices = as.list(cols), selected = "Position", multiple = FALSE, options = list(dropdownParent="body"))
  })
  
  output$traitColumns <- renderUI({
    if(is.null(input$datasets)){return()}
    cols <- varnames()
    if(input$datasets %in% datasetProp()$dataset){
      selected = cols[which(names(cols) %in% unlist(strsplit(datasetProp()$traitCol[datasetProp()$dataset == input$datasets],";")))]
    }else{
      selected = names(cols[3:4])
    }
    print(selected)
    conditionalPanel(condition = "input.plotAll==false",
                     selectizeInput("traitColumns", "Group by these trait column(s):", choices = as.list(cols), selected = "Effect", multiple = TRUE, options = list(dropdownParent="body"))
    )        
  })  
  #traits <- c("Select All",sort(unique(values[[input$datasets]][,i])))
  
  output$plotAll <- renderUI({
    if(is.null(input$datasets)){return()}
    if(input$datasets %in% datasetProp()$dataset){      
      val = datasetProp()$plotAll[datasetProp()$dataset == input$datasets]
    }else{
      val = FALSE
    }    
    checkboxInput('plotAll', 'All data is the same trait', val)    
  })
  
  output$yAxisColumn <- renderUI({
    if(is.null(input$datasets)){return()}
    cols <- varnames()       
    if(input$datasets %in% datasetProp()$dataset){      
      selected = cols[datasetProp()$yAxisColumn[datasetProp()$dataset == input$datasets]]
    }else{
      #selected = names(cols[10])
      selected = as.character(cols[10])
    }    
    selectizeInput("yAxisColumn", "Y-axis column:", choices = as.list(cols), selected = "score", multiple = FALSE, options = list(dropdownParent="body"))
  })
  outputOptions(output, "yAxisColumn", suspendWhenHidden=FALSE)
  
  output$axisLim <- renderUI({    
    if(is.null(input$datasets)){return()}
    if(input$datasets %in% datasetProp()$dataset){
      min = datasetProp()$axisMin[datasetProp()$dataset == input$datasets]
      max = datasetProp()$axisMax[datasetProp()$dataset == input$datasets]
    }else{
      min = 0
      max = 1
    }
    conditionalPanel(condition = "input.axisLimBool==true",
                     numericInput("axisMin","Min:",value=min),
                     numericInput("axisMax","Max:",value=max)
    )    
  })  

  output$supportInterval <- renderUI({
    if(is.null(input$datasets)){return()}
    if(input$datasets %in% datasetProp()$dataset){
      val = datasetProp()$supportInterval[datasetProp()$dataset == input$datasets]
    }else{
      val = FALSE}
    checkboxInput('supportInterval', 'Plot base pair intervals (e.g., Joint linkage support intervals)?', val)
  })

  output$SIbpStart <- renderUI({
    if(is.null(input$datasets)){return()}
    cols <- varnames()
    if(input$datasets %in% datasetProp()$dataset){
      selected = cols[datasetProp()$SIbpStart[datasetProp()$dataset == input$datasets]]
      selectedEnd = cols[datasetProp()$SIbpEnd[datasetProp()$dataset == input$datasets]]
    }else{
      selected = names(cols[2])
      selectedEnd = names(cols[2])
    }
    conditionalPanel(condition = "input.supportInterval==true",
        selectizeInput("SIbpStart", "Interval Base Pair Start:", choices = as.list(cols), selected = selected, multiple = FALSE, options = list(dropdownParent="body")),
        selectizeInput("SIbpEnd", "Interval Base Pair End:", choices = as.list(cols), selected = selectedEnd, multiple = FALSE, options = list(dropdownParent="body"))
    )
  })   

  output$SIyAxisColumn <- renderUI({
    if(is.null(input$datasets)){return()}
    cols <- varnames()       
    if(input$datasets %in% datasetProp()$dataset){  
      selected = cols[datasetProp()$SIyAxisColumn[datasetProp()$dataset == input$datasets]]
    }else{
      #selected = names(cols[10])
      selected = as.character(cols[10])
    }        
    conditionalPanel(condition = "input.supportInterval==true",                     
                     selectizeInput("SIyAxisColumn", "Support Interval Y-axis column:", choices = as.list(cols), selected = selected, multiple = FALSE, options = list(dropdownParent="body"))
    )
  })
  outputOptions(output, "SIyAxisColumn", suspendWhenHidden=FALSE)  

  output$SIaxisLimBool <- renderUI({
    if(is.null(input$datasets)){return()}
    if(input$datasets %in% datasetProp()$dataset){
      val = datasetProp()$SIaxisLimBool[datasetProp()$dataset == input$datasets]
    }else{
      val = TRUE
    }
    conditionalPanel(condition = "input.supportInterval==true", 
      checkboxInput('SIaxisLimBool', 'Set Support Interval Y-axis Limits?', val)
    )
  })  

  output$SIaxisLim <- renderUI({    
    if(is.null(input$datasets)){return()}
    if(input$datasets %in% datasetProp()$dataset){
      min = datasetProp()$axisMin[datasetProp()$dataset == input$datasets]
      max = datasetProp()$axisMax[datasetProp()$dataset == input$datasets]
    }else{
      min = 0
      max = 1
    }
    conditionalPanel(condition = "input.supportInterval==true && input.SIaxisLimBool==true",
                     numericInput("SIaxisMin","Min:",value=min),
                     numericInput("SIaxisMax","Max:",value=max)
    )    
  })  

  #builds list of multiple selection boxes for traits that have multiple columns in dataset
  output$traitColBoxes <- renderUI({
    if(is.null(input$datasets)){return()}
    if(input$plotAll == TRUE){return()}
    lapply(input$traitColumns, function(i) {      
      traits <- c("Select All",sort(unique(values[[input$datasets]][,i])))
      selectizeInput(inputId=i, label=paste0("Select ",i),traits,
                  selected="Select All",
                  multiple=TRUE, options = list(dropdownParent="body",plugins=list("remove_button")))
    })
  })
  
  observe({
    lapply(input$traitColumns, function(i){
      if("Select All" %in% input[[i]]){
        selected_choices <- sort(unique(values[[input$datasets]][,i]))
        updateSelectizeInput(session, i, selected = selected_choices)
      }
    })
  })
  
  #checkbox to suppress plot legend
  output$legend <- renderUI({
    if(is.null(input$datasets)){return()}
    checkboxInput('legend', 'Suppress Legend', FALSE)
  })
  outputOptions(output, "legend", suspendWhenHidden=FALSE)
  
  #checkbox for whether to filter for only overlapping SNPs
  output$overlaps <- renderUI({
    if(is.null(input$datasets)){return()}
#    if(input$plotAll == TRUE){return()}
    checkboxInput('overlaps', 'Show only overlapping SNPs', FALSE)
  })  
  outputOptions(output, "overlaps", suspendWhenHidden=FALSE)

  #how many traits must overlap to be included in output, 1 means traits that overlap with themselves will be included
  output$numOverlapTraits <- renderUI({
    numericInput("numOverlaps", "Minimum number of overlaps?", value=2,min=1)    
  })

  #how big is the window when calculating whether two snps overlap
  output$overlapSize <- renderUI({
    numericInput(inputId="overlapSize",label="Overlap size around each point:",min=1,max=.5e6,value=10000)
  })

  output$selectChr <- renderUI({
    if(is.null(input$organism)){return()}
    selectInput("chr", "Chromosome:",chrName[input$organism][[1]],selectize = FALSE)
    #selectInput("chr", "Chromosome:",1:length(chrSize[input$organism][[1]]),selectize = FALSE)
  })
  outputOptions(output, "selectChr", suspendWhenHidden=FALSE)
  
  output$selectedOut <- renderUI({
    numericInput("selected", "", value=100000)
  })
  outputOptions(output, "selectedOut", suspendWhenHidden=FALSE)
  output$windowOut <- renderUI({
    #sliderInput(inputId="window",label="Window size around selected point:",min=-1e6,max=1e6,value=c(-7.5e5,7.5e5))
    sliderInput(inputId="window",label="Window size around selected point:",min=1000,max=.5e6,value=2.5e5)
  })
  outputOptions(output, "windowOut", suspendWhenHidden=FALSE)
  
  #returns datasets from uploaded file
  getdata <- reactive({
    if(is.null(input$datasets)){return()}
    values[[input$datasets]]
  })
  
  #builds list of column names and type in dataset
  varnames <- reactive({
    if(is.null(input$datasets)) return()
    dat <- getdata_class()
    vars <- names(dat)
    names(vars) <- paste(vars, " {", dat, "}", sep = "")
    vars
  })
  
  getdata_class <- reactive({
    if(is.null(input$datasets)) return()
    cls <- sapply(getdata(), function(x) class(x)[1])
    gsub("ordered","factor", cls)
  })
  
  #Function to handle loading of data from a file or rObject
  loadUserData <- function(filename, uFile) {  
    ext <- file_ext(filename)
    # objname <- robjname <- sub(paste(".",ext,sep = ""),"",basename(filename))
    objname <- sub(paste(".",ext,sep = ""),"",basename(filename))
    ext <- tolower(ext)
    
    if(ext == 'rda' || ext == 'rdata') {
      # objname will hold the name of the object(s) inside the R datafile
      robjname <- load(uFile)
      
      if(length(robjname) > 1) {
        
        #values[[objname]] <- data.frame(get(robjname[-which(robjname == "description")]))
        values[[objname]] <- data.frame(get(robjname[1]))
        #values[[paste0(objname,"_descr")]] <- get("description")
        
      } else {
        
        values[[objname]] <- data.frame(get(robjname))  # only work with data.frames
      }
    }
    
    if(length(values[['datasetlist']]) == 0 || values[['datasetlist']][1] == '') {
      values[['datasetlist']] <- c(objname)
    } else {
      values[['datasetlist']] <- unique(c(objname,values[['datasetlist']]))
    }
    
    if(ext == 'sav') {
      values[[objname]] <- as.data.frame(as.data.set(spss.system.file(uFile)))
    } else if(ext == 'dta') {
      values[[objname]] <- read.dta(uFile)
    } else{
      values[[objname]] <- read.csv(uFile, header=input$header, sep=input$sep,stringsAsFactors=FALSE)
    }
  }
  
  #add a totalBP column to an input dataset if not already present
  calculateTotalBP <- reactive({ 
    if("totalBP" %in% colnames(values[[input$datasets]])){
      
    }else{
   
      cumBP<-c(0,cumsum(as.numeric(chrSize[input$organism][[1]])))
      #to order by desired chromosome add factor levels in the desired order to the chrColumn, any chr names that differ in gwas file compared
      #to organism file will turn into NA
      values[[input$datasets]][,input$chrColumn] <- factor(values[[input$datasets]][,input$chrColumn],levels=chrName[input$organism][[1]])
      values[[input$datasets]] <- values[[input$datasets]][order(values[[input$datasets]][,input$chrColumn],values[[input$datasets]][,input$bpColumn]),]
      numeachchr<-aggregate(values[[input$datasets]][,input$bpColumn],list(values[[input$datasets]][,input$chrColumn]),length)
#      adjust<-rep(cumBP[1],numeachchr$x[numeachchr$Group.1==1])            
      adjust <- numeric()
      for (i in 1:(length(cumBP)-1)){#max(unique(values[[input$datasets]][,input$chrColumn]))){
        if(length(numeachchr$x[numeachchr$Group.1==chrName[input$organism][[1]][i]])==0){next;}
        adjust<-c(adjust,rep(cumBP[i],numeachchr$x[numeachchr$Group.1==chrName[input$organism][[1]][i]]))
      }
      #newval <- values[[input$datasets]][600,input$bpColumn]+adjust[600]      
      values[[input$datasets]]$totalBP <- values[[input$datasets]][,input$bpColumn]+adjust
      
      #values[[input$datasets]] <- adply(values[[input$datasets]],1,function(x){data.frame(totalBP=sum(x[[input$bpColumn]],chrSize$bp[chrSize$chr %in% if(x[[input$chrColumn]]==1) 0 else c(1:(x[[input$chrColumn]]-1))]))})
    }
   if(input$supportInterval == TRUE){
      if("SIbpStartTotal" %in% colnames(values[[input$datasets]])){
        
      }else{
         
        cumBP<-c(0,cumsum(as.numeric(chrSize[input$organism][[1]])))
        values[[input$datasets]][,input$chrColumn] <- factor(values[[input$datasets]][,input$chrColumn],levels=chrName[input$organism][[1]])
        values[[input$datasets]] <- values[[input$datasets]][order(values[[input$datasets]][,input$chrColumn],values[[input$datasets]][,input$SIbpStart]),]
        numeachchr<-aggregate(values[[input$datasets]][,input$SIbpStart],list(values[[input$datasets]][,input$chrColumn]),length)
        adjust <- numeric()
        for (i in 1:(length(cumBP)-1)){#max(unique(values[[input$datasets]][,input$chrColumn]))){
          if(length(numeachchr$x[numeachchr$Group.1==chrName[input$organism][[1]][i]])==0){next;}
          adjust<-c(adjust,rep(cumBP[i],numeachchr$x[numeachchr$Group.1==chrName[input$organism][[1]][i]]))
        }
        values[[input$datasets]]$SIbpStartTotal <- values[[input$datasets]][,input$SIbpStart]+adjust    
      }
    
      if("SIbpEndTotal" %in% colnames(values[[input$datasets]])){
        
      }else{
        
        cumBP<-c(0,cumsum(as.numeric(chrSize[input$organism][[1]])))
        values[[input$datasets]][,input$chrColumn] <- factor(values[[input$datasets]][,input$chrColumn],levels=chrName[input$organism][[1]])
        values[[input$datasets]] <- values[[input$datasets]][order(values[[input$datasets]][,input$chrColumn],values[[input$datasets]][,input$SIbpEnd]),]
        numeachchr<-aggregate(values[[input$datasets]][,input$SIbpEnd],list(values[[input$datasets]][,input$chrColumn]),length)
        adjust <- numeric()
        for (i in 1:(length(cumBP)-1)){#max(unique(values[[input$datasets]][,input$chrColumn]))){
          if(length(numeachchr$x[numeachchr$Group.1==chrName[input$organism][[1]][i]])==0){next;}
          adjust<-c(adjust,rep(cumBP[i],numeachchr$x[numeachchr$Group.1==chrName[input$organism][[1]][i]]))
        }
        values[[input$datasets]]$SIbpEndTotal <- values[[input$datasets]][,input$SIbpEnd]+adjust    
      }
    } #end SI total bp calculation
  })#end calculateTotalBP
  
  output$pChart <- renderChart2({
    #this function makes the chromsomeview chart  
    #subset whole chart based on selection
    chromChart <- values[[input$datasets]]
    chromChart <- chromChart[chromChart[,input$chrColumn]==input$chr,]
    
    if(input$plotAll==FALSE){
      for(i in input$traitColumns){
        chromChart <- chromChart[chromChart[,i] %in% input[[i]],]
      }    
      if(length(input$traitColumns) > 1){
        chromChart$trait <- do.call(paste,c(chromChart[,input$traitColumns],sep="_"))    
      }else{
        chromChart$trait <- chromChart[,input$traitColumns]
      }
    }else{
      chromChart$trait <- input$datasets
    }
            
    #Separate Support Interval data from GWAS data, if support, GWAS data is assumed to be anything that has an NA in the SIbpStart column
    if(input$supportInterval == TRUE){
      SIchart <- chromChart[!(is.na(chromChart[,input$SIbpStart])),]
      chromChart <- chromChart[is.na(chromChart[,input$SIbpStart]),]
    }        
    #check if there is any data for the selected traits
    chromChart <- chromChart[!(is.na(chromChart[,input$bpColumn])),]
    chromChart <- chromChart[!(is.na(chromChart[,input$yAxisColumn])),]
    
    #if checked, filter for only overlapping SNPs
    if(!is.null(input$overlaps) & input$overlaps == TRUE){
      chromChart <- findGWASOverlaps(chromChart)
    }            
    
    if(nrow(chromChart)==0){ #nothing is in the window, but lets still make a data.frame
      chromChart <- values[[input$datasets]][1,]
      chromChart[,input$yAxisColumn] <- -1    
      if(length(input$traitColumns) > 1){
        chromChart$trait <- do.call(paste,c(chromChart[,input$traitColumns],sep="_"))    
      }else{
        chromChart$trait <- chromChart[,input$traitColumns]
      }             
    }    
    colorTable <- colorTable() 
    
    #take -log10 of y-axis column if requested
    if(input$logP == TRUE && chromChart[1,input$yAxisColumn] != -1){
      chromChart[,input$yAxisColumn] <- -log(chromChart[,input$yAxisColumn],10)
    }        
    
    #check if there is too much data (>2500 data points), trim to 2500
    if(nrow(chromChart)>2500){
      cutVal <- sort(chromChart[,input$yAxisColumn],decreasing = T)[2500]
      chromChart <- chromChart[chromChart[,input$yAxisColumn] >= cutVal,]
    }        
    
    #calculate window for plotband
    pbWin <- isolate({
      center <- as.numeric(input$selected[[1]])
      winHigh <- center + input$window[1]
      winLow <- center - input$window[1]
      list(winLow=winLow,winHigh=winHigh)
    })    
    
    pkTable <- data.frame(x=chromChart[,input$bpColumn],y=chromChart[,input$yAxisColumn],trait=chromChart$trait,
                          #                          name=sprintf("<table cellpadding='4' style='line-height:1.5'><tr><th>%1$s</th></tr><tr><td align='left'>Y-value: %2$s<br>Location: %3$s<br>Base Pair: %4$s<br>SNP: %5$s<br>Chromosome: %6$s</td></tr></table>",
                          #name=sprintf("<table cellpadding='4' style='line-height:1.5'><tr><th>%1$s</th></tr><tr><td align='left'>Y-value: %2$s<br>Base Pairs: %3$s<br>Chromosome: %4$s</td></tr></table>",
                          name=sprintf("Base Pair: %1$s<br/>Chromosome: %2$s<br/>",
#                                       chromChart$trait,
#                                       chromChart[,input$yAxisColumn],
                                       #                                       pk$loc,
                                       prettyNum(chromChart[,input$bpColumn], big.mark = ","),
                                       #                                       pk$SNP,
                                       chromChart[,input$chrColumn]
                          ),
                          url="http://danforthcenter.org",
                          chr=chromChart[,input$chrColumn],
                          bp=chromChart[,input$bpColumn],stringsAsFactors=FALSE)
    pkSeries <- lapply(split(pkTable, pkTable$trait), function(x) {
      res <- lapply(split(x, rownames(x)), as.list)
      names(res) <- NULL
      res <- res[order(sapply(res, function(x) x$x))]
      return(res)
    })

    #build JL series
    if(input$supportInterval==TRUE){
      if(nrow(SIchart)==0){ #nothing is in the window, but lets still make a data.frame
        SIchart <- values[[input$datasets]][1,]
        SIchart[,input$SIyAxisColumn] <- -1    
        if(length(input$traitColumns) > 1){
          SIchart$trait <- do.call(paste,c(SIchart[,input$traitColumns],sep="_"))    
        }else{
          SIchart$trait <- SIchart[,input$traitColumns]
        }             
      }
      SIchart$loc_el <- SIchart$trait
      SIchart$trait <- paste(SIchart$trait,"Int",sep="_")
      SIchart <- SIchart[order(SIchart[[input$SIbpStart]]),]
      jlTable <- adply(SIchart,1,function(x) {data.frame(x=c(x[[input$SIbpStart]],x[[input$SIbpEnd]],x[[input$SIbpEnd]]),y=c(x[[input$SIyAxisColumn]],x[[input$SIyAxisColumn]],NA),trait=x$trait,
                                                         name=sprintf("<table cellpadding='4' style='line-height:1.5'><tr><td align='left'>Interval: %1$s-%2$s<br>Chromosome: %3$s</td></tr></table>",
#                                                                      x$trait,
#                                                                      x[[input$SIyAxisColumn]],
                                                                      prettyNum(x[[input$SIbpStart]], big.mark = ","),
                                                                      prettyNum(x[[input$SIbpEnd]], big.mark = ","),
                                                                      x[[input$chrColumn]]
                                                         ),loc_el=x$loc_el,bp=x[[input$bpColumn]],chr=x[[input$chrColumn]],stringsAsFactors=FALSE
                                                         #                                                   
                                                         #                                                   totalBP=x$totalBP,
                                                         #                                                   chr=x$Chromosome,stringsAsFactors=FALSE
      )}#end jlTable and function
      )#end adply
      jlTable <- jlTable[,c("x","y","trait","name","loc_el","bp","chr")]
      #jlTable <- jlTable[order(jlTable$x),]
    }#end build jlTable if support intervals        
    
    a <- rCharts::Highcharts$new()
    a$LIB$url <- 'highcharts/' #use the local copy of highcharts, not the one installed by rCharts
    a$xAxis(title = list(text = "Base Pairs"),startOnTick=TRUE,min=1,max=chrSize[input$organism][[1]][as.numeric(input$chr)],endOnTick=FALSE,
            plotBands = list(list(from=pbWin$winLow,to=pbWin$winHigh,color='rgba(68, 170, 213, 0.4)')))
#    a$xAxis(title = list(text = "Base Pairs"),startOnTick=TRUE,min=1,max=30000000,endOnTick=TRUE)
    
    if(input$axisLimBool == TRUE){
      a$yAxis(title=list(text=input$yAxisColumn),min=input$axisMin,max=input$axisMax,startOnTick=FALSE)
    }else{
      a$yAxis(title=list(text=input$yAxisColumn),startOnTick=FALSE)      
    }    

    if(input$supportInterval==TRUE){
      if(input$SIaxisLimBool == TRUE){
        a$yAxis(title=list(text=input$SIyAxisColumn),min=input$SIaxisMin,max=input$SIaxisMax,gridLineWidth=0,minorGridLineWidth=0,startOnTick=FALSE,opposite=TRUE,replace=FALSE)
      }else{
        a$yAxis(title=list(text=input$SIyAxisColumn),gridLineWidth=0,minorGridLineWidth=0,startOnTick=FALSE,opposite=TRUE,replace=FALSE)   
      }
      
      if(SIchart[1,input$SIyAxisColumn] != -1){
        d_ply(jlTable,.(trait),function(x){
          a$series(
            data = toJSONArray2(x,json=F,names=T),
            type = "line",
            name = unique(x$trait),
            yAxis=1,           
            color = colorTable$color[colorTable$trait == as.character(unique(x$loc_el))])})            
      }
    }

   if(chromChart[1,input$yAxisColumn] != -1){    
      invisible(sapply(pkSeries, function(x) {if(length(x)==0){return()};a$series(data = x, type = "scatter", turboThreshold=5000, name = paste0(x[[1]]$trait), color = colorTable$color[colorTable$trait == as.character(x[[1]]$trait)])}))
    }
    a$chart(zoomType="xy", alignTicks=FALSE,events=list(click = "#!function(event) {this.tooltip.hide();}!#"))
    a$title(text=paste(input$datasets,"Results for Chromosome",input$chr,sep=" "))
    a$subtitle(text="Rollover for more info. Drag chart area to zoom. Click point for zoomed annotated plot.")
    
    a$plotOptions(
      scatter = list(
        cursor = "pointer",

        point = list(
          events = list(

            click = "#! function(){$('input#selected').val(this.options.bp); $('input#selected').trigger('change');} !#")),
        marker = list(
          symbol = "circle",
          radius = 5
#          states = list(hover = list(enabled = TRUE))
        ),
        tooltip = list(
          headerFormat = "<b>{series.name}</b><br/>{point.key}<br/>Y-value: {point.y}<br/>",
          pointFormat = "",
          followPointer = TRUE
        )

      ),
      line = list(
        lineWidth = 10,
        dashStyle = 'Solid',
        cursor = "pointer",
        point = list(
          events = list(

            click = "#! function(){$('input#selected').val(this.options.bp); $('input#selected').trigger('change');} !#")),
        marker = list(
          enabled = FALSE,
          states = list(hover = list(enabled=FALSE))
        )
      ),
      spline = list(
        lineWidth = 3,
        cursor = "pointer"
      )
    )
    #a$tooltip(useHTML = T, formatter = "#! function() { return this.point.name; } !#")
    #a$tooltip(snap = 5, useHTML = T, formatter = "#! function() { return this.point.name; } !#")
    a$exporting(enabled=TRUE,filename='chromChart',sourceWidth=2000)
    a$set(dom = 'pChart')
    return(a)
    
  })#end pchart
    
  #Genome wide chart
  output$gChart <- renderChart2({
       
    calculateTotalBP()

    #subset whole chart based on selection
    genomeChart <- values[[input$datasets]]
    if(input$plotAll == FALSE){
      for(i in input$traitColumns){
        genomeChart <- genomeChart[genomeChart[,i] %in% input[[i]],]
      }    
      if(length(input$traitColumns) > 1){
        genomeChart$trait <- do.call(paste,c(genomeChart[,input$traitColumns],sep="_"))    
      }else{
        genomeChart$trait <- genomeChart[,input$traitColumns]
      }
    }else{
        genomeChart$trait <- input$datasets
    }
    
    #Separate Support Interval data from GWAS data, if support, GWAS data is assumed to be anything that has an NA in the SIbpStart column
    if(input$supportInterval == TRUE){
      SIchart <- genomeChart[!(is.na(genomeChart[,input$SIbpStart])),]
      genomeChart <- genomeChart[is.na(genomeChart[,input$SIbpStart]),]
    }
    
    #filter genomeChart for only rows that have a base pair and yaxis value
    genomeChart <- genomeChart[!(is.na(genomeChart[,input$bpColumn])),]
    genomeChart <- genomeChart[!(is.na(genomeChart[,input$yAxisColumn])),]
    
    #if checked, filter for only overlapping SNPs
    if(!is.null(input$overlaps) & input$overlaps == TRUE){
      genomeChart <- findGWASOverlaps(genomeChart)
    }
    
    #check if there is any data for the selected traits
    if(nrow(genomeChart)==0){ #nothing is in the window, but lets still make a data.frame
       genomeChart <- values[[input$datasets]][1,]
       genomeChart[,input$yAxisColumn] <- -1    
       if(length(input$traitColumns) > 1){
         genomeChart$trait <- do.call(paste,c(genomeChart[,input$traitColumns],sep="_"))    
       }else{
         genomeChart$trait <- genomeChart[,input$traitColumns]
       }             
    }
    
    #take -log10 of y-axis column if requested
    if(input$logP == TRUE && genomeChart[1,input$yAxisColumn] != -1){
      genomeChart[,input$yAxisColumn] <- -log(genomeChart[,input$yAxisColumn],10)
    }
    
    #check if there is too much data (>2500 data points), trim to 2500
    if(nrow(genomeChart)>2500){
      cutVal <- sort(genomeChart[,input$yAxisColumn],decreasing = T)[2500]
      genomeChart <- genomeChart[genomeChart[,input$yAxisColumn] >= cutVal,]
    }
    
    colorTable <- colorTable()
     genomeTable <- data.frame(x=genomeChart$totalBP,y=genomeChart[,input$yAxisColumn],trait=genomeChart$trait,
                                name=sprintf("Base Pair: %1$s<br/>Chromosome: %2$s<br/>",

                                            prettyNum(genomeChart[,input$bpColumn], big.mark = ","),
                                            genomeChart[,input$chrColumn]
                               ),
                               url="http://danforthcenter.org",
                               chr=genomeChart[,input$chrColumn],
                               bp=genomeChart[,input$bpColumn],stringsAsFactors=FALSE)
     genomeSeries <- lapply(split(genomeTable, genomeTable$trait), function(x) {
       res <- lapply(split(x, rownames(x)), as.list)
       names(res) <- NULL
       res <- res[order(sapply(res, function(x) x$x))]
       return(res)
     })
#     
      #build JL series
    if(input$supportInterval==TRUE){
      if(nrow(SIchart)==0){ #nothing is in the window, but lets still make a data.frame
        SIchart <- values[[input$datasets]][1,]
        SIchart[,input$SIyAxisColumn] <- -1    
        if(length(input$traitColumns) > 1){
          SIchart$trait <- do.call(paste,c(SIchart[,input$traitColumns],sep="_"))    
        }else{
          SIchart$trait <- SIchart[,input$traitColumns]
        }             
      }
      SIchart$loc_el <- SIchart$trait
      SIchart$trait <- paste(SIchart$trait,"Int",sep="_")
      SIchart <- SIchart[order(SIchart$SIbpStartTotal),]
       jlTable <- adply(SIchart,1,function(x) {data.frame(x=c(x$SIbpStartTotal,x$SIbpEndTotal,x$SIbpEndTotal),y=c(x[[input$SIyAxisColumn]],x[[input$SIyAxisColumn]],NA),trait=x$trait,
                                                    #name=sprintf("<table cellpadding='4' style='line-height:1.5'><tr><th>%1$s</th></tr><tr><td align='left'>Y-value: %2$.2f <br>Interval: %3$s-%4$s<br>Chromosome: %5$s</td></tr></table>",
                                                    name=sprintf("<table cellpadding='4' style='line-height:1.5'><tr><td align='left'>Interval: %1$s-%2$s<br>Chromosome: %3$s</td></tr></table>",
#                                                                 x$trait,
#                                                                 x[[input$SIyAxisColumn]],
                                                                 prettyNum(x[[input$SIbpStart]], big.mark = ","),
                                                                 prettyNum(x[[input$SIbpEnd]], big.mark = ","),
                                                                 x[[input$chrColumn]]
                                                    ),loc_el=x$loc_el,bp=x[[input$bpColumn]],chr=x[[input$chrColumn]],stringsAsFactors=FALSE
  #                                                   
  #                                                   totalBP=x$totalBP,
  #                                                   chr=x$Chromosome,stringsAsFactors=FALSE
         )}#end jlTable and function
       )#end adply
       jlTable <- jlTable[,c("x","y","trait","name","loc_el","bp","chr")]
       #jlTable <- jlTable[order(jlTable$x),]
    }#end build jlTable if support intervals

    #build list for where to put plotbands for this organism
    bigList <- list()
    cumBP<-c(0,cumsum(as.numeric(chrSize[input$organism][[1]])))
    for(i in 1:(length(cumBP)-1)){
      if(i %% 2 == 0 ){ #even
        bigList[[length(bigList)+1]] <- list(from=cumBP[i]+1,to=cumBP[i+1],label=list(text=chrName[input$organism][[1]][i],style=list(color="#6D869F"),verticalAlign="bottom"))
      }else{ #odd
        bigList[[length(bigList)+1]] <- list(from=cumBP[i]+1,to=cumBP[i+1],color='rgba(68, 170, 213, 0.1)',label=list(text=chrName[input$organism][[1]][i],style=list(color="#6D869F"),verticalAlign="bottom"))
      }
    }    
    
     c <- rCharts::Highcharts$new()
    c$LIB$url <- 'highcharts/'
    c$xAxis(title = list(text = "Chromosome",margin=15),startOnTick=TRUE,min=0,max=sum(as.numeric(chrSize[input$organism][[1]])),endOnTick=FALSE,labels=list(enabled=FALSE),tickWidth=0,
            plotBands = bigList)   
    
     if(input$axisLimBool == TRUE){       
       c$yAxis(title=list(text=input$yAxisColumn),min=input$axisMin,max=input$axisMax,startOnTick=FALSE)
     }else{
       c$yAxis(title=list(text=input$yAxisColumn),startOnTick=FALSE)      
     }
     
     if(input$supportInterval==TRUE){
       if(input$SIaxisLimBool == TRUE){
         c$yAxis(title=list(text=input$SIyAxisColumn),min=input$SIaxisMin,max=input$SIaxisMax,gridLineWidth=0,minorGridLineWidth=0,startOnTick=FALSE,opposite=TRUE,replace=FALSE)   
       }else{
         c$yAxis(title=list(text=input$SIyAxisColumn),gridLineWidth=0,minorGridLineWidth=0,startOnTick=FALSE,opposite=TRUE,replace=FALSE)
       }
       
       if(SIchart[1,input$SIyAxisColumn] != -1){
         d_ply(jlTable,.(trait),function(x){
           c$series(
             data = toJSONArray2(x,json=F,names=T),
             type = "line",
             name = unique(x$trait),
             dashStyle = 'Solid',
             marker = list(enabled=F),
             yAxis=1,           
             color = colorTable$color[colorTable$trait == as.character(unique(x$loc_el))])})            
       }
     }
     if(genomeChart[1,input$yAxisColumn] != -1){
       invisible(sapply(genomeSeries, function(x) {if(length(x)==0){return()};c$series(data = x, turboThreshold=5000,type = "scatter", color = colorTable$color[colorTable$trait == as.character(x[[1]]$trait)], name = paste0(x[[1]]$trait))}))
     }
     
     c$chart(zoomType="xy",alignTicks=FALSE,events=list(click = "#!function(event) {this.tooltip.hide();}!#"))
     c$title(text=paste(input$datasets," Results",sep=" "))
     c$subtitle(text="Rollover for more info. Drag chart area to zoom. Click point to switch to chromosome and annotation view.")
     
     c$plotOptions(
        scatter = list(
          cursor = "pointer",
          point = list(
            events = list(

              click = "#! function(){$('select#chr').val(this.options.chr); $('select#chr').trigger('change'); $('input#selected').val(this.options.bp); 
                                     $('input#selected').trigger('change'); $('ul#datatabs li').eq(0).removeClass('active'); 
                                     $('ul#datatabs li').eq(1).removeClass('active'); $('ul#datatabs li').eq(2).removeClass('active');
                                     $('ul#datatabs li').eq(4).removeClass('active');
                                     $('ul#datatabs li').eq(3).addClass('active'); 
                                     $('#pChart').trigger('change');$('#pChart').trigger('shown');
                                     $('.tab-content div').toggleClass(function(){if(this.getAttribute('data-value')=='panel2' || this.getAttribute('data-value')=='panel1'){return 'active';}else{return '';}});
                                     $('.tab-content div').trigger('change');$('ul#datatabs li').trigger('change');} !#")), 
          marker = list(
            symbol = "circle",
            radius = 5
          ),
          tooltip = list(
            headerFormat = "<b>{series.name}</b><br/>{point.key}<br/>Y-value: {point.y}<br/>",
            pointFormat = "",
            followPointer = TRUE
          )
        ),
        line = list(
          lineWidth = 10,
          cursor = "pointer",
          point = list(
            events = list(

             click = "#! function(){$('select#chr').val(this.options.chr); $('select#chr').trigger('change'); $('input#selected').val(this.options.bp); 
                                    $('input#selected').trigger('change'); $('ul#datatabs li').eq(0).removeClass('active'); 
                                    $('ul#datatabs li').eq(1).removeClass('active'); $('ul#datatabs li').eq(2).removeClass('active');
                                    $('ul#datatabs li').eq(4).removeClass('active');
                                    $('ul#datatabs li').eq(3).addClass('active'); 
                                    $('#pChart').trigger('change');$('#pChart').trigger('shown');
                                    $('.tab-content div').toggleClass(function(){if(this.getAttribute('data-value')=='panel2' || this.getAttribute('data-value')=='panel1'){return 'active';}else{return '';}});
                                    $('.tab-content div').trigger('change');$('ul#datatabs li').trigger('change');} !#")),             
          marker = list(
            enabled = FALSE,
            states = list(hover = list(enabled=FALSE)),
            symbol = "square"
          )
        )            
      )#end plotOptions        

     c$exporting(enabled=TRUE,filename='genomeChart',sourceWidth=2000)
     if(!is.null(input$legend) & input$legend == TRUE){
       c$legend(enabled=FALSE)
     }
     
     c$credits(enabled=TRUE)
     c$set(dom = 'gChart')     
     return(c)
  
   })#end gchart


  output$zChart <- renderChart2({
    if(is.null(input$selected)) return()

    centerBP <- as.numeric(input$selected[[1]])
    winHigh <- centerBP+input$window[1]
    winLow <- centerBP-input$window[1]
    if(winLow < 0){winLow <- 0}
    
    zoomChart <- values[[input$datasets]]
    zoomChart <- zoomChart[zoomChart[,input$chrColumn]==input$chr,]    
    
    if(input$plotAll == FALSE){
      for(i in input$traitColumns){
        zoomChart <- zoomChart[zoomChart[,i] %in% input[[i]],]
      }
      
      if(length(input$traitColumns) > 1){
        zoomChart$trait <- do.call(paste,c(zoomChart[,input$traitColumns],sep="_"))    
      }else{
        zoomChart$trait <- zoomChart[,input$traitColumns]
      }
    }else{
      zoomChart$trait <- input$datasets
    }
    
    #Separate Support Interval data from GWAS data, if support, GWAS data is assumed to be anything that has an NA in the SIbpStart column
    if(input$supportInterval == TRUE){
      SIchart <- zoomChart[!(is.na(zoomChart[,input$SIbpStart])),]
      zoomChart <- zoomChart[is.na(zoomChart[,input$SIbpStart]),]
    }    
    
    zoomChart <- zoomChart[(zoomChart[,input$bpColumn] <= winHigh) & (zoomChart[,input$bpColumn] >= winLow),]    
    
    #filter for only rows that have a base pair value
    zoomChart <- zoomChart[!(is.na(zoomChart[,input$bpColumn])),]
    zoomChart <- zoomChart[!(is.na(zoomChart[,input$yAxisColumn])),]    
    
    #if checked, filter for only overlapping SNPs
    if(!is.null(input$overlaps) & input$overlaps == TRUE){
      zoomChart <- findGWASOverlaps(zoomChart)
    }                    
    
    if(nrow(zoomChart)==0){ #nothing is in the window, but lets still make a data.frame
      zoomChart <- values[[input$datasets]][1,]
      zoomChart[,input$yAxisColumn] <- -1    
      if(length(input$traitColumns) > 1){
        zoomChart$trait <- do.call(paste,c(zoomChart[,input$traitColumns],sep="_"))    
      }else{
        zoomChart$trait <- zoomChart[,input$traitColumns]
      }                   
    }
    colorTable <- colorTable() 

    #take -log10 of y-axis column if requested
    if(input$logP == TRUE && zoomChart[1,input$yAxisColumn] != -1){
      zoomChart[,input$yAxisColumn] <- -log(zoomChart[,input$yAxisColumn],10)
    }                
    
    #check if there is too much data (>2500 data points), trim to 2500
    if(nrow(zoomChart)>2500){
      cutVal <- sort(zoomChart[,input$yAxisColumn],decreasing = T)[2500]
      zoomChart <- zoomChart[zoomChart[,input$yAxisColumn] >= cutVal,]
    }                
    
    zoomTable <- data.frame(x=zoomChart[,input$bpColumn],y=zoomChart[,input$yAxisColumn],trait=zoomChart$trait,
#                                         name=sprintf("<table cellpadding='4' style='line-height:1.5'><tr><th>%1$s</th></tr><tr><td align='left'>RMIP: %2$s<br>Location: %3$s<br>Base Pairs: %4$s<br>SNP: %5$s<br>Chromosome: %6$s</td></tr></table>",
                                         name=sprintf("<table cellpadding='4' style='line-height:1.5'><tr><th>%1$s</th></tr><tr><td align='left'>Y-axis value: %2$s<br>Base Pairs: %3$s<br>Chromosome: %4$s</td></tr></table>",                                         
                                         zoomChart$trait,
                                         zoomChart[,input$yAxisColumn],
                                         #zoomChart$loc,
                                         prettyNum(zoomChart[,input$bpColumn], big.mark = ","),
                                         #zoomChart$SNP,
                                         zoomChart[,input$chrColumn]
                            ),
                            url="http://danforthcenter.org",
                            chr=zoomChart[,input$chrColumn],
                            bp=zoomChart[,input$bpColumn])
    zoomSeries <- lapply(split(zoomTable, zoomTable$trait), function(x) {
      res <- lapply(split(x, rownames(x)), as.list)
      names(res) <- NULL
      res <- res[order(sapply(res, function(x) x$x))]
      return(res)
    })
    

     if(input$supportInterval==TRUE){
       if(nrow(SIchart) == 0){ #make a dummy table, but we won't plot the series anyways
         SIchart <- values[[input$datasets]][1,]
         SIchart[,input$SIyAxisColumn] <- -1    
         if(length(input$traitColumns) > 1){
           SIchart$trait <- do.call(paste,c(SIchart[,input$traitColumns],sep="_"))    
         }else{
           SIchart$trait <- SIchart[,input$traitColumns]
         }                      
       }     
       SIchart$loc_el <- SIchart$trait
       SIchart$trait <- paste(SIchart$trait,"Int",sep="_")
       SIchart <- SIchart[order(SIchart[[input$SIbpStart]]),]
       jlTable <- adply(SIchart,1,function(x) {data.frame(x=c(x[[input$SIbpStart]],x[[input$SIbpEnd]],x[[input$SIbpEnd]]),y=c(x[[input$SIyAxisColumn]],x[[input$SIyAxisColumn]],NA),trait=x$trait,
                                                          name=sprintf("<table cellpadding='4' style='line-height:1.5'><tr><td align='left'>Interval: %1$s-%2$s<br>Chromosome: %3$s</td></tr></table>",
                                                                       #                                                                      x$trait,
                                                                       #                                                                      x[[input$SIyAxisColumn]],
                                                                       prettyNum(x[[input$SIbpStart]], big.mark = ","),
                                                                       prettyNum(x[[input$SIbpEnd]], big.mark = ","),
                                                                       x[[input$chrColumn]]
                                                          ),loc_el=x$loc_el,bp=x[[input$bpColumn]],chr=x[[input$chrColumn]],stringsAsFactors=FALSE
                                                          #                                                   
                                                          #                                                   totalBP=x$totalBP,
                                                          #                                                   chr=x$Chromosome,stringsAsFactors=FALSE
       )}#end jlTable and function
       )#end adply
       jlTable <- jlTable[,c("x","y","trait","name","loc_el","bp","chr")]
       #jlTable <- jlTable[order(jlTable$x),]
     }#end if support interval

    thisChrAnnot <- subset(annotGeneLoc[input$organism][[1]],chromosome==input$chr)
    thisAnnot <- thisChrAnnot[thisChrAnnot$transcript_start >= winLow & thisChrAnnot$transcript_end <= winHigh,]
    if(nrow(thisAnnot)==0){ #nothing is in the window, but lets still make a data.frame (actually make it big just to hopefully pick up one row from each strand...)
      thisAnnot <- thisChrAnnot[1:100,]
    }
    thisAnnot <- thisAnnot[order(thisAnnot$transcript_start),]
    

    araburlBase <- 'http://arabidopsis.org/servlets/TairObject?type=locus&name='

    annotYvalReverse <- 0.01    
    #if(input$axisLimBool == TRUE){annotYvalReverse <- input$axisMin+0.01}
    annotYvalForward <- annotYvalReverse + 0.04
    if(input$organism ==  "Arabidopsis"){#strand is '+' or '-'
      annotTable <- adply(thisAnnot[thisAnnot$strand=="+",],1,function(x) {data.frame(x=c(x$transcript_start,x$transcript_end,x$transcript_end),y=c(annotYvalForward,annotYvalForward,NA),url=paste0(araburlBase,x$Locus),
                                                              name=sprintf("<table cellpadding='4' style='line-height:1.5'><tr><th>%1$s</th></tr><tr><td align='left'>Location: %2$s-%3$s<br>Chromosome: %4$s, Strand: %5$s<br>Short Desc.: %6$s</td></tr></table>",
                                                                           x$name,
                                                                           prettyNum(x$transcript_start, big.mark = ","),
                                                                           prettyNum(x$transcript_end, big.mark = ","),
                                                                           x$chromosome,                                                                           
                                                                           x$strand,
                                                                           x$short_description
                                                                          # x$Curator_summary
                                                              ),
                                                              stringsAsFactors=FALSE)})   
      
      annotTableReverse <- adply(thisAnnot[thisAnnot$strand=="-",],1,function(x) {data.frame(x=c(x$transcript_start,x$transcript_end,x$transcript_end),y=c(annotYvalReverse,annotYvalReverse,NA),url=paste0(araburlBase,x$Locus),
                                                                                     name=sprintf("<table cellpadding='4' style='line-height:1.5'><tr><th>%1$s</th></tr><tr><td align='left'>Location: %2$s-%3$s<br>Chromosome: %4$s, Strand: %5$s<br>Short Desc.: %6$s</td></tr></table>",
                                                                                                  x$name,
                                                                                                  prettyNum(x$transcript_start, big.mark = ","),
                                                                                                  prettyNum(x$transcript_end, big.mark = ","),
                                                                                                  x$chromosome,                                                                           
                                                                                                  x$strand,
                                                                                                  x$short_description
                                                                                                  # x$Curator_summary
                                                                                     ),
                                                                                     stringsAsFactors=FALSE)})
    }else{#} if(input$organism == "Sorghum"){#strand is '+' or '-'
      annotTable <- adply(thisAnnot[thisAnnot$strand=="+",],1,function(x) {data.frame(x=c(x$transcript_start,x$transcript_end,x$transcript_end),y=c(annotYvalForward,annotYvalForward,NA),url=paste0(sorgurlBase,x$ID),
                                                                                      name=sprintf("<table cellpadding='4' style='line-height:1.5'><tr><th>%1$s</th></tr><tr><td align='left'>Location: %2$s-%3$s<br>Chromosome: %4$s, Strand: %5$s<br>Desc: %6$s<br>Top TAIR Hit: %7$s<br>Top Rice Hit: %8$s</td></tr></table>",
                                                                                                   x$name,
                                                                                                   #1,#x$V2.1,
                                                                                                   prettyNum(x$transcript_start, big.mark = ","),
                                                                                                   prettyNum(x$transcript_end, big.mark = ","),
                                                                                                   x$chromosome,                                                                           
                                                                                                   x$strand,
                                                                                                   x$defLine,
                                                                                                   x$bestArabHitDefline,
                                                                                                   x$bestRiceHitDefline
                                                                                                   # x$Curator_summary
                                                                                      ),
                                                                                      stringsAsFactors=FALSE)})   
      
      annotTableReverse <- adply(thisAnnot[thisAnnot$strand=="-",],1,function(x) {data.frame(x=c(x$transcript_start,x$transcript_end,x$transcript_end),y=c(annotYvalReverse,annotYvalReverse,NA),url=paste0(sorgurlBase,x$ID),
                                                                                             name=sprintf("<table cellpadding='4' style='line-height:1.5'><tr><th>%1$s</th></tr><tr><td align='left'>Location: %2$s-%3$s<br>Chromosome: %4$s, Strand: %5$s<br>Desc: %6$s<br>Top TAIR Hit: %7$s<br>Top Rice Hit: %8$s</td></tr></table>",
                                                                                                          x$name,
                                                                                                          #1,#x$V2.1,
                                                                                                          prettyNum(x$transcript_start, big.mark = ","),
                                                                                                          prettyNum(x$transcript_end, big.mark = ","),
                                                                                                          x$chromosome,                                                                           
                                                                                                          x$strand,
                                                                                                          x$defLine,
                                                                                                          x$bestArabHitDefline,
                                                                                                          x$bestRiceHitDefline
                                                                                                          # x$Curator_summary
                                                                                             ),
                                                                                             stringsAsFactors=FALSE)})
    }

    annotTable <- annotTable[,c("x","y","name","url")]

    if(nrow(annotTableReverse)==0){
      annotTableReverse <- data.frame(x=character(0),y=character(0),name=character(0),url=character(0),stringsAsFactors = FALSE)
    }
    annotTableReverse <- annotTableReverse[,c("x","y","name","url")]

    annotArray <- toJSONArray2(annotTable, json = F, names = T)

    
    annotArrayReverse <- toJSONArray2(annotTableReverse, json = F, names = T)



    b <- rCharts::Highcharts$new()
    b$LIB$url <- 'highcharts/'
    b$chart(zoomType="xy",alignTicks=FALSE,events=list(click = "#!function(event) {this.tooltip.hide();}!#"))
    b$xAxis(title = list(text = "Base Pairs"),startOnTick=FALSE,min=winLow,max=winHigh,endOnTick=FALSE)      

    if(input$axisLimBool == TRUE){
      b$yAxis(title=list(text=input$yAxisColumn),min=input$axisMin,max=input$axisMax,startOnTick=FALSE)
      #create a hidden axis to put the gene track on, all the options are setting to hide everything from the axis 
      b$yAxis(labels=list(enabled=FALSE),title=list(text=NULL),min=0,max=1,lineWidth=0,gridLineWidth=0,minorGridLineWidth=0,lineColor="transparent",minorTickLength=0,tickLength=0,startOnTick=FALSE,opposite=TRUE,replace=FALSE)
    }else{      
      b$yAxis(title=list(text=input$yAxisColumn),startOnTick=FALSE) 
      #create a hidden axis to put the gene track on, all the options are setting to hide everything from the axis
      b$yAxis(labels=list(enabled=FALSE),title=list(text=NULL),min=0,max=1,lineWidth=0,gridLineWidth=0,minorGridLineWidth=0,lineColor="transparent",minorTickLength=0,tickLength=0,startOnTick=FALSE,opposite=TRUE,replace=FALSE)
    }

    if(input$supportInterval==TRUE){
      if(input$SIaxisLimBool == TRUE){
        b$yAxis(title=list(text=input$SIyAxisColumn),min=input$SIaxisMin,max=input$SIaxisMax,gridLineWidth=0,minorGridLineWidth=0,startOnTick=FALSE,opposite=TRUE,replace=FALSE)
      }else{
        b$yAxis(title=list(text=input$SIyAxisColumn),gridLineWidth=0,minorGridLineWidth=0,startOnTick=FALSE,opposite=TRUE,replace=FALSE)   
      }
      
      if(SIchart[1,input$SIyAxisColumn] != -1){
        d_ply(jlTable,.(trait),function(x){
          b$series(
            data = toJSONArray2(x,json=F,names=T),
            type = "line",
            dashStyle = 'Solid',
            lineWidth = 10,
            name = unique(x$trait),
            yAxis=2,           
            color = colorTable$color[colorTable$trait == as.character(unique(x$loc_el))])})            
      }
    }

    if(zoomChart[1,input$yAxisColumn] != -1){
      invisible(sapply(zoomSeries, function(x) {if(length(x)==0){return()};b$series(data = x, type = "scatter", color = colorTable$color[colorTable$trait == as.character(x[[1]]$trait)], name = paste0(x[[1]]$trait))}))
    }
    
    b$series(
      data = annotArray,
      type = "line",
      name = "Forward Genes",
      color = "blue",
      yAxis = 1
    )    

    b$series(
      data = annotArrayReverse,
      type = "line",
      name = "Reverse Genes",
      color = "red", 
      yAxis = 1
    )      

    b$chart(zoomType="xy",alignTicks=FALSE,events=list(click = "#!function(event) {this.tooltip.hide();}!#"))

    b$plotOptions(
      scatter = list(
        cursor = "pointer",
        point = list(
          events = list(
            click = "#! function(event) {alert(this.trait);} !#")), #display popup

        marker = list(
          symbol = "circle",
          radius = 5
        )
      ),
       line = list(
         lineWidth = 6,
         cursor = "pointer",
        #stickyTracking=FALSE,
        point = list(
          events = list(
            click = "#! function() { window.open(this.url); } !#")), #open webpage

        marker = list(
          enabled = FALSE,
          radius = 1,
          states = list(hover = list(enabled=FALSE))
        )
     )            
    )        

    b$tooltip(snap=5, useHTML = T, formatter = "#! function() { return this.point.name; } !#") 
    b$exporting(enabled=TRUE,filename='zoomChart',sourceWidth=2000)
    b$credits(enabled=TRUE)
    b$set(dom = 'zChart')
    return(b)
  })
    
  output$testChart <- renderChart({
    h1 <- hPlot(x = "Wr.Hnd", y = "NW.Hnd", data = MASS::survey, type = c("line", 
                                                                          "bubble", "scatter"), group = "Clap", size = "Age")
    h1$set(dom = 'testChart')
    return(h1)     
  })
  

   colorTable <- reactive({     
     traitVals <- list()
     if(input$plotAll == FALSE){
       for(i in input$traitColumns){
         traitVals[[i]] <- input[[i]]
       }     
       
       traits <- do.call(paste,c(expand.grid(traitVals),sep="_"))     
       if(length(traits)==0){return(NULL)}
       
       colorTable <- data.frame(trait=traits,color=rep(allColors,ceiling(length(traits)/30))[1:length(traits)])
     }else{
       colorTable <- data.frame(trait=input$datasets,color=allColors[1])
     }
     colorTable
   })
   
  findGWASOverlaps <- function(genomeChart){  
    if(is.null(input$overlapSize)){return(genomeChart[1,])}
    tableIn <- genomeChart
    tableIn$winStart <- tableIn[,input$bpColumn]-input$overlapSize
    tableIn$winStop <- tableIn[,input$bpColumn]+input$overlapSize
    
    allGr <- GRanges(tableIn[,input$chrColumn], IRanges(start=tableIn$winStart,end=tableIn$winStop))

    tableIn$group <- subjectHits(findOverlaps(allGr, reduce(allGr)))
    
    #just groups that have more than one unique SNP
    gwasDataOverlap <- tableIn[tableIn$group %in% as.data.frame(table(tableIn$group))[as.data.frame(table(tableIn$group))$Freq>1,"Var1"],]
    
    #just groups that have more than one unique phenotype
    gwasDataOverlapDiffPheno <- ddply(gwasDataOverlap,.(group),function(x){if(nrow(unique(as.data.frame(x[,"trait"])))>=input$numOverlaps){x}else{x[0,]}})

    return(gwasDataOverlapDiffPheno)    
  }

   observe({     
    if(is.null(input$SubmitColsButton) || input$SubmitColsButton == 0){return()}
    isolate({
      currDatasetProp <- datasetProp()
      #print("before")
      #print(currDatasetProp)
      if(as.character(input$datasets) %in% currDatasetProp$dataset){
        currDatasetProp <- currDatasetProp[currDatasetProp$dataset != as.character(input$datasets),]
      }
      #print("after")
      #print(currDatasetProp)
      cols <- varnames()
      #print("data.frame")
      #print(data.frame(dataset=input$datasets,chrColumn=names(cols[cols==input$chrColumn]),bpColumn=names(cols[cols==input$bpColumn]),
      #                 traitCol=paste(names(cols[cols %in% input$traitColumns]),collapse=";"),yAxisColumn=names(cols[cols==input$yAxisColumn]),axisLim=input$axisLimBool,axisMin=input$axisMin,axisMax=input$axisMax,stringsAsFactors=FALSE))
      currDatasetProp <-  rbind(currDatasetProp,data.frame(dataset=input$datasets,chrColumn=names(cols[cols==input$chrColumn]),bpColumn=names(cols[cols==input$bpColumn]),
                                                  traitCol=paste(names(cols[cols %in% input$traitColumns]),collapse=";"),yAxisColumn=names(cols[cols==input$yAxisColumn]),
                                                  logP=input$logP,axisLim=input$axisLimBool,axisMin=input$axisMin,axisMax=input$axisMax,organism=input$organism,plotAll=input$plotAll,
                                                  supportInterval=input$supportInterval,SIyAxisColumn=input$SIyAxisColumn,SIbpStart=input$SIbpStart,SIbpEnd=input$SIbpEnd,
                                                  SIaxisLimBool=input$SIaxisLimBool,SIaxisMin=input$SIaxisMin,SIaxisMax=input$SIaxisMax,stringsAsFactors=FALSE))      

      write.table(file="./www/config/datasetProperties.csv",x=currDatasetProp,col.names=TRUE,row.names=FALSE,sep=",")
      updateTabsetPanel(session, "datatabs", selected = "panel1")
     })

   })
  
   observe({
     if(is.null(input$saveDatasetButton) || input$saveDatasetButton == 0){return()}
       isolate({
          if(!file.exists(paste0("./www/config/data/",input$datasets))){
            write.table(getdata(),paste0("./www/config/data/",input$datasets),sep=",",col.names=TRUE,row.names=FALSE) 
          }
       })
   })
   
   datasetProp <- function(){
     return(read.table("./www/config/datasetProperties.csv",sep=",",head=TRUE,stringsAsFactors=FALSE))
   }
 
   observe({
    center <- as.numeric(input$selected[[1]])
    winHigh <- center + input$window[1]
    winLow <- center - input$window[1]
    band = list(from = winLow, to = winHigh, color = "rgba(68, 170, 213, 0.4)")
    session$sendCustomMessage(type = "customMsg", band)
   })  
  

  
  
  
    })
  
  

