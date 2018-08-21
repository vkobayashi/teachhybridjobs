#Sys.setenv("plotly_username"="vkobayashi")
#Sys.setenv("plotly_api_key"="aBeivnQu1THokB6hAkZl")


options(shiny.maxRequestSize=300*1024^2) 

library(rsconnect)
library(plotly)
library(shiny)
library(shinythemes)
library(dplyr)
library(wordcloud)
library(tm)
#library(data.table)
options(encoding = "UTF-8")
load("kaiser_match_final.rda")
#table(kaiser_match_final$employment_type)

actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}


ui <- fluidPage(
  theme = shinytheme("spacelab"),
  
  h2("Hybrid Jobs Explorer"),
  h4("This app shows hybrid jobs for teachers and non-teachers."),
  
  tags$hr(),
  
  fluidRow(
    # column(3,
    #        wellPanel(
    #          h4("File Input"),
    #          fileInput("file1", "Choose a File", accept=c("text/csv",
    #                                                       "text/comma-separated-values,text/plain",
    #                                                       ".csv")),
    #          tags$hr(),
    #          checkboxInput("header","Header", TRUE),
    #          radioButtons('sep', "Separator",
    #                       c(Comma=",",
    #                         Semicolon=";",
    #                         Tab="\t"),
    #                       "\t"),
    #          radioButtons('quote',"Quote",
    #                       c(None="",
    #                         "Double Quote"='"',
    #                         'Single Quote'="'"),
    #                       "")
    #        )
    # ),
    
   column(4, wellPanel(
     selectInput("input_teachtype","Wat doceer je op dit moment?"
                 ,choices=c("Nederlands","Informatiekunde/ICT-vakken","Scheikunde",
                   "Natuurkunde","Biologie","Wiskunde"
                   ,"Aardrijskunde","Duits","Frans"
                   )),
     uiOutput("ui_educlevel")
   )),
  
  
  column(3, uiOutput("ui_contract"),
         uiOutput("ui_employtype"),  
         uiOutput("ui_cities"),
         uiOutput("ui_hoursmax"))
  
    #column(2, checkboxGroupInput("eductype", label="Select education type", choices=c("HBO","HBO/WO","MBO","MBO/HBO","Onbekend",
    #                                                                           "Post_WO","VMBO/MBO","WO"), selected="HBO"))
  ),
  tags$hr(),
  fluidRow(column(8,
  tags$h2("The chart here shows the job function classes"),
  selectInput("input_crit", "Choose the criterion",
              c("Average Cosine", "Maximum Cosine")))),
  
  fixedRow(
    
    column(8,plotlyOutput("bubblefunctclass")),
    column(3,tabsetPanel(type="tabs",
                         tabPanel("Selection",verbatimTextOutput("selection")),
                         tabPanel("Top 10 Function Classes", tableOutput("topfunctionclass"))))
    
    
  ),
  
  tags$hr(),
  tags$h2("The chart here shows the job titles for the selected function class"),
  
  fixedRow(
    column(7,plotlyOutput("bubblejobtitle")),
    column(4,tabsetPanel(type="tabs",
                tabPanel("Top 10 Job Titles", tableOutput("rankjobtitles")),
                tabPanel("Top Locations", plotlyOutput("barlocation"))))
    
  ),
  
  
  tags$hr(),
  column(7, wellPanel(
    plotOutput("wordcloudjb")
  ))
  #tags$blockquote("The chart here provide location information of the jobtitles from the selected function class")
  
  #fixedRow(
   # column(5, plotlyOutput("barlocation"))
  #)
  
)

server <- function(input, output, session) {
  
  #output$heat <- renderPlotly({
   # plot_ly(x = nms, y = nms, z = correlation, 
  #          key = correlation, type = "heatmap", source = "heatplot") %>%
  #    layout(xaxis = list(title = ""), 
  #           yaxis = list(title = ""))
  #})
  
   filedata <- reactive({
     
     if(!exists("kaiser_match_final")) return(NULL)
     #inFile <- input$file1
     #if(is.null(inFile)) return(NULL)
     #kaiser_match_final =fread("kaiser_match_final_df.txt", header=TRUE, sep=",", quote="", stringsAsFactors = FALSE, encoding = "UTF-8")
     else return(kaiser_match_final) 
     #return(as.data.frame(kaiser_match_final))
   })
  
  output$ui_contract <- renderUI({
    #if (is.null(input$input_crit))
    #if(is.null(filedata())) return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    #switch(input$input_crit,
    #       "Average Cosine" = selectInput("dynamic", label="Select teaching type", choices=c("wis","ned","aard"), selected="wis"),
    #       "Maximum Cosine" = selectInput("dynamic", label="Select teaching type", choices=c("wiskunde","nederlands","aardrijskunde"), selected="wiskunde")
    #)
    contracttype= unique(filedata()$contract_type)
    #contracttype= unique(kaiser_match_final$contract_type)
    selectInput("contracttype", "Type of Contract",contracttype)
  })
  
  output$ui_cities <- renderUI({
    #if (is.null(input$input_crit))
    #if(is.null(filedata())) return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    #switch(input$input_crit,
    #       "Average Cosine" = selectInput("dynamic", label="Select teaching type", choices=c("wis","ned","aard"), selected="wis"),
    #       "Maximum Cosine" = selectInput("dynamic", label="Select teaching type", choices=c("wiskunde","nederlands","aardrijskunde"), selected="wiskunde")
    #)
    cities= unique(filedata()$job_location)
    #cities= unique(kaiser_match_final$job_location)
    
    selectizeInput("jobloc","Stad/Regio", cities,select="Amsterdam", multiple=TRUE)
  })
  
  output$ui_employtype <- renderUI({
    #if (is.null(input$input_crit))
    #if(is.null(filedata())) return()

    employment_type_=as.character(unique(filedata()$employment_type))
    #employmenttype=unique(kaiser_match_final$employment_type)
    selectInput("employmenttype","Type of Employment", choices=employment_type_)
  })
  
  output$ui_hoursmax <- renderUI({
    #if (is.null(input$input_crit))
    #if(is.null(filedata())) return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    #switch(input$input_crit,
    #       "Average Cosine" = selectInput("dynamic", label="Select teaching type", choices=c("wis","ned","aard"), selected="wis"),
    #       "Maximum Cosine" = selectInput("dynamic", label="Select teaching type", choices=c("wiskunde","nederlands","aardrijskunde"), selected="wiskunde")
    #)

    hoursmax=unique(filedata()$hours_per_week_max)
    #hoursmax=unique(kaiser_match_final$hours_per_week_max)

    sliderInput("hoursmax",label="Maximum number of hours", min=0, max=40, value=40, step=5)
  })
  
  output$ui_educlevel <- renderUI({
    #if (is.null(input$input_crit))
    #if(is.null(filedata())) return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    #switch(input$input_crit,
    #       "Average Cosine" = selectInput("dynamic", label="Select teaching type", choices=c("wis","ned","aard"), selected="wis"),
    #       "Maximum Cosine" = selectInput("dynamic", label="Select teaching type", choices=c("wiskunde","nederlands","aardrijskunde"), selected="wiskunde")
    #)

    educlevel = unique(filedata()$education_level)
    #educlevel = unique(kaiser_match_final$education_level)
    checkboxGroupInput("educlevel","Opleiding",choices=educlevel, selected="HBO" )

  })
  
  jobmatches <- reactive({
      validate(
        need(!is.null(filedata()), "Loading data...")
      )
    req(input$educlevel)
    req(input$employmenttype)
    req(input$jobloc)
     #validate(
     #  need(try(!is.null(kaiser_match_final)), "Loading data...")
     #)
    
    
    #if(is.null(filedata())) return()
    #req(kaiser_match_final)
    #get(kaiser_match_final)
    
   
    
    # Apply filters
    #if(!is.null(filedata()) ){
      max_job_names= c("aard_max","bio_max","duits_max","frans_max","ict_max","natuur_max","ned_max","schei_max","wis_max")
      mean_job_names =c("aard_mean","bio_mean","duits_mean","frans_mean","ict_mean","natuur_mean","ned_mean","schei_mean","wis_mean")
      
      max_jobs = apply(filedata()[,8:16],1,function(x) which(x==max(x)))
      
      # maxmean_output<-function(x){
      #   names(x)=mean_job_names[x]
      #   return(x)
      # }
      
      mean_jobs = apply(filedata()[,18:26],1,function(x) which(x==max(x)))
      
      # if(class(mean_jobs)=="integer"){
      #   mean_jobs=as.list(mean_jobs)
      #   mean_jobs=lapply(mean_jobs, maxmean_output)
      # }
      
      # teacher_choices = choices=c("Aardrijskunde","Biologie","Duits","Frans","Informatiekunde/ICT-vakken"
      #                             ,"Natuurkunde","Nederlands","Scheikunde","Wiskunde")
      
      teacher_choices =c("Aardrijskunde","Biologie","Duits","Frans","Informatiekunde/ICT-vakken"
                                  ,"Natuurkunde","Nederlands","Scheikunde","Wiskunde")
      
      max_scores =unname(apply(filedata()[,8:16],1,function(x) max(x)))
      maxmean_scores = unname(apply(filedata()[,18:26],1,function(x) max(x)))
      
      
      m <- filedata() %>% 
        mutate(MaxScores=max_scores, MaxMeanScores=maxmean_scores)
        
        
        
      
      if(input$input_crit=="Maximum Cosine") {
        chosen_type = max_job_names
        select_score= max_jobs
        #chosen_type = setNames(chosen_type,teacher_choices )
      } else {
        chosen_type = mean_job_names
        select_score = mean_jobs
        #chosen_type=setNames(chosen_type,teacher_choices )
        }
      
      chosen_type=setNames(chosen_type,teacher_choices )
      educ_level = input$educlevel
      employ_type= input$employmenttype
      job_loc = input$jobloc
      m <- m %>% 
        filter(unlist(lapply(select_score, function(x) chosen_type[input$input_teachtype] %in% names(x)), use.names=FALSE)) %>%
        filter(education_level %in% educ_level) %>%
        filter(employment_type==employ_type) %>%
        filter(job_location %in% job_loc) %>%
        select(employment_type,title,jobfeed_profession,jobfeed_profession_group, jobfeed_profession_class,job_location,MaxScores,MaxMeanScores,keywords) %>%
        droplevels()
      
      #if(!is.null(input$eductype)){
        #educlevel <- paste("%", input$educlevel,"%")
       # educlevel <- input$eductype
       # m <- m %>% filter(education_level %in% educlevel)
      #}
    #} else m <- NULL
      #as.data.frame(m)
  })
  
  output$bubblefunctclass <- renderPlotly({
       validate(
         need(!is.null(filedata()), "Please load data")
       )
    #req(jobmatches())
    
    
    #if(!(is.data.frame(jobmatches()) & nrow(jobmatches())==0)) {
    m_functclass <- jobmatches() %>% 
      #filter(!education_level %in% c("VMBO","HAVO","VWO","Elementair","HAVO/VWO","MAVO/HAVO","MAVO/VMBO")) %>%
      group_by(jobfeed_profession_class) %>%  
      summarise(Count = n()
                , maxscore=mean(MaxScores)
                , maxminscore=mean(MaxMeanScores)
                #, keywords=paste(keywords, sep= " ", collapse= " ")
                ) %>%
      #,meanpercen= max(percent), meancosine = max(aggrecos))
      arrange(desc(Count)) %>%
      as.data.frame()
    
     t <- list(
       family = "sans serif",
       size = 12,
       color = toRGB("grey50"))
    
    plot_ly(m_functclass, x = ~maxscore, y=~maxminscore, text=~jobfeed_profession_class, type="scatter",  mode='markers', 
            size=~Count, source="functionclassname",key=~jobfeed_profession_class, 
            #sizes=c(10,50),
            marker = list(opacity = 0.5, sizemode='diameter')) %>%
      layout(title = 'Matching job function classes',
             xaxis = list(title="Max  matches",showgrid = FALSE),
             yaxis = list(title="Max mean matches",showgrid = FALSE),
             showlegend=FALSE) %>%
      add_text(textfont=t,textposition = "top right")
    #} else return(NULL)
      #add_annotations()
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click", source = "functionclassname")
    #if(is.null(s) == T) return(NULL)
    if (length(s) == 0) {
      "Click a bubble in the chart to display a bubble chart of job titles"
    } else {
      cat("You selected: \n\n")
      as.list(s)
      #list(s)
    }
  })
  
  output$bubblejobtitle <- renderPlotly({
    validate(
      need(!is.null(jobmatches()), "Please load data")
    )
    #req(jobmatches())
    if(!(is.data.frame(jobmatches()) & nrow(jobmatches())==0)) {
    s <- event_data("plotly_click", source = "functionclassname")
    #if(is.null(s) == T) return(NULL)
    
    if (length(s)) {
      #vars <- c(s[["x"]], s[["y"]])
      #d <- setNames(mtcars[vars], c("x", "y"))
      #yhat <- fitted(lm(y ~ x, data = d))
      #functclassnames = m_functclass$functionclass
      
      m_jobtitle <- jobmatches() %>% 
        #filter(!education_level %in% c("VMBO","HAVO","VWO","Elementair","HAVO/VWO","MAVO/HAVO","MAVO/VMBO")) %>%
        group_by(jobfeed_profession_class,jobfeed_profession_group, title) %>%  
        summarise(Count = n()
                  , maxscore=mean(MaxScores)
                  , maxminscore=mean(MaxMeanScores)
                  #, keywords=paste(keywords, sep= " ", collapse= " ")
                  ) %>%
        #,   meanpercen= max(percent), meancosine = max(aggrecos))
        arrange(desc(Count)) %>% ungroup() %>%
        filter(jobfeed_profession_class == s[["key"]]) %>% 
        as.data.frame()
      
      plot_ly(m_jobtitle, x = ~maxscore, y=~maxminscore, type="scatter",mode="markers",size=~Count,
              #sizes=c(10,50),
              marker = list(symbol='circle',opacity = 0.5, sizemode = 'diameter', color='rgb(255,65,54)',
                            line = list(width = 2, color = '#FFFFFF')),
              hoverinfo='text',
              text=~paste('Job Title:', title,'<br>Max Cosine:', maxscore,'<br>Max Mean:', maxminscore,'<br>Count:', Count)) %>%
        layout(title = 'Matching job title',
               xaxis = list(title="Max percentage matches",showgrid = FALSE),
               yaxis = list(title="Max cosine matches",showgrid = FALSE),
               showlegend=FALSE)
      
            
      #plot_ly(d, x = ~x) %>%
      #  add_markers(y = ~y) %>%
      #  add_lines(y = ~yhat) %>%
       # layout(xaxis = list(title = s[["x"]]), 
        #       yaxis = list(title = s[["y"]]), 
        #       showlegend = FALSE)
    } else {
      plotly_empty()
    }
    } else return(NULL)
  })
  
  output$rankjobtitles <- renderTable({
    s <- event_data("plotly_click", source = "functionclassname")
    if (length(s)) {
    m_jobtitle <- jobmatches() %>% 
      #filter(!education_level %in% c("VMBO","HAVO","VWO","Elementair","HAVO/VWO","MAVO/HAVO","MAVO/VMBO")) %>%
      group_by(jobfeed_profession_class
               ,jobfeed_profession_group
               , title) %>%  
      summarise(Count = n()
                , maxscore=mean(MaxScores)
                ,maxminscore=mean(MaxMeanScores)) %>% 
      arrange(desc(maxminscore)) %>% ungroup() %>%
      filter(jobfeed_profession_class == s[["key"]]) %>% select(jobfeed_profession_group, title) %>%
      as.data.frame()
    head(m_jobtitle,10)
    
  }
    })
  
  output$topfunctionclass <- renderTable({
    #s <- event_data("plotly_click", source = "functionclassname")
    #if (length(s)) {
      m_jobtitle <- jobmatches() %>% 
        #filter(!education_level %in% c("VMBO","HAVO","VWO","Elementair","HAVO/VWO","MAVO/HAVO","MAVO/VMBO")) %>%
        group_by(jobfeed_profession_class, jobfeed_profession_group) %>%  
        summarise(Count = n(), maxscore=mean(MaxScores),maxminscore=mean(MaxMeanScores)) %>% 
        #,meanpercen= max(percent), meancosine = max(aggrecos)
        arrange(desc(maxminscore)) %>%
        as.data.frame()
      head(m_jobtitle,10)
      
    #}
  })
  
  
  output$barlocation <- renderPlotly({
    validate(
      need(!is.null(jobmatches()), "Please load data")
    )
    
    if(!(is.data.frame(jobmatches()) && nrow(jobmatches())==0)) {
    s <- event_data("plotly_click", source = "functionclassname")
    if (length(s)) {
      m_location <- jobmatches() %>% 
        #filter(!education_level %in% c("VMBO","HAVO","VWO","Elementair","HAVO/VWO","MAVO/HAVO","MAVO/VMBO")) %>%
        group_by(jobfeed_profession_class,jobfeed_profession_group, title,job_location) %>%  
        summarise(Count = n()) %>% 
         ungroup() %>%
        filter(jobfeed_profession_class == s[["key"]], !job_location=="") %>% group_by(job_location) %>% summarise(cityCount =sum(Count)) %>% 
        arrange(desc(cityCount)) %>% 
        as.data.frame() %>% head(20)
      
      plot_ly(m_location, x = ~job_location, y=~cityCount, type="bar") %>%
        layout(title = 'Location',
               xaxis = list(title="Cities",showgrid = FALSE, tickangle=-45),
               yaxis = list(title="Count",showgrid = FALSE),
               margin=list(b=100),
               showlegend=FALSE)
    
    } else {
      plotly_empty()
    }
    } else return(NULL)
    })
  
  output$wordcloudjb <- renderPlot({
    validate(
      need(!is.null(jobmatches()), "Please load data")
    )
    if(!(is.data.frame(jobmatches()) && nrow(jobmatches())==0)) {
      s <- event_data("plotly_click", source = "functionclassname")
      if(is.null(s) == T) return(NULL)
      
      if (length(s)) {
        #vars <- c(s[["x"]], s[["y"]])
        #d <- setNames(mtcars[vars], c("x", "y"))
       # yhat <- fitted(lm(y ~ x, data = d))
        #functclassnames = m_functclass$functionclass
        
        m_jobtitle <- jobmatches() %>% 
          #filter(!educ %in% c("VMBO","HAVO","VWO","Elementair","HAVO/VWO","MAVO/HAVO","MAVO/VMBO")) %>%
          filter(jobfeed_profession_class == s[["key"]]) %>% 
          select(keywords) %>%
          as.data.frame()
        
        job_corpus <- SimpleCorpus(VectorSource(as.character(m_jobtitle)))
        dtm<- TermDocumentMatrix(job_corpus)
        m <- as.matrix(dtm)
        v <- sort(rowSums(m), decreasing=TRUE)
        d<- data.frame(word=names(v), freq=v)
        
        wordcloud(d$word,d$freq, min.freq=1, max.words=50,random.color = TRUE)
        
        
        #plot_ly(d, x = ~x) %>%
         # add_markers(y = ~y) %>%
         # add_lines(y = ~yhat) %>%
         #layout(xaxis = list(title = s[["x"]]), 
         #      yaxis = list(title = s[["y"]]), 
         #      showlegend = FALSE)
      } else {
        NULL
      }
    } else return(NULL)
  })
}

shinyApp(ui, server, options = list(display.mode = "showcase"))
