suppressPackageStartupMessages({
  library(shiny)
  library(plotly)
  library(shinydashboard)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(data.table)
  library(shinyWidgets)
  library(DT)
  library(htmlwidgets)
  library(reshape2)
  library(RColorBrewer)
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(rhandsontable)
  library(shinythemes)
  library(bslib)
  library(vistime)  
  library(shinyscreenshot)
})

dt_projects <- read_csv("./demo_data.csv")

a<-unique(na.omit(dt_projects)$Project)

Sys.getlocale()


ui = function(request){
  dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Making Project Management Seamless using Automated Gantt Charts", titleWidth = 430
    ),
    
    dashboardSidebar(sidebarMenu(
      menuItem("Home", icon = icon("house"), tabName = "home"),
      menuItem("Gantt Chart", tabName = "chart", icon = icon("bars-progress")),
      menuItem("Progress", tabName = "progress", icon = icon("chart-line")),
      menuItem("Create your own", tabName = "plot", icon = icon("arrows-rotate")),
      menuItem("About the shiny app", tabName = "report", icon = icon("circle-info")))),
    
    dashboardBody(                                                                   
      
      tabItems(  
        tabItem(tabName = "home",  
                
                fluidRow(
                  valueBoxOutput("Completed", width = 3),
                  valueBoxOutput("WIP", width = 3),
                  valueBoxOutput("Delayed", width = 3),
                  valueBoxOutput("OnHold", width = 3)
                ),
                
                fluidRow(
                  box(downloadButton("downloadData", "Download"),DTOutput("Overview"), width = 12,solidHeader = TRUE, status = "info", title = "Projects Overview", collapsible = TRUE),
                  box(plotlyOutput("Project_category"), width = 4,solidHeader = TRUE, status = "primary", title = "Research Areas", collapsible = TRUE),
                  box(plotlyOutput("Project_status"), width = 8,solidHeader = TRUE, status = "success", title = "Project Status", collapsible = TRUE),
                  box(DTOutput("upcoming_deadlines"), width = 6,solidHeader = TRUE, status = "danger", title = "Upcoming Deadlines", collapsible = TRUE),
                  box(DTOutput("overdue_tasks"), width = 6,solidHeader = TRUE, status = "danger", title = "Overdue Tasks", collapsible = TRUE)
                  
                  
                ) ),
        
        
        tabItem(tabName = "chart",
                fluidRow(box(width = 20,textOutput(outputId = "gg1"),
                             downloadButton('downloadPlot1', 'Download'),
                             plotlyOutput(outputId = "gantt1", height = 600, width="100%"))),
                fluidRow(box(width = 20,textOutput(outputId = "gg2"),
                             downloadButton('downloadPlot2', 'Download'),
                             plotlyOutput(outputId = "gantt2", height = 600, width="100%"))),
                fluidRow(box(width = 20,textOutput(outputId = "Vistime"),
                             downloadButton('downloadPlot3', 'Download'),
                             plotlyOutput(outputId = "gantt3", height = 600, width="100%"))),
                
        ),
        
        tabItem(tabName = "progress",
                fluidRow(radioButtons(inputId = "project",
                                      label = "Here is the progress line chart for:",
                                      choices =c(a[1:length(a)]),
                                      selected = "Project 1",inline = TRUE)), 
                fluidRow(box(width = 20,height = "100%",
                             downloadButton('downloadchart', 'Download'),
                             plotlyOutput("progress", width="100%"))) 
        ),
        
        
        tabItem(tabName = "plot",
                fluidRow(box(width = 20,textOutput(outputId = "text_table"),
                             downloadButton('downloadData1', 'Download'),
                             rHandsontableOutput("hot"),
                             actionButton('addRow', 'Add a Row')),
                         fluidRow(box(width = 20,radioButtons(inputId = "Gantt_type",
                                                              label = "Please choose the type of Gantt chart you want:",
                                                              choices =c("ggplot2_V1","plotly_V1","Vistime"),selected = "ggplot2_V1",inline = TRUE))),
                         fluidRow(box(width = 20,height = "100%",
                                      screenshotButton(label = "Capture entire page"),
                                      screenshotButton(label = "Capture plot", id = "create_gantt"),
                                      plotlyOutput("create_gantt", height = 600, width="100%")))) 
        ),
        tabItem(tabName = "report",
                fluidRow(
                  box(width = 20,h2("Background"),
                      h4(p(
                        "We are often involved in many projects as researchers in academia. 
                        Ensuring that we have the bigger picture of the project tasks can be challenging
                        if not well documented. Recently, our line manager asked us to create a Gantt chart
                        to present all current tasks and how far we are from completing them. 
                        R Markdown came to mind at first, and we created beautiful Gantt charts 
                        which can be shared as an HTML file. Since the projects are evolving,
                        new tasks are assigned to us. Therefore, to present the updated Gantt Chart,
                        we should rerun the R Markdown file." ),
                        
                        p("A question came to mind. Instead of rerunning the R Markdown file to 
                        update the Gantt Chart, could Shiny Apps be used to automate this process? 
                        The answer is yes! We created an R Shiny App that can automate and update Gantt Charts. 
                        This results in improving the efficiency of project management in academia."),
                        p("In this App, we aim to point out some of the awesome features of 
                          R shiny and demonstrate how our R Shiny App can easily automate Gantt charts and track the progress of tasks."))
                  )
                ),
                br(),
                fluidRow(
                  box(width = 20,h2("Code"),
                      h4(p("Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href=" https://www.nature.com/articles/s41746-019-0180-3","Github.")))
                  )
                ),
                br(),
                fluidRow(
                  box(width = 20,h2("Sources"),
                      h4(p("A blog post on Creating a pretty Gantt chart with ggplot2 by Andrew Heiss.",tags$a(href="https://stats.andrewheiss.com/misc/gantt.html","Github.")),
                         p("A shiny app ",tags$a(href="https://github.com/AABoyles/ShinyGanttCharts","ShinyGanttCharts"),"developed by Anthony A. Boyles."),
                         p(tags$a(href="https://cran.r-project.org/web/packages/rhandsontable/vignettes/intro_rhandsontable.html","A rhandsontable"),"introduction blog post by Jonathan Owen"),
                         p("A blog post on",tags$a(href="https://cran.r-project.org/web/packages/rhandsontable/vignettes/intro_rhandsontable.html","Project mangement"),"by Amit Kulkarni")
                      )
                  )),
                br(),
                fluidRow(
                  box(width = 20,h2("Authors"),
                      h4(p("Dr Yuanyuan Zhang , Research Associate, The Centre for Digital Trust and Society , School of Social Sciences , The University of Manchester"),
                         p("Lucy Njoki Njuki, Research Assistant, Division of Musculoskeletal & Dermatological Sciences, Centre for Epidemeology Versus Arthritis, School of Biological Sciences , The University of Manchester")))
                ),
                
                fluidRow(
                  box(width = 20,h2("Contact"),
                      h4(p("Yuanyuan Zhang (yuanyuan.zhang@manchester.ac.uk)"),
                         p("Lucy Njoki Njuki (lucy.njuki@manchester.ac.uk)")))
                ),
                br(),
                fluidRow(
                  box(width = 20,h4("No copyright infringement intended. "),
                      h4(p("DISCLAIMER: If you use or refer to this project management 
                           tool in your work (including presentations), please include the following text:"),
                         p("Univerisity of Manchester, Making Project Management Seamless using Automated Gantt Charts Tool v1.0, 2023")))
                ),
                br(),
                br(),
                tags$img( height = 200, width = 450, src= "https://www.staffnet.manchester.ac.uk/brand/visual-identity/logo/logo_big.gif")
                
        )
        
      )
    ))
    
}

server <- function(input, output, session){
  
  
  output$Completed <- renderValueBox({
    
    completed_projects <- dt_projects %>%
      filter(dt_projects$Status == "Completed")
    
    valueBox(value = tags$p(NROW(completed_projects), style = "font-size: 150%;"), 
             "Tasks Completed ", icon = icon("stack-overflow"), color = "fuchsia")
  })
  
  
  output$WIP <- renderValueBox({
    
    WIP_projects <- dt_projects %>%
      filter(dt_projects$Status == "Active")
    
    valueBox(value = tags$p(NROW(WIP_projects), style = "font-size: 150%;"),  
             "Tasks In Progress ", icon = icon("spinner"), color = "green")
  })
  
  
  output$Delayed <- renderValueBox({
    
    Delayed_projects <- dt_projects %>%
      filter(dt_projects$Status %in% c("Delayed","On hold"))
    
    valueBox(value = tags$p(NROW(Delayed_projects), style = "font-size: 150%;"),  
             "Tasks Postponed ", icon = icon("exclamation-triangle"), color = "yellow")
  })
  
  output$OnHold <- renderValueBox({
    
    OnHold_projects <- dt_projects %>%
      filter(dt_projects$Status == "Cancelled")
    
    valueBox(value = tags$p(NROW(OnHold_projects), style = "font-size: 150%;"),   
             "Tasks Cancelled", icon = icon("fire"), color = "red")
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(dt_projects, file)
    }
  )
  
  output$Overview <- renderDT({
    
    datatable(dt_projects,options = list(pageLength = 12))
    
  })
  
  
  
  ##---------------Home page / project share of research area pie chart------------------------------------------
  output$Project_category <- renderPlotly({
    
    df_ProjectType <- dt_projects %>% 
      group_by(Project) %>% 
      summarise("Tasks" = n())
    
    fig <- plot_ly(type='pie', labels=df_ProjectType$Project, values=df_ProjectType$Tasks, 
                   textinfo='label+percent',insidetextorientation='radial')
    
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig
    
  })
  
  output$Project_status <- renderPlotly({
    ggplot(dt_projects) +
      aes(x = Project, fill = Status) +
      geom_bar(position = "dodge") +
      scale_fill_hue() +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none") +
      facet_wrap(vars(Status))
    
  })
  
  
  output$upcoming_deadlines <- renderDT({
    
    df_tmp_upcoming <- dt_projects
    
    df_tmp_upcoming$END.DATE <- as.Date(df_tmp_upcoming$`End(Plan)`, format = "%m/%d/%Y")
    df_tmp_upcoming$START.DATE <- as.Date(df_tmp_upcoming$`Start(Plan)`, format = "%m/%d/%Y")
    
    df_tmp_upcoming <- df_tmp_upcoming%>% 
      filter((as.Date(END.DATE) > today()) & (Status != "Completed"))
    
    
    datatable(df_tmp_upcoming[,c("Project", "Task", "Start(Plan)","End(Plan)", "Status")],options = list(pageLength = 5))
    
  })
  
  
  
  output$overdue_tasks <- renderDT({
    
    df_tmp_overdue <- dt_projects
    df_tmp_overdue$END.DATE <- as.Date(df_tmp_overdue$`End(Plan)`, format = "%m/%d/%Y")
    df_tmp_overdue$START.DATE <- as.Date(df_tmp_overdue$`Start(Plan)`, format = "%m/%d/%Y")
    
    df_tmp_overdue <- df_tmp_overdue %>% 
      filter(as.Date(END.DATE) < today() & Status != "Completed")
    
    
    datatable(df_tmp_overdue[,c("Project", "Task", "Start(Plan)","End(Plan)", "Status")],options = list(pageLength = 5))
    
  })
  
  
  output$gg1 <- renderText({ paste("The following Gantt Chart ploted with ggplot2: ")})
  
  createPlot1 <- reactive({
    # req(input$hot)
    # 
    # tasks <- hot_to_r(input$hot)
    tasks <- dt_projects %>% select("Project","Task","Start","End") 
    tasks.long <- tasks %>%
      mutate(Start = as.Date(tasks$Start, format = "%m/%d/%Y"), End = as.Date(tasks$End, format = "%m/%d/%Y")) %>%
      gather(date.type, task.date, -c(Project, Task)) %>%
      arrange(date.type, task.date) %>%
      mutate(Task = factor(Task, levels=rev(unique(Task)), ordered=TRUE))
    
    # Calculate where to put the dotted lines that show up every three entries
    x.breaks <- seq(length(tasks$Task) + 0.5 - 3, 0, by=-3)
    theme_gantt <- function(base_size=11) {
      theme_bw(base_size) %+replace%
        theme(panel.background = element_rect(fill="#ffffff", colour=NA),
              axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
              title=element_text(vjust=1.2),
              panel.border = element_blank(), axis.line=element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(size=0.5, colour="grey80"),
              axis.ticks = element_blank(),
              legend.position = "bottom", 
              axis.title = element_text(size=rel(0.8)),
              strip.text = element_text(size=rel(1)),
              strip.background = element_rect(fill="#ffffff", colour=NA),
              panel.spacing.y = unit(1.5, "lines"),
              legend.key = element_blank())
    }
    
    tasks.long <- na.omit(tasks.long)
    plot1 <- ggplot(tasks.long, aes(x=Task, y=task.date, colour=Project)) +
      geom_line(size=6) +
      geom_vline(xintercept=x.breaks, colour="grey80", linetype="dotted") +
      guides(colour=guide_legend(title=NULL)) +
      coord_flip() +
      theme_gantt() + theme(axis.text.x=element_text(angle=45, hjust=1))
    
    ggplotly(plot1)
    
  })
  
  output$gantt1 <- renderPlotly({
    print(createPlot1())
  })
  
  output$downloadPlot1 <- downloadHandler(
    filename = "ShinyGanttChart1.html",
    content = function(file){
      htmlwidgets::saveWidget(as_widget(createPlot1()), file)
    }
  )
  
  output$gg2 <- renderText({ paste("The following Gantt Chart ploted with plotly: ")})
  
  createPlot2 <- reactive({
    # if appears errors, try this code "Sys.setlocale('LC_ALL','C')"
    df <- dt_projects %>% 
      dplyr::mutate(start = as.Date(dt_projects$Start, format = "%m/%d/%Y"),
                    end = as.Date(dt_projects$End, format = "%m/%d/%Y"),
                    start_plan =as.Date(dt_projects$`Start(Plan)`, format = "%m/%d/%Y"),
                    end_plan =as.Date(dt_projects$`End(Plan)`, format = "%m/%d/%Y"))
    df <- na.omit(df)
    
    cols <- RColorBrewer::brewer.pal(length(unique(df$Project)), name = "Dark2")
    df$color <- factor(df$Project, labels = cols)
    
    p <- plot_ly()
    
    
    for(i in 1:nrow(df)){
      p <- add_lines(p,
                     x = c(df$Start[i], df$End[i]),  
                     y = c(i, i),  
                     # mode = "lines",
                     line = list(color = df$color[i], width = 20), 
                     showlegend = F,
                     hoverinfo = "text", 
                     text = paste("Project: ",
                                  df$Project[i],
                                  "<br>",
                                  "Task: ",
                                  df$Task[i],
                                  "<br>",
                                  "Start: ",
                                  df$Start[i],
                                  "<br>",
                                  "End: ",
                                  df$End[i],
                                  "<br>",
                                  "Duration:",
                                  df$Duration_days[i],
                                  "<br>",
                                  "Status:",
                                  df$Status[i],
                                  "<br>",
                                  "Signed by:",
                                  df$signed_by[i])
      )
    }
    
    
    p <- layout(p,
                xaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6")),
                yaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6"),
                             tickmode = "array", tickvals = 1:nrow(df), ticktext = unique(df$Task),
                             domain = c(0, 0.9)),
                plot_bgcolor = "#333333",  
                paper_bgcolor = "#333333") 
    
    
    p
  })
  
  output$gantt2 <- renderPlotly({
    print(createPlot2())
  })
  
  output$downloadPlot2 <- downloadHandler(
    filename = "ShinyGanttChart2.html",
    content = function(file){
      htmlwidgets::saveWidget(as_widget(createPlot2()), file)
    }
  )
  
  output$Vistime <- renderText({ paste("The following Gantt Chart ploted with vistime package: ")})
  
  createPlot3 <- reactive({
    colnames(dt_projects)[5] <- "start"
    colnames(dt_projects)[6] <- "end"
    dt_projects$`Start(Plan)` <-as.Date(dt_projects$`Start(Plan)`, format = "%m/%d/%Y")
    dt_projects$`End(Plan)` <-as.Date(dt_projects$`End(Plan)`, format = "%m/%d/%Y")
    dt_projects$end <-as.Date(dt_projects$end, format = "%m/%d/%Y")
    dt_projects$start <-as.Date(dt_projects$start, format = "%m/%d/%Y")
    dt_projects <- na.omit(dt_projects)
    Sys.setlocale('LC_ALL','C')
    
    p1 <- vistime(dt_projects, col.event="Task", col.group="Project", 
                  title="Gantt Chart test with Vistime",optimize_y = FALSE, linewidth = 25)
    
    pp <- plotly::plotly_build(p1)
    
    pp$x$layout[["yaxis"]]$tickfont <- list(size =18)
    for(i in seq_along(pp$x$data)){
      if(pp$x$data[[i]]$mode == "text") pp$x$data[[i]]$textfont$size <- 8
    }
    pp
  })
  
  output$gantt3 <- renderPlotly({
    print(createPlot3())
    
  })
  
  output$downloadPlot3 <- downloadHandler(
    filename = "ShinyGanttChart3.html",
    content = function(file){
    htmlwidgets::saveWidget(as_widget(createPlot3()), file)
    }
  )
  
  progress_chart <- reactive({
    dt_projects <-na.omit(dt_projects)
    dt_projects$`End(Plan)` <- as.Date(dt_projects$`End(Plan)`, format = "%m/%d/%Y")
    dt_projects$End <-as.Date(dt_projects$End, format = "%m/%d/%Y")
    dt_projects$`Overall Progress` <- as.numeric(dt_projects$`Overall Progress`)
    project_name <-input$project
    data <-dt_projects[as.character(dt_projects$Project)==input$project,]
    
    plot_ly( data, x = ~`End(Plan)`, y = ~`Overall Progress`, type = 'scatter', mode = 'lines+markers', name = "Expected Progress",
             hoverinfo = text,
             text = ~paste("Expected Progress","</br> ",
                           "</br> # Project:" , data$Project,
                           "</br> # Task:" , data$Task,
                           "</br> # End:" , data$End.Plan,
                           "</br> # Overall Progress:" , data$`Overall Progress`,"%",
                           "</br> # Signed by:" , data$signed.by))  %>%
      add_trace(x = ~End,  mode = 'lines+markers',name = 'Real Progress',
                hoverinfo = text ,
                text = ~paste("Real Progress","</br> ",
                              "</br> # Project:" , data$Project,
                              "</br> # Task:" , data$Task,
                              "</br> # End:" , data$End,
                              "</br> # Overall Progress:" , data$`Overall Progress`,"%",
                              "</br> # Signed by:" , data$signed_by)) %>%
      layout(title = paste(project_name,"Planner"),yaxis = list(title = 'Percentage(100%)'),xaxis = list(title = 'Timeline'))
  })
  
  output$progress <- renderPlotly({
    print(progress_chart())
    
  })
  
  output$downloadchart <- downloadHandler(
    filename = "ProgressReport.html",
    content = function(file){
    htmlwidgets::saveWidget(as_widget(progress_chart()), file)
    }
  )
  
  ##### Create your own page ###########################
  
  tasks <- tibble(
    Project = c("","",""),
    Task = c("","",""),
    Start.Plan.=c("","",""),
    End.Plan.=c("","",""),
    Start = c("","",""),
    End = c("","",""),
    Duration_days=c("","",""),
    Status=c("","",""),
    Milestone=c("","",""),
    Overall.Progress=c("","",""),
    signed_by=c("","","")
  )
  
  output$downloadData1 <- downloadHandler(
    filename = 'ShinyGanttExport.csv',
    content = function(file){
      input$hot %>%
        hot_to_r() %>%
        write_csv(file)
    }
  )
  
  output$hot <- renderRHandsontable({
    if(!is.null(input$file)){
      if(input$file1$type %in% c("text/csv","text/comma-separated-values","text/plain",".csv")){
        tasks <- read_csv(input$file1$datapath)
      } else {
        tasks <- read_excel(input$file1$datapath)
      }
    }
    
    tasks %>%
      rhandsontable(width=1200) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  
  observeEvent(input$addRow, {
    tasks <- hot_to_r(input$hot)
    tasks[nrow(tasks)+1,] <- NA
    output$hot <- renderRHandsontable({
      tasks %>%
        rhandsontable() %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
  })
  
  generate_gantt <- reactive({
    
    if(input$Gantt_type == "plotly_V1"){
      req(input$hot)
      tasks <- hot_to_r(input$hot)
      df <- tasks %>%
        dplyr::mutate(start = as.Date(tasks$Start, format = "%m/%d/%Y"),
                      end = as.Date(tasks$End, format = "%m/%d/%Y"),
                      start_plan =as.Date(tasks$Start.Plan., format = "%m/%d/%Y"),
                      end_plan =as.Date(tasks$End.Plan., format = "%m/%d/%Y"))
      df <- na.omit(df)
      if(length(unique(df$Project))==0){
        
      }
      
      else if(length(unique(df$Project))==1){
        p <- plot_ly()
        
        for(i in 1:nrow(df)){
          p <- add_lines(p,
                         x = c(df$start[i], df$end[i]),
                         y = c(i, i),
                         # mode = "lines",
                         line = list(color = "orange", width = 20),
                         showlegend = F,
                         hoverinfo = "text",
                         text = paste("Project: ",
                                      df$Project[i],
                                      "<br>",
                                      "Task: ",
                                      df$Task[i],
                                      "<br>",
                                      "Start: ",
                                      df$Start[i],
                                      "<br>",
                                      "End: ",
                                      df$End[i],
                                      "<br>",
                                      "Duration:",
                                      df$Duration_days[i],
                                      "<br>",
                                      "Status:",
                                      df$Status[i],
                                      "<br>",
                                      "Signed by: ",
                                      df$signed_by[i])
          )
        }
        
        
        p <- layout(p,
                    xaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6")),
                    yaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6"),
                                 tickmode = "array", tickvals = 1:nrow(df), ticktext = unique(df$Task),
                                 domain = c(0, 0.9)),
                    plot_bgcolor = "#333333",
                    paper_bgcolor = "#333333")
        
        p
      }
      
      
      
      else if(length(unique(df$Project)) == 2){
        cols <- c("#E69F00", "#56B4E9")
        df$color <- factor(df$Project, labels = cols)
        p <- plot_ly()
        
        for(i in 1:nrow(df)){
          p <- add_lines(p,
                         x = c(df$start[i], df$end[i]),
                         y = c(i, i),
                         # mode = "lines",
                         line = list(color = df$color[i], width = 20),
                         showlegend = F,
                         hoverinfo = "text",
                         text = paste("Project: ",
                                      df$Project[i],
                                      "<br>",
                                      "Task: ",
                                      df$Task[i],
                                      "<br>",
                                      "Start: ",
                                      df$Start[i],
                                      "<br>",
                                      "End: ",
                                      df$End[i],
                                      "<br>",
                                      "Duration:",
                                      df$Duration_days[i],
                                      "<br>",
                                      "Status:",
                                      df$Status[i],
                                      "<br>",
                                      "Signed by: ",
                                      df$signed_by[i])
          )
        }
        
        
        p <- layout(p,
                    xaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6")),
                    yaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6"),
                                 tickmode = "array", tickvals = 1:nrow(df), ticktext = unique(df$Task),
                                 domain = c(0, 0.9)),
                    plot_bgcolor = "#333333",
                    paper_bgcolor = "#333333")
        
        
        p
        
      }
      
      
      
      else if(length(unique(df$Project)) > 2){
        
        cols <- RColorBrewer::brewer.pal(length(unique(df$Project)), name ="Dark2")
        df$color <- factor(df$Project, labels = cols)
        p <- plot_ly()
        
        for(i in 1:nrow(df)){
          p <- add_lines(p,
                         x = c(df$start[i], df$end[i]),
                         y = c(i, i),
                         line = list(color = df$color[i], width = 20),
                         showlegend = F,
                         hoverinfo = "text",
                         text = paste("Project: ",
                                      df$Project[i],
                                      "<br>",
                                      "Task: ",
                                      df$Task[i],
                                      "<br>",
                                      "Start: ",
                                      df$Start[i],
                                      "<br>",
                                      "End: ",
                                      df$End[i],
                                      "<br>",
                                      "Duration:",
                                      df$Duration_days[i],
                                      "<br>",
                                      "Status:",
                                      df$Status[i],
                                      "<br>",
                                      "Signed by: ",
                                      df$signed_by[i])
          )
        }
        
        
        p <- layout(p,
                    xaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6")),
                    yaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6"),
                                 tickmode = "array", tickvals = 1:nrow(df), ticktext = unique(df$Task),
                                 domain = c(0, 0.9)),
                    plot_bgcolor = "#333333",
                    paper_bgcolor = "#333333")
        
        
        p
        
      }
    }
    
    else if(input$Gantt_type == "ggplot2_V1"){
      
      req(input$hot)
      tasks <- hot_to_r(input$hot)
      tasks<- tasks%>% select("Project","Task","Start","End") 
      tasks <- tasks %>%
        dplyr::mutate(Start = as.Date(tasks$Start, format = "%m/%d/%Y"),
                      End = as.Date(tasks$End, format = "%m/%d/%Y"))
      tasks <- na.omit(tasks)
      tasks.long <- tasks %>%
        gather(date.type, task.date, -c(Project, Task)) %>%
        arrange(date.type, task.date) %>%
        mutate(Task = factor(Task, levels=rev(unique(Task)), ordered=TRUE))
      
      if(length(unique(tasks$Project))==0){
        
        
      }
      
      else if(length(unique(tasks$Project))>0){
        x.breaks <- seq(length(tasks$Task) + 0.5 - 3, 0, by=-3)
        theme_gantt <- function(base_size=11) {
          theme_bw(base_size) %+replace%
            theme(panel.background = element_rect(fill="#ffffff", colour=NA),
                  axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
                  title=element_text(vjust=1.2),
                  panel.border = element_blank(), axis.line=element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_line(size=0.5, colour="grey80"),
                  axis.ticks = element_blank(),
                  legend.position = "bottom", 
                  axis.title = element_text(size=rel(0.8)),
                  strip.text = element_text(size=rel(1)),
                  strip.background = element_rect(fill="#ffffff", colour=NA),
                  panel.spacing.y = unit(1.5, "lines"),
                  legend.key = element_blank())
        }
        
        tasks.long <- na.omit(tasks.long)
        ggplot(tasks.long, aes(x=Task, y=task.date, colour=Project)) +
          geom_line(size=6) +
          geom_vline(xintercept=x.breaks, colour="grey80", linetype="dotted") +
          guides(colour=guide_legend(title=NULL)) +
          coord_flip() +
          theme_gantt() + theme(axis.text.x=element_text(angle=45, hjust=1))
      }
    }
    
    else if(input$Gantt_type == "Vistime"){
      
      req(input$hot)
      tasks <- hot_to_r(input$hot)
      tasks <- tasks %>%
        dplyr::mutate(start = as.Date(tasks$Start, format = "%m/%d/%Y"),
                      end = as.Date(tasks$End, format = "%m/%d/%Y"),
                      start_plan =as.Date(tasks$Start.Plan., format = "%m/%d/%Y"),
                      end_plan =as.Date(tasks$End.Plan., format = "%m/%d/%Y"))
      tasks <- na.omit(tasks)
      if(length(unique(tasks$Project))==0){  
        
      }
      
      if(length(unique(tasks$Project))>0){ 
        
        p <- vistime(tasks, col.event="Task", col.group="Project", title="Gantt Chart with Vistime",optimize_y = FALSE, linewidth = 25)
        
        pp <- plotly::plotly_build(p)
        
        pp$x$layout[["yaxis"]]$tickfont <- list(size =18)
        for(i in seq_along(pp$x$data)){
          if(pp$x$data[[i]]$mode == "text") pp$x$data[[i]]$textfont$size <- 8
        }
        pp 
      }
    }
  })
  
  output$create_gantt <- renderPlotly({
    print(generate_gantt())
  })
  
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
