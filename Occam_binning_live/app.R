#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(OneR)
library(DT)
library(dplyr)

source("app-functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("App to Bin data for Occam"),
  
  fluidRow(
    column(3,
           fileInput("datafile", "Choose CSV file",
                     accept=c("text/csv",
                              "text/comma-separated-values,text/plain")),
           checkboxInput("heading", "Has heading", TRUE),
           radioButtons("sep", "Separator",
                        c(Comma=",", Semicolon=";" ,Tab="\t"), ","),
           radioButtons("quote", "Quote",
                        c(None="", "Double Quote"="\"", "Single Quote"="'"), "")
    ),
    column(9,
           tabsetPanel(id = "tabs",
             tabPanel("File contents", DT::dataTableOutput('contents'))
#             tabPanel("it.contents", DT::dataTableOutput('it.contents')),
#             tabPanel("Interface table", DT::dataTableOutput('it.ready.contents')),
#             tabPanel("table.selected", DT::dataTableOutput('table.selected')),
#             tabPanel("Binned", DT::dataTableOutput('binned.table')),
#             tabPanel("Occam header", DT::dataTableOutput('outfile.table'))
             # I will eventually want an option to output the bin
             # breaks back to the user. This can be done w/ an option
             # in the interface, so the user can interpret their results
             # more thoroughly.
           )
    ),
    hr()
  ),
  conditionalPanel(
    condition = 'output.contents',
    fluidRow(
      p("Select two or more column variables for binning."),
      p("Variables with ordinality <2 will be ignored."),
      p("Character variables are assigned default bin target of 2."),
      p("NAs can be either categorized with bin 0 or an additional",
        "bin as \".\" that does not count against the target entered."),
      br(),
      
      # code below should allow select all but does not work
      #      column(2, helpText("Select All/None")),
      #      column(2,
      #             selectInput('change.selection', "",
      #                         c(" " = 2, "All" = 1, "None" = 0),
      #                         selected = "", width = '100%')
      #      ),
      
      DT::dataTableOutput('interface.table'),
      br(),
      fluidRow(
        column(3, actionButton("do", "Apply")),
        column(3,
               conditionalPanel(
                 condition = 'input.do',
                 downloadButton('downloadData', 'Download')
               )
        )
      ),
      br(),
      br()
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # uploaded csv file
  filedata <- reactive({
    req(input$datafile)
    inFile <- input$datafile
    read.csv(inFile$datapath,
             header = input$heading,
             sep = input$sep,
             quote = input$quote,
             na.strings = c("", "NA"),
             stringsAsFactors = FALSE)
  })
  
  # display a sample of uploaded file
  output$contents <- DT::renderDataTable(
    filedata(), options = list(autoWidth = TRUE, 
                               scrollX = TRUE, dom = "ltip",
                               ordering = FALSE,
                               lengthMenu = list(c(3,10,20), 
                                                 list("3", "10", "20")),
                               rownames = FALSE, selection = "none")
  )
  
  observeEvent(input$datafile, {
    appendTab(inputId = "tabs",
      tabPanel("Interface table", DT::dataTableOutput('it.ready.contents')),
      tabPanel("Binned", DT::dataTableOutput('binned.table')),
      tabPanel("Occam header", DT::dataTableOutput('outfile.table'))
    )
  })
  
  
  
  # create a character vector of shiny inputs
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- numeric(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }
  
  # obtain the values of inputs
  shinyValue <- function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
      value <- input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))
  }
  
  extend <- function(alphabet) function(i) {
    base10toA <- function(n, A) {
      stopifnot(n >= 0L)
      N <- length(A)
      j <- n %/% N 
      if (j == 0L) A[n + 1L] else paste0(Recall(j - 1L, A), A[n %% N + 1L])
    }   
    vapply(i-1L, base10toA, character(1L), alphabet)
  }
  MORELETTERS <- extend(LETTERS)
  
  # store ordinality as this is reused
  init.ordinality <- reactive({
    init.ordinality <- lengths(lapply(filedata(), unique))
  })
  
  # dataframe for displayed interface table
  it.df <- eventReactive((input$datafile), {
    df <- filedata()
    data.frame(
      Include = shinyInput(checkboxInput, ncol(df), "rows.selected",
                           value = FALSE, width = "100%"),
      "Variable name" = names(df),
      "Re-name" = shinyInput(textInput, ncol(df), "new.vars",
                             width = "100%"),
      Ordinality = init.ordinality(),
      Bins = shinyInput(numericInput, ncol(df), "bin.values",
                        value = NULL, width = "100%", min = 2,
                        max = 12),
      "Bin Type" = shinyInput(selectInput, ncol(df), "bin.type",
                              choices = c("Equal interval" = "interval",
                                          "Equal sample" = "distro",
                                          "Clusters (Jenkins)" = "clusters"),
                              selected = "interval", width = "100%"),
      "Bin NA" = shinyInput(selectInput, ncol(df), "na.handling",
                            choices = c("." = ".", "0" = 0), 
                            selected = ".", width = "100%"),
      Direction = shinyInput(selectInput, ncol(df), "direction",
                             choices = c("IV" = 1, "DV" = 2,
                                         "Pass" = 0),
                             selected = 1, width = "100%"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  
  # update checkboxes for select all
  # not working - does not render the table with update
  #  observeEvent(input$change.selection == 1, {
  #      updateCheckboxInput(session = session,
  #                          inputId = "rows.selected",
  #                          value = input$change.selection)
  #    }
  #  )
  
  # display interface table
  output$interface.table <- DT::renderDataTable(
    it.df(), rownames = FALSE, escape = FALSE, selection = "none",
    options = list(autoWidth = TRUE, scrollY = 200,
                   dom = "t", ordering = FALSE, paging = FALSE,
                   preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
                   drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "))
  )
  
  # function for dynamically renaming columns for Occam
  extend <- function(alphabet) function(i) {
    base10toA <- function(n, A) {
      stopifnot(n >= 0L)
      N <- length(A)
      j <- n %/% N 
      if (j == 0L) A[n + 1L] else paste0(Recall(j - 1L, A), A[n %% N + 1L])
    }   
    vapply(i-1L, base10toA, character(1L), alphabet)
  }
  moreletters <- extend(letters)
  
  # store binDirection as this is reused
  binDirection <- reactive({
    binDirection <- shinyValue("direction", ncol(filedata()))
  })
  
  # store it.rows.selected as this is reused
  it.rows.selected <- reactive({
    it.rows.selected <- shinyValue("rows.selected", ncol(filedata()))
  })
  
  # gather contents of user input from interface table
  it.data <- reactive({
    #    req(input$do)
    it.data <- data.frame(
      "Re-name" = shinyValue("new.vars", ncol(filedata())),
      Ordinality = init.ordinality(),
      Bins = as.integer(shinyValue("bin.values", ncol(filedata()))),
      "Bin Type" = shinyValue("bin.type", ncol(filedata())),
      "Bin NA" = shinyValue("na.handling", ncol(filedata())),
      Direction = binDirection(),
      stringsAsFactors = FALSE
    ) %>%
      .[it.rows.selected(),]
  })
  
  # display contents of user input from interface table
  output$it.contents <- DT::renderDataTable(
    it.data(),
    options = list(autoWidth = TRUE, 
                   scrollX = TRUE, dom = "ltip",
                   ordering = FALSE,
                   lengthMenu = list(c(3,10,20), 
                                     list("3", "10", "20")),
                   rownames = FALSE, selection = "none")
  )
  
  # New variable names assigned here. User input has precendence
  # otherwise dynamically assigned with the DV as Z.
  renameVars <- reactive({
    var.names <- it.data()[,1]
    bin.dir <- it.data()[,6]
    req(sum(bin.dir == 2) %in% c(0,1), length(it.rows.selected()) > 1)
    new.name <- var.names
    sum.blank <- sum(new.name == "")
    if (sum(bin.dir == 2) == 1) {
      if (var.names[which(bin.dir == 2)] == "") {
        new.name[which(bin.dir == 2)] <- "Z"
        if (sum.blank > 25) {
          new.name[new.name == ""] <- MORELETTERS(c(1:25,27:sum.blank))
        }
      }
    }
    new.name[new.name == ""] <- MORELETTERS(seq(sum(new.name == "")))
    return(new.name)
  })
  
  it.data.ready <- reactive({
    it.data.ready <- cbind("Re-name" = renameVars(), it.data()[,2:6])
  })
  
  # display contents of user interface table from selected rows
  # with new variable names
  output$it.ready.contents <- DT::renderDataTable(
    it.data.ready(),
    options = list(autoWidth = TRUE, 
                   scrollX = TRUE, dom = "ltip",
                   ordering = FALSE,
                   lengthMenu = list(c(3,10,20), 
                                     list("3", "10", "20")),
                   rownames = FALSE, selection = "none")
  )  
  
  # contents subset from rows.selected
  rs.contents <- reactive({
    req(input$do)
    rs.contents <- filedata()[,it.rows.selected()]
  })
  
  # show that the rows selected match the column entries
  output$table.selected <- DT::renderDataTable(
    rs.contents(),
    options = list(autoWidth = TRUE, 
                   scrollX = TRUE, dom = "ltip",
                   ordering = FALSE,
                   lengthMenu = list(c(3,10,20), 
                                     list("3", "10", "20")),
                   rownames = FALSE, selection = "none")
  )
  
  # store bin.target as this is reused
  bin.target <- reactive({
    req(input$do)
    df <- it.data.ready()
    bin.target <- integer()
    for (i in seq(nrow(df))) {
      bin.target[i] <- min(df[i,2], df[i,3], na.rm = TRUE)
    }
    return(bin.target)
  })
  
  # data is binned here, this is also reused
  bin.df <- reactive({
    dataBinning(rs.contents(), it.data.ready()[,4], bin.target(), it.data.ready()[,5])
  })
  
  # show sample results from binning
  output$binned.table <- DT::renderDataTable(
    bin.df(),
    options = list(autoWidth = TRUE, 
                   scrollX = TRUE, dom = "ltip",
                   ordering = FALSE,
                   lengthMenu = list(c(3,10,20), 
                                     list("3", "10", "20")),
                   rownames = FALSE, selection = "none")
  )
  
  # calculate table that gets passed to text output file
  output.file <- reactive({
    output.file <- cbind(names(rs.contents()), bin.target(), it.data.ready()[,c(6,1)])
  })
  
  # test to show the output.file table
  output$outfile.table <- DT::renderDataTable(
    output.file(),
    options = list(autoWidth = TRUE, 
                   scrollX = TRUE, dom = "ltip",
                   ordering = FALSE,
                   lengthMenu = list(c(3,10,20), 
                                     list("3", "10", "20")),
                   rownames = FALSE, selection = "none")
  )
  
  # prepare and handle downloading of the output text file
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(sub(".csv", "-input.txt", input$datafile), sep = "")
    },
    content = function(file) {
      writeOccamInputFile(output.file(), bin.df(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

