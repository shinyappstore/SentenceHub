# Random sentence generator

# prepare environment
quickcode::clean()
quickcode::libraryAll(shiny, nextGenShinyApps, shinyjs, shinyStorePlus,clear = TRUE)

# app.version
app.version <- 0.4

# data import a 1023 sentence database
.data.02 <- readLines("www/random.txt")

# application UI object

ui <- fluidPage(
  # Theme: Select color style from 1-13
  style = "9",

  # add custom background
  # custom.bg.color = "brown",

  # add scripts
  tags$head(
    tags$script(src = "script.js"),
    tags$link(rel = "stylesheet", href = "style.css")
  ),

  # Header: Insert header content using titlePanel ------------
  header = titlePanel(left = paste0("Random Sentence Generator v",app.version), right = "@rpkg.net"),
  useShinyjs(), #use shiny js
  initStore(), #use shinyStorePlus
  row(
    altPanel(
      actionButton("generate", "▶ Generate random sentences", size = "l", bg.type = "success"),
      downloadButton("downloadData", "Download", style="background-color: #666!important; color: white; padding: 14px!important; border-width: 1px; border-radius: 0px; background: unset;"),

      card(
        title = "Key settings",
        numericInput("num_names", "Number of Sentences:", value = 1, min = 1, max = 1000),
        textInput("mustcontain", "Sentences must contain:"),
        hr(),
        div("Note: ShinyStorePlus preserves your entries from previous sessions. These are random sentences database built from sentences generated on www.randomwordgenerator.com")
      ),
      card(
        title = "Other settings",
        collapsed = TRUE,
        numericInput("mincahr", "Minimum characters in sentence:", value = 15, min = 1, max = 20000000),
        numericInput("maxcahr", "Maximum characters in sentence:", value = 1000, min = 2, max = 2000000),
      )

    ),
    mainPanel(
      card(
      title = "Outputs of Random Sentences",
      h2(textOutput("sentences")),
      shiny::tags$span(id="messageRandomNames")
      )

    )
  )
)





# application Server function

server <- function(input, output, session) {
  # Declare variable to hold results
  resultsVar <- reactiveVal(.data.02[1])



  # Generate random sentences when the Generate button is clicked
  observeEvent(input$generate, {

    #set data to fetch from
    remData <- .data.02

    #filter based on must contain
    if(not.null(input$mustcontain)){
      if(nchar(input$mustcontain) > 2){
        remData <- remData[grepl(input$mustcontain,remData)]
      }
    }

    #filter based on least number of characters
    remData <- remData[nchar(remData)>input$mincahr]

    #filter based on max number of characters
    remData <- remData[nchar(remData)<input$maxcahr]

    #shuffle remainder based on how many needed

    fetchrandoms <- sample(1:length(remData),as.integer(input$num_names), replace = TRUE)
    remData <- remData[fetchrandoms]




    #retrurn results
    resultsVar(remData)
  })

  # show results
  output$sentences <- renderText({
    resultsVar()
  })


  # download it
  output$downloadData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("randomsentence_",Sys.Date(), "_data.doc")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      writeLines(resultsVar(), file)
    }
  )

  #insert at the bottom  !!!IMPORTANT
  appid = "randomnamesapp"
  setupStorage(appId = appid,inputs = TRUE)

}



shinyApp(ui, server)
