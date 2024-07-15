library(tidyverse)
library(shinyTime)
library(shinythemes)
library(ggrepel)
library(DT)
library(Cairo)
library(shinyscreenshot)


# Define UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  #shinythemes::themeSelector(),
  titlePanel("Indication for Phototherapy"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      tabsetPanel(
        type = "tab",
        id = "main",
        tabPanel(
          "Patient Data",
          conditionalPanel(
            condition = "(input.advanced == 'no')",
            selectInput("premature", "Premature < 35 weeks:",
                        c("no" = "no",
                          "yes" = "yes")),
          ),
          selectInput("bili_risk", "Risk Factors Present",
                      choices = c("choose" = "choose", "no" = "no",
                                  "yes" = "yes")),
          conditionalPanel(condition = "input.birthdate == '2022-00-00'",
                           textInput("name", "Name", value = "name")),
          conditionalPanel(
            condition = "(input.advanced == 'yes')",
            p(
              "Advanced settings selected, manual input not possible. Click",
              a("here", href = "http://rubenvp.shinyapps.io/bilicurve"),
              "to be redirected to the application with manual input."
            )
          ),
          conditionalPanel(
            condition = "(input.premature == 'no' && input.advanced == 'no')",
            dateInput(
              inputId = 'birthdate',
              label = 'Birthdate (yyyy-mm-dd)',
              value = Sys.Date()
            ),
            textInput("birthtime", "Birth Time", value = "00:01"),
            textInput("GA_birth", "GA at birth (format = \"36+1/7\"). Error for values < 35+0/7.", value = NA),
            fileInput(
              'file_term',
              'Upload previously saved table for term',
              accept = c(".xlsx")
            ),
          ),
          conditionalPanel(
            condition = "(input.premature == 'yes' && input.advanced == 'no')",
            fileInput(
              'file_preterm',
              'Upload previously saved table for preterm',
              accept = c(".xlsx")
            )
          )
        ),
        tabPanel(
          "Term - Bilirubin Values",
          conditionalPanel(
            condition = "(input.premature == 'no' && input.advanced == 'no')",
            fluidRow(
              h4("Sampling Moment 1"),
              column(7,
                     dateInput(
                       inputId = 'samplingdate1',
                       label = 'Date',
                       value = Sys.Date()
                     )),
              column(5,
                     textInput("samplingtime1", "Time", value = "10:00")),
              column(7,
                     numericInput(
                       "bili1",
                       "Bilirubin (mg/dL)",
                       0,
                       min = 0,
                       max = 100
                     ))),
            fluidRow(
              h4("Sampling Moment 2"),
              column(7,
                     dateInput(
                       inputId = 'samplingdate2',
                       label = 'Date',
                       value = Sys.Date()
                     )),
              column(5,
                     textInput("samplingtime2", "Time", value = "10:00")),
              column(7,
                     numericInput(
                       "bili2",
                       "Bilirubin (mg/dL)",
                       0,
                       min = 0,
                       max = 100
                     ))),
            fluidRow(
              h4("Sampling Moment 3"),
              column(7,
                     dateInput(
                       inputId = 'samplingdate3',
                       label = 'Date',
                       value = Sys.Date()
                     )),
              column(5,
                     textInput("samplingtime3", "Time", value = "10:00")),
              column(7,
                     numericInput(
                       "bili3",
                       "Bilirubin (mg/dL)",
                       0,
                       min = 0,
                       max = 100
                     ))),
          ),
          conditionalPanel(
            condition = "(input.premature == 'yes' || input.advanced == 'yes')",
            h4("Error:"),
            p("Premature curve or advanced settings selected.")
          ),
        ),
        tabPanel(
          "Preterm - Bilirubin Values",
          conditionalPanel(
            condition = "(input.premature == 'yes' && input.advanced == 'no')",
            h4("Sampling Moment 1"),
            textInput("GA1", "GA (format = \"23+1/7\")", value = "23+1/7"),
            numericInput(
              "biliprem1",
              "Bilirubin in mg/dL",
              0,
              min = 0,
              max = 100
            ),
            hr(),
            h4("Sampling Moment 2"),
            textInput("GA2", "GA", value = "23+1/7"),
            numericInput(
              "biliprem2",
              "Bilirubin in mg/dL",
              0,
              min = 0,
              max = 100
            ),
            hr(),
            h4("Sampling Moment 3"),
            textInput("GA3", "GA", value = "23+1/7"),
            numericInput(
              "biliprem3",
              "Bilirubin in mg/dL",
              0,
              min = 0,
              max = 100
            ),
          ),
          conditionalPanel(
            condition = "(input.premature == 'no' || input.advanced == 'yes')",
            h4("Error:"),
            p("Term curve or advanced settings selected.")
          ),
        ),
        tabPanel(
          "Advanced",
          selectInput(
            "advanced",
            "Advanced settings:",
            c("no" = "no",
              "yes" = "yes")
          ),
          conditionalPanel(
            condition = "input.advanced == 'yes'",
            h4("Error"),
            p("Advanced settings selected, manual input not possible. Click",
              a("here", href = "http://rubenvp.shinyapps.io/bilicurve"),
              "to be redirected to the application with manual input. The values below are automatically generated and cannot be modified."
            ),
            textInput("birth_GET", "Birthdate and time in CSV (only for curve > 35 weeks)", value = NA),
            textInput("sampling_GET", "Sampling date and time in CSV (only for curve > 35 weeks)", value = NA),
            textInput("GA_birth_GET", "Postmenstrual age at birth (only for curve > 35 weeks)", value = NA),
            textInput("GA_GET", "Postmenstrual age at sampling (only needed for curve < 35 weeks) in CSV", value = NA),
            textInput("bili_GET", "Bilirubin in mg/dL in CSV (both curves)", value = NA),
            textInput("PT_start_GET", "Phototherapy start date+time in CSV", value = NA),
            textInput("PT_stop_GET", "Phototherapy stop date+time in CSV", value = NA),
            textInput("PT_numLamps_GET", "Number of lamps during phototherapy in CSV", value = NA)
          )
        ),
        tabPanel(
          "Error",
          "Advanced settings selected, manual input not possible. Click",
          a("here", href = "http://rubenvp.shinyapps.io/bilicurve"),
          "to be redirected to the application with manual input."
        ),
      ),
    ),
    
    mainPanel(tabsetPanel(
      type = "tabs",
      id = "output",
      tabPanel(
        "Bilirubin Curve",
        conditionalPanel(
          condition = "(input.bilirisk != NA)",
          plotOutput("bilicurve", height = "650px", width = "100%"),
          screenshotButton(label = "Save Figure", id = "bilicurve"),
          hr(),
          #h4("Entered Values"),
          DT::dataTableOutput("time_output1"),
          hr(),
          h4("Disclaimer", style = "font-size:12px;"),
          p(
            "Authors: Ruben Van Paemel, Kris De Coen, Sophie Vanhaesebrouck (NICU Ghent University Hospital). This tool has not been extensively tested, so verify with the original curves before initiating therapy (included above). For questions or suggestions or bugs, e-mail ruben.vanpaemel@ugent.be. For infants born close to 35 weeks, cut-offs from the term graph were added, where the upper border of the box = infants > 35 weeks with no risk factor and the lower border = infants > 35 weeks with risk factors (each box represents 1 day after 35 weeks, ending at 35+6/7). Source: Kemper AR, Newman TB, Slaughter JL, et al. Clinical Practice Guideline Revision: Management of Hyperbilirubinemia in the Newborn Infant 35 or More Weeks of Gestation. Pediatrics. 2022;150(3):e2022058859. doi:10.1542/peds.2022-058859 and Maisels MJ, Watchko JF, Bhutani VK, Stevenson DK. An approach to the management of hyperbilirubinemia in the preterm infant less than 35 weeks of gestation. Journal of Perinatology 2012;32:660-4. De Luca D, Romagnoli C, Tiberi E, Zuppa AA, Zecca E. Skin bilirubin nomogram for the first 96 h of life in a European normal healthy newborn population, obtained with multiwavelength transcutaneous bilirubinometry. Acta Paediatr. 2008 Feb;97(2):146-50. doi: 10.1111/j.1651-2227.2007.00622.x. PMID: 18254903. The code and documentation is available at https://github.com/rmvpaeme/bilicurve-shiny ."
            ,
            style = "font-size:12px;"
          ),
          hr()
        )),
      tabPanel(
        "Choose an Option",
        img(
          src = 'notification.png',
          width = "600px",
          height = "600px"
        )),
      tabPanel(
        "Original Curves",
        img(
          src = 'bili_RF.png',
          width = "100%",
          height = "100%"
        ),
        img(
          src = 'bili_noRF.png',
          width = "100%",
          height = "100%"
        ),
        img(
          src = 'FT.PNG',
          width = "90%",
          height = "90%"
        )
      ),
      tabPanel(
        "Usage",
        p("Documentation on advanced usage can be found on", a("https://github.com/rmvpaeme/bilicurve-shiny/", href = "https://github.com/rmvpaeme/bilicurve-shiny/"))
      )
    ))
  )
)


server <- function(input, output, session) {options(shiny.usecairo=TRUE)
  observeEvent(input$premature, {
    if (input$premature == "yes") {
      showTab(inputId = "main", target = "Preterm - Bilirubin Values")
      showTab(inputId = "main", target = "Preterm - Sampling Time 2")
      showTab(inputId = "main", target = "Preterm - Sampling Time 3")
      hideTab(inputId = "main", target = "Term - Sampling Time 3")
      hideTab(inputId = "main", target = "Term - Sampling Time 2")
      hideTab(inputId = "main", target = "Term - Bilirubin Values")
    } else if (input$premature == "no") {
      hideTab(inputId = "main", target = "Preterm - Bilirubin Values")
      hideTab(inputId = "main", target = "Preterm - Sampling Time 2")
      hideTab(inputId = "main", target = "Preterm - Sampling Time 3")
      showTab(inputId = "main", target = "Term - Sampling Time 3")
      showTab(inputId = "main", target = "Term - Sampling Time 2")
      showTab(inputId = "main", target = "Term - Bilirubin Values")
    }
  })
  
  
  observeEvent(input$bili_risk, {
    if (input$bili_risk == "choose") {
      hideTab(inputId = "output", target = "Bilirubin Curve")
      showTab(inputId = "output", target = "Choose an Option")
    } else {
      showTab(inputId = "output", target = "Bilirubin Curve")
      hideTab(inputId = "output", target = "Choose an Option")
    }
  })
  
  observeEvent(input$advanced, {
    if (input$advanced == "yes") {
      hideTab(inputId = "main", target = "Preterm - Bilirubin Values")
      hideTab(inputId = "main", target = "Preterm - Sampling Time 2")
      hideTab(inputId = "main", target = "Preterm - Sampling Time 3")
      hideTab(inputId = "main", target = "Term - Sampling Time 3")
      hideTab(inputId = "main", target = "Term - Sampling Time 2")
      hideTab(inputId = "main", target = "Term - Bilirubin Values")
      #hideTab(inputId = "main", target = "Advanced")
      hideTab(inputId = "main", target = "Error")
      #hideTab(inputId = "main", target = "Patient Data")
    } else{
      hideTab(inputId = "main", target = "Error")
    }
  })
  
  # format text to yyy-mm-dd hh:mm:ss
  vals <- reactiveValues()
  
  
  observe({
    inFile <- input$file_term
    if (is.null(inFile)) {
      testdatetime <- paste(input$birthdate, input$birthtime)
      testdatetime <-
        as.POSIXct(testdatetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
      vals$initial_date <- testdatetime
    }
    else {
      df_datetime <- readxl::read_excel(inFile$datapath, skip = 2)
      vals$initial_date <-
        df_datetime %>% pull(birth) %>% first()
      updateDateInput(session, "birthdate", value = vals$initial_date)
      updateTextInput(session, "birthtime", value = strsplit(vals$initial_date, " ")[[1]][2])
      GA_update <-
        df_datetime %>% pull(`GA at birth`) %>% first()
      updateTextInput(session, "GA_birth", value = GA_update)
      risk_update <- df_datetime %>% pull(`risk factors`) %>% first()
      updateSelectInput(session, "bili_risk", selected = risk_update)
    }
    
  })
  
  observe({
    testdatetime2 <- paste(input$samplingdate1, input$samplingtime1)
    testdatetime2 <-
      as.POSIXct(testdatetime2, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date1 <- testdatetime2
  })
  
  observe({
    testdatetime3 <- paste(input$samplingdate2, input$samplingtime2)
    testdatetime3 <-
      as.POSIXct(testdatetime3, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date2 <- testdatetime3
  })
  
  observe({
    testdatetime4 <- paste(input$samplingdate3, input$samplingtime3)
    testdatetime4 <-
      as.POSIXct(testdatetime4, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date3 <- testdatetime4
    test4 <- inherits(testdatetime4, "POSIXct")
    print(test4)
  })
  
  # placeholder code to expand the manual input to 7 points
  observe({
    testdatetime5 <- paste(input$samplingdate4, input$samplingtime4)
    testdatetime5 <-
      as.POSIXct(testdatetime5, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date4 <- testdatetime5
  })
  
  
  observe({
    testdatetime6 <- paste(input$samplingdate5, input$samplingtime5)
    testdatetime6 <-
      as.POSIXct(testdatetime6, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date5 <- testdatetime6
  })
  
  observe({
    testdatetime7 <- paste(input$samplingdate6, input$samplingtime6)
    testdatetime7 <-
      as.POSIXct(testdatetime7, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date6 <- testdatetime7
  })
  
  observe({
    testdatetime8 <- paste(input$samplingdate7, input$samplingtime7)
    testdatetime8 <-
      as.POSIXct(testdatetime8, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date7 <- testdatetime8
  })
  
  observe({
    vals$bilirubin  <- as.double(input$bili1)
    vals$bilirubin2  <- as.double(input$bili2)
    vals$bilirubin3  <- as.double(input$bili3)
    vals$bilirubin4  <- as.double(input$bili4)
    vals$bilirubin5  <- as.double(input$bili5)
    vals$bilirubin6  <- as.double(input$bili6)
    vals$bilirubin7  <- as.double(input$bili7)
  })
  # format e.g. 23+1/7 to 23.14
  calc <- function(x)
    eval(parse(text = x))
  
  newData <- reactive({
    # parse GET request
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['name']])) {
      updateTextInput(session, "name", value = query[['name']])
    }
    if (!is.null(query[['advanced']])) {
      updateTextInput(session, "advanced", value = query[['advanced']])
    }
    if (!is.null(query[['premature']])) {
      updateTextInput(session, "premature", value = query[['premature']])
    }
    if (!is.null(query[['birth_GET']])) {
      updateTextInput(session, "birth_GET", value = query[['birth_GET']])
    }
    if (!is.null(query[['sampling _GET']])) {
      updateTextInput(session, "sampling_GET", value = query[['sampling_GET']])
    }
    if (!is.null(query[['samplingdate1']])) {
      updateTextInput(session, "samplingdate1", value = query[['samplingdate1']])
    }
    if (!is.null(query[['samplingtime1']])) {
      updateTextInput(session, "samplingtime1", value = query[['samplingtime1']])
    }
    if (!is.null(query[['samplingdate2']])) {
      updateTextInput(session, "samplingdate2", value = query[['samplingdate2']])
    }
    if (!is.null(query[['samplingtime2']])) {
      updateTextInput(session, "samplingtime2", value = query[['samplingtime2']])
    }
    if (!is.null(query[['samplingdate3']])) {
      updateTextInput(session, "samplingdate3", value = query[['samplingdate3']])
    }
    if (!is.null(query[['samplingtime3']])) {
      updateTextInput(session, "samplingtime3", value = query[['samplingtime3']])
    }
    if (!is.null(query[['bili_GET']])) {
      updateTextInput(session, "bili_GET", value = query[['bili_GET']])
    }
    if (!is.null(query[['bili1']])) {
      updateTextInput(session, "bili1", value = query[['bili1']])
    }
    if (!is.null(query[['bili2']])) {
      updateTextInput(session, "bili2", value = query[['bili2']])
    }
    if (!is.null(query[['bili3']])) {
      updateTextInput(session, "bili3", value = query[['bili3']])
    }
    if (!is.null(query[['GA_GET']])) {
      updateTextInput(session, "GA_GET", value = query[['GA_GET']])
    }
    if (!is.null(query[['PT_start_GET']])) {
      updateTextInput(session, "PT_start_GET", value = query[['PT_start_GET']])
    }
    if (!is.null(query[['PT_stop_GET']])) {
      updateTextInput(session, "PT_stop_GET", value = query[['PT_stop_GET']])
    }
    if (!is.null(query[['PT_numLamps_GET']])) {
      updateTextInput(session, "PT_numLamps_GET", value = query[['PT_numLamps_GET']])
    }
    if (!is.null(query[['GA_birth_GET']])) {
      updateTextInput(session, "GA_birth_GET", value = query[['GA_birth_GET']])
    }
    
    name <- as.character(input$name)
    value <- as.character(vals$initial_date)
    value1 <-
      as.character(difftime(vals$to_date1, vals$initial_date, units = "days"))
    value2 <-
      as.character(difftime(vals$to_date2, vals$initial_date, units = "days"))
    value3 <-
      as.character(difftime(vals$to_date3, vals$initial_date, units = "days"))
    value4 <-
      as.character(difftime(vals$to_date4, vals$initial_date, units = "days"))
    value5 <-
      as.character(difftime(vals$to_date5, vals$initial_date, units = "days"))
    value6 <-
      as.character(difftime(vals$to_date6, vals$initial_date, units = "days"))
    value7 <-
      as.character(difftime(vals$to_date7, vals$initial_date, units = "days"))
    
    bili1 <- vals$bilirubin
    bili2 <- vals$bilirubin2
    bili3 <- vals$bilirubin3
    bili4 <- vals$bilirubin4
    bili5 <- vals$bilirubin5
    bili6 <- vals$bilirubin6
    bili7 <- vals$bilirubin7
    
    if (nchar(value) == nchar(as.character(Sys.Date()))) {
      value <- paste(value, "00:00:00 ")
    }
    
    if (input$advanced == "yes") {
      #birth_GET <- c("2023-11-22 10:00:00")
      birth_GET <- as.character(input$birth_GET)
      GA_birth_GET <- as.character(input$GA_birth_GET)
      #sampling_GET <- c("2023-11-23 10:00:00,2023-11-24 10:00:00")
      sampling_GET <- as.character(input$sampling_GET)
      #bili_GET <- c("10,9")
      bili_GET <- as.character(input$bili_GET)
      #GA_GET <- c("23+1/7,24+1/7")
      GA_GET <- as.character(input$GA_GET)
      #PT_start_GET <- c("2023-11-23 11:00:00,2023-11-24 12:00:00,2023-11-25 12:00:00")
      #PT_start_GET <- c("24+1/7,25+1/7,26+1/7,27+1/7")
      #PT_start_GET <- c("24+3/7,24+5/7,24+6/7,25+0/7,25+0/7,25+2/7,25+3/7,25+6/7,26+0/7,26+1/7")
      PT_start_GET <- as.character(input$PT_start_GET)
      #PT_numLamps_GET <- c("1,2,1")
      #PT_numLamps_GET <- c("1,1,2,2,3,1,2,1,3,2")
      PT_numLamps_GET <- as.character(input$PT_numLamps_GET)
      #PT_stop_GET <- as.character(input$PT_stop_GET)
      PT_stop_GET <- as.character(input$PT_stop_GET)
      #PT_stop_GET_split <- as.character(unlist(strsplit(PT_stop_GET, split = ",")))
      #PT_numLamps_GET_split <- as.character(unlist(strsplit(PT_numLamps_GET, split = ",")))
      annotation <- "sample"
      
      if (input$premature == "no") {
        birth_GET_POSIX <-
          as.POSIXct(unlist(strsplit(birth_GET, split = ",")), format = "%Y-%m-%d %H:%M", tz = "UTC")
        sampling_GET_POSIX <-
          as.POSIXct(unlist(strsplit(sampling_GET, split = ",")), format = "%Y-%m-%d %H:%M", tz = "UTC")
        GA_birth_GET <- calc(unlist(strsplit(GA_birth_GET, split = ",")))
        
        PT_start_GET_POSIX <-
          as.POSIXct(unlist(strsplit(PT_start_GET, split = ",")), format = "%Y-%m-%d %H:%M", tz = "UTC")
        PT_stop_GET_POSIX <-
          as.POSIXct(unlist(strsplit(PT_stop_GET, split = ",")), format = "%Y-%m-%d %H:%M", tz = "UTC")
        PT_numLamps_GET_split <- as.numeric(unlist(strsplit(PT_numLamps_GET, split = ",")))
        df_PT <- tibble(
          PT_start = PT_start_GET_POSIX,
          PT_stop = PT_stop_GET_POSIX,
          birth = birth_GET_POSIX,
          PT_numLamps = PT_numLamps_GET_split,
          specified = TRUE
        )
        df_PT$diff_days_PT_start <-
          as.character(difftime(df_PT$PT_start, df_PT$birth, units = "days"))
        df_PT$diff_days_PT_stop <-
          as.character(difftime(df_PT$PT_stop, df_PT$birth, units = "days"))
        
        bili_GET_split <-
          as.numeric(unlist(strsplit(bili_GET, split = ",")))
        df_GET <-
          tibble(
            birth = birth_GET_POSIX,
            sampling = sampling_GET_POSIX,
            bili = bili_GET_split,
            annotation = annotation
          )
        
        df_GET$diff_hours <-
          as.character(difftime(df_GET$sampling, df_GET$birth, units = "hours"))
        df_GET$diff_days <-
          as.character(difftime(df_GET$sampling, df_GET$birth, units = "days"))
        df2 <-
          df_GET %>%
          mutate(
            birth = as.character(as.POSIXct(birth_GET_POSIX, origin = "1970-01-01", tz = "UTC")),
            samplingmoment = as.character(as.POSIXct(sampling_GET_POSIX, origin = "1970-01-01", tz = "UTC")),
            `time in hours` = as.double(diff_hours),
            `time in days` = as.double(diff_days),
            bili_value = as.double(bili),
            `GA at birth` = as.double(GA_birth_GET),
            annotation = "sample"
          ) %>%
          dplyr::select(
            birth,
            samplingmoment,
            `time in hours`,
            `time in days`,
            `GA at birth`,
            bili_value,
            annotation
          )
        list(df = df2, df_PT = df_PT)
      }
      else {
        PT_start_GET_split <- as.character(unlist(strsplit(PT_start_GET, split = ",")))
        PT_stop_GET_split <- as.character(unlist(strsplit(PT_stop_GET, split = ",")))
        PT_numLamps_GET_split <- as.character(unlist(strsplit(PT_numLamps_GET, split = ",")))
        
        if (all(sapply(list(
          length(PT_start_GET_split),
          length(PT_stop_GET_split),
          length(PT_numLamps_GET_split)
        ), function(x)
          x == length(PT_numLamps_GET_split)))) {
          df_PT <- tibble(
            PT_start = PT_start_GET_split,
            PT_stop = PT_stop_GET_split,
            PT_numLamps = PT_numLamps_GET_split,
            specified = TRUE
          )
          df_PT <-
            df_PT %>%
            rowwise() %>%
            mutate(PT_start = calc(PT_start)) %>%
            mutate(PT_stop = calc(PT_stop) + 0.035)
        }
        else {
          df_PT <- tibble(PT_start = NA,
                          PT_stop = NA,
                          PT_numLamps = NA,
                          specified = FALSE)
        }
        
        df2 <- tibble(time_HR = NA,
                      value = NA,
                      annotation = "sample")
        bili_GET_split <-
          as.numeric(unlist(strsplit(bili_GET, split = ",")))
        GA_GET <-
          as.character(unlist(strsplit(GA_GET, split = ",")))
        preterm_df <- tibble(GA_GET = GA_GET,
                             biliprem = bili_GET_split)
        preterm_df <-
          preterm_df %>%
          rowwise() %>%
          mutate(GA = calc(GA_GET))
        preterm_df <-
          preterm_df %>%
          mutate(`postmenstrual age` = GA,
                 bili_value = biliprem) %>%
          dplyr::select(`postmenstrual age`, bili_value) %>%
          filter(bili_value > 0)
        list(df = preterm_df, df_PT = df_PT)
      }
    }
    else if (input$premature == "no") {
      df2 <-
        tibble(
          birth = as.character(as.POSIXct(c(
            vals$initial_date
          ), origin = "1970-01-01", tz = "UTC")),
          samplingmoment = as.character(as.POSIXct(c(
            vals$to_date1,
            vals$to_date2,
            vals$to_date3,
            vals$to_date4,
            vals$to_date5,
            vals$to_date6,
            vals$to_date7
          ), origin = "1970-01-01", tz = "UTC")),
          `time in days` = as.double(c(
            value1,
            value2,
            value3,
            value4,
            value5,
            value6,
            value7
          )),
          `time in hours` = as.double(c(value1, value2, value3, value4, value5, value6, value7)) * 24,
          bili_value = as.double(c(
            bili1,
            bili2,
            bili3,
            bili4,
            bili5,
            bili6,
            bili7
          )),
          annotation = "sample",
          `GA at birth` = input$GA_birth
        )
      list(df = df2)
    }
    else if (input$premature == "yes") {
      preterm_df <-
        tibble(
          `postmenstrual age` = c(calc(input$GA1), calc(input$GA2), calc(input$GA3)),
          bili_value = c(input$biliprem1, input$biliprem2, input$biliprem3)
        )
      list(df = preterm_df)
    }
  })
  
  output$time_output1 <- DT::renderDataTable({
    inFile_term <- input$file_term
    inFile_preterm <- input$file_preterm
    if (is.null(inFile_term) && is.null(inFile_preterm)) {
      df <- newData()$df
    }
    else {
      if (!is.null(inFile_term) && input$premature == "no") {
        df <- readxl::read_excel(inFile_term$datapath, skip = 2)
        df$`GA at birth` <- as.character(df$`GA at birth`)
        df <- bind_rows(df, newData()$df)
      }
      else if (!is.null(inFile_preterm) &&
               input$premature == "yes") {
        df <- readxl::read_excel(inFile_preterm$datapath, skip = 2)
        df <- bind_rows(df, newData()$df)
      }
    }
    
    df <- df %>% filter(bili_value > 0)
    df <- df %>% mutate_if(is.numeric, ~ round(., 2))
    
    df$risk_factors <- input$bili_risk
    DT::datatable({
      df
    },
    caption = "You can save the table via the Excel button to later import it back into the tool to add more values. Important: make no self-modifications to the Excel.",
    extensions = 'Buttons',
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'frtBip',
      buttons = list(list(extend = "excel",
                          text = "Save as Excel file"))
    ),
    rownames = FALSE,
    
    class = "display")
  })
  
  
  output$bilicurve <- renderPlot({
    if (input$premature == "no") {
      df <- tibble(`time in days` = NA, time = NA, bili_value = NA, bilirubin = NA, annotation = NA, highlight = NA)
      
      inFile <- input$file_term
      if (is.null(inFile)) {
        df2 <- newData()$df
      } else {
        df2 <- readxl::read_excel(inFile$datapath, skip = 2)
        df2$`GA at birth` <- as.character(df2$`GA at birth`)
        df2 <- bind_rows(df2, newData()$df)
      }
      
      ggplot_text <- "Loading..."
      
      # read the dataframe 
      GA_birth <- df2 %>% pull(`GA at birth`) %>% first()
      if (input$bili_risk == "no") {
        highlight <- NA
        ggplot_text <- "No Hyperbilirubinemia Neurotoxicity Risk Factors"
        df <- read_tsv("./data/all_norisk.tsv")
        df <- df %>% filter(!is.na(bilirubin))
        df <- df %>% arrange(annotation, time)
        df <- df %>% 
          group_by(annotation) %>%
          filter(!duplicated(bilirubin))
        max_vals <- df %>% group_by(annotation) %>% summarise(bilirubin = max(bilirubin, na.rm = TRUE)) %>% mutate(time = 336) %>% filter(!is.na(annotation))
        df <- rbind(max_vals, df)
        
        if (GA_birth < 36 && GA_birth >= 35) {
          df$annotation <- sub("35w_norisk", "35 weeks", df$annotation)
          highlight <- "35 weeks"
        } else if (GA_birth < 37 && GA_birth >= 36) {
          df$annotation <- sub("36w_norisk", "36 weeks", df$annotation)
          highlight <- "36 weeks"
        } else if (GA_birth < 38 && GA_birth >= 37) {
          df$annotation <- sub("37w_norisk", "37 weeks", df$annotation)
          highlight <- "37 weeks"
        } else if (GA_birth < 39 && GA_birth >= 38) {
          df$annotation <- sub("38w_norisk", "38 weeks", df$annotation)
          highlight <- "38 weeks"
        } else if (GA_birth < 40 && GA_birth >= 39) {
          df$annotation <- sub("39w_norisk", "39 weeks", df$annotation)
          highlight <- "39 weeks"
        } else if (GA_birth >= 40) {
          df$annotation <- sub("40w_norisk", ">= 40 weeks", df$annotation)
          highlight <- ">= 40 weeks"
        }
      } else if (input$bili_risk == "yes") {
        highlight <- NA
        ggplot_text <- "One or More Hyperbilirubinemia Neurotoxicity Risk Factors"
        df <- read_tsv("./data/all_risk.tsv")
        df <- df %>% filter(!is.na(bilirubin))
        df <- df %>% arrange(annotation, time)
        df <- df %>% 
          group_by(annotation, bilirubin) %>%
          filter(!duplicated(bilirubin))
        max_vals <- df %>% group_by(annotation) %>% summarise(bilirubin = max(bilirubin, na.rm = TRUE)) %>% mutate(time = 336) %>% filter(!is.na(annotation))
        df <- rbind(max_vals, df)
        
        if (GA_birth < 36 && GA_birth >= 35) {
          df$annotation <- sub("35w_risk", "35 weeks", df$annotation)
          highlight <- "35 weeks"
        } else if (GA_birth < 37 && GA_birth >= 36) {
          df$annotation <- sub("36w_risk", "36 weeks", df$annotation)
          highlight <- "36 weeks"
        } else if (GA_birth < 38 && GA_birth >= 37) {
        df$annotation <- sub("37w_risk", "37 weeks", df$annotation)
        highlight <- "37 weeks"
      } else if (GA_birth >= 38) {
        df$annotation <- sub("38w_risk", ">= 38 weeks", df$annotation)
        highlight <- ">= 38 weeks"          
      }
    }
    
    df <-
      df %>% mutate(`time in days` = time/24, bili_value = bilirubin) %>% select(-c(bilirubin, time))
    
    df <- bind_rows(df, df2)
    df_TcB = tibble(
      annotation = "threshold for serum confirmation of TcB if no risk factors",
      `time in days` = c(1, 1.5, 2, 3, 4),
      bili_value = c(8, 10, 12, 14, 17)
    )
    df_all <- bind_rows(df, df_TcB)
    x_seq = df2 %>% filter(bili_value > 0) %>% pull(`time in days`)
    
    intersections <- df_all %>% filter(annotation != "sample") %>% filter(annotation == highlight | annotation == "threshold for serum confirmation of TcB if no risk factors") %>%
      group_by(annotation) %>%
      dplyr::reframe(interpolated = approx(x = `time in days`, y = bili_value, xout = x_seq)$y) %>%
      mutate(x_seq = rep(x_seq, 2)) %>%
      arrange(annotation, x_seq) %>%
      group_by(annotation) %>%
      summarise(across(everything(), last)) %>% mutate(interpolated = round(interpolated, 1))
    
    last_intersect <- intersections %>% pull(x_seq) %>% unique()
  }
  
  else if (input$premature == "yes") {
    inFile <- input$file_preterm
    if (is.null(inFile)) {
      preterm_df <- newData()$df
    }
    else {
      preterm_df <- readxl::read_excel(inFile$datapath, skip = 2)
      preterm_df <- bind_rows(preterm_df, newData()$df)
    }
    
  }
  
  if (input$premature == "no") {
    df_PT <- newData()$df_PT
    if ((length(input$PT_start_GET) == length(input$PT_stop_GET)) &&
        (sum(!is.na(input$PT_stop_GET)) == sum(!is.na(input$PT_start_GET))) && !is.null(df_PT$diff_days_PT_start) ) {
      PT_ggplot <- geom_rect(data = df_PT, aes(xmin = c(as.numeric(diff_days_PT_start)), xmax = c(as.numeric(diff_days_PT_stop)), 
                                               ymin = -Inf, ymax =  Inf, fill = PT_numLamps), 
                             alpha = 0.7, inherit.aes = FALSE) 
      PT_legend <-  scale_fill_manual(labels = c("1 lamp", "2 lamps", "3 lamps"), name = "phototherapy intensity", values = c("1" = "#A3BE8C", "2" = "#EBCB8B", "3" = "#BF616A"))
      
    } else {
      PT_ggplot <- NULL
      PT_legend <- NULL
    }
    
    g <-
      ggplot(df, aes(y = bili_value, x = `time in days`, col = annotation)) +  geom_vline(
        xintercept = last_intersect,
        color = "black",
        linetype = "dashed",
        alpha = 0.3
      ) + 
      geom_text_repel(
        data = intersections,
        show.legend = FALSE,
        size = 5,
        aes(x_seq, interpolated, label = round(interpolated, 2)),
        min.segment.length = 1,
        seed = 42,
        box.padding = 0.5,
        max.overlaps =  Inf,
        nudge_x = 1,
        force = 1
      )  +
      geom_line(data = df %>% filter(annotation != "sample") %>% filter(annotation == highlight), size = 1)  + 
      geom_line(data = df %>% filter(annotation != "sample") %>% filter(annotation != highlight), aes(group = annotation, col = annotation), size = 0.5, color = "gray80")  + 
      theme_bw() + xlab("age in days") + ylab("bilirubin, mg/dL") +
      geom_line(data = df_TcB, linetype = "dashed", size = 1) +
      geom_point(
        data = df %>% filter(annotation == "sample", bili_value > 0),
        aes(y = bili_value, x = `time in days`, col = annotation),
        size = 3, color = "#5E81AC"
      ) + geom_line(data = df %>% filter(annotation == "sample", bili_value > 0), aes(group = annotation), color = "#5E81AC") + labs(color = "legend", subtitle = paste0(ggplot_text, "\n", "°", df2 %>% pull(birth) %>% first()) ) + theme(
        text = element_text(size = 20),
        legend.position = "bottom",
        legend.direction="vertical",
      ) + scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7,8,9,10,11,12,13,14),
                             limits = c(0, 14.1)) +
      scale_y_continuous(breaks = seq(0,22.5, by = 2),
                         limits = c(5, 22.5)) +
      scale_color_manual(values = c( "#4C566A", "#88C0D0")) +
      theme(plot.subtitle=element_text(size=18)) +
      geom_point(data = intersections %>% filter(x_seq > 0) , aes(x = x_seq, y = interpolated)) + PT_ggplot + PT_legend
    
    g + guides(color = guide_legend(nrow = 5))
  } else{
    
    df_PT <- newData()$df_PT
    if ((length(input$PT_start_GET) == length(input$PT_stop_GET)) &&
        (sum(!is.na(input$PT_stop_GET)) == sum(!is.na(input$PT_start_GET))) && !is.null(df_PT$PT_start) ) {
      PT_ggplot <- geom_rect(data = df_PT, aes(xmin = c(as.numeric(PT_start)), xmax = c(as.numeric(PT_stop)), 
                                               ymin = -Inf, ymax =  Inf, fill = PT_numLamps), 
                             alpha = 0.7, inherit.aes = FALSE) 
      
      PT_legend <-  scale_fill_manual(labels = c("1 lamp", "2 lamps", "3 lamps"), name = "phototherapy intensity", values = c("1" = "#A3BE8C", "2" = "#EBCB8B", "3" = "#BF616A"))
    } else {
      PT_ggplot <- NULL
      PT_legend <- NULL
    }
    
    fill_PT <- "#88C0D0"
    fill_ET <- "#5E81AC"
    alpha = 0.1
    
    ggplot(
      preterm_df %>% filter(bili_value > 0),
      aes(x = `postmenstrual age`, y = bili_value)
    ) +
      geom_point(size = 3,
                 color = "#5E81AC",
                 alpha = 1) + ylim(0, 25) + geom_line(color = "#5E81AC",
                                                      alpha = 1)+
      scale_x_continuous(
        limits = c(23, 36),
        minor_breaks = seq(
          from = 1,
          to = 36,
          by = 1 / 7
        ),
        breaks = 1:36
      ) + theme_bw() +
      theme(
        text = element_text(size = 20),
        legend.position = "bottom",
      ) +
      labs(caption = "shaded area = consider phototherapy (bottom) or exchange transfusion (top)",
           x = "gestational age (week)",
           y = "total serum bilirubin (mg/dL)") +
      PT_ggplot + PT_legend +
      geom_vline(xintercept = 35, linetype="dashed", 
                 color = "grey", size=0.8, ) +annotate(geom = "text", x=35, y=1, vjust = -0.2, label= "term values", angle = "90",  color = "gray20") +
      annotate(
        geom = "rect",
        xmin = -Inf,
        xmax = 28,
        ymin = 5,
        ymax = 6,
        color = "grey30",
        linetype = 3,
        fill = fill_PT, 
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = 28,
        xmax = 30,
        ymin = 6,
        ymax = 8,
        color = "grey30",
        linetype = 3,
        fill = fill_PT, 
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = 30,
        xmax = 32,
        ymin = 8,
        ymax = 10,
        color = "grey30",
        linetype = 3,
        fill = fill_PT, 
        alpha = 0.2
      )  +
      annotate(
        geom = "rect",
        xmin = 32,
        xmax = 34,
        ymin = 10,
        ymax = 12,
        color = "grey30",
        linetype = 3,
        fill = fill_PT, 
        alpha = 0.2
      )  +
      annotate(
        geom = "rect",
        xmin = 34,
        xmax = 35,
        ymin = 12,
        ymax = 14,
        color = "grey30",
        linetype = 3,
        fill = fill_PT, 
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = 35+(0/7),
        xmax = 35+(2/7),
        ymin = 12,
        ymax = 14,
        color = "grey30",
        linetype = 3,
        fill = fill_PT, 
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = 35+(2/7),
        xmax = 35+(3/7),
        ymin = 14.5,
        ymax = 17,
        color = "grey30",
        linetype = 3,
        fill = fill_PT, 
        alpha = 0.2
      ) +
      
      annotate(
        geom = "rect",
        xmin = 35+(3/7),
        xmax = 35+(4/7),
        ymin = 16,
        ymax = 18.5,
        color = "grey30",
        linetype = 3,
        fill = fill_PT, 
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = 35+(4/7),
        xmax = 35+(5/7),
        ymin = 16.2,
        ymax = 18.8,
        color = "grey30",
        linetype = 3,
        fill = fill_PT, 
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = 35+(5/7),
        xmax = 35+(6/7),
        ymin = 16.4,
        ymax = 19,
        color = "grey30",
        linetype = 3,
        fill = fill_PT, 
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = 35+(6/7),
        xmax = 35+(7/7),
        ymin = 16.5,
        ymax = 19,
        color = "grey30",
        linetype = 3,
        fill = fill_PT, 
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = -Inf,
        xmax = 28,
        ymin = 11,
        ymax = 14,
        color = "grey30",
        linetype = 3,
        fill = fill_ET,
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = 28,
        xmax = 30,
        ymin = 12,
        ymax = 14,
        color = "grey30",
        linetype = 3,
        fill = fill_ET,
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = 30,
        xmax = 32,
        ymin = 13,
        ymax = 16,
        color = "grey30",
        linetype = 3,
        fill = fill_ET,
        alpha = 0.2
      )  +
      annotate(
        geom = "rect",
        xmin = 32,
        xmax = 34,
        ymin = 15,
        ymax = 18,
        color = "grey30",
        linetype = 3,
        fill = fill_ET,
        alpha = 0.2
      )  +
      annotate(
        geom = "rect",
        xmin = 34,
        xmax = 35,
        ymin = 17,
        ymax = 19,
        color = "grey30",
        linetype = 3,
        fill = fill_ET,
        alpha = 0.2
      )+
      annotate(
        geom = "rect",
        xmin = 35+(0/7),
        xmax = 35+(2/7),
        ymin = 17,
        ymax = 19,
        color = "grey30",
        linetype = 3,
        fill = fill_ET,
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = 35+(2/7),
        xmax = 35+(3/7),
        ymin = 18.5,
        ymax = 21.5,
        color = "grey30",
        linetype = 3,
        fill = fill_ET,
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = 35+(3/7),
        xmax = 35+(4/7),
        ymin = 20,
        ymax = 23,
        color = "grey30",
        linetype = 3,
        fill = fill_ET,
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = 35+(4/7),
        xmax = 35+(5/7),
        ymin = 21,
        ymax = 24.5,
        color = "grey30",
        linetype = 3,
        fill = fill_ET,
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = 35+(5/7),
        xmax = 35+(6/7),
        ymin = 21.2,
        ymax = 24.8,
        color = "grey30",
        linetype = 3,
        fill = fill_ET,
        alpha = 0.2
      ) +
      annotate(
        geom = "rect",
        xmin = 35+(6/7),
        xmax = 35+(7/7),
        ymin = 21.5,
        ymax = 25,
        color = "grey30",
        linetype = 3,
        fill = fill_ET,
        alpha = 0.2
      )
  }
})
  }

# Run the application 
shinyApp(ui = ui, server = server)

