#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggpubr)
library(jpeg)
library(tidyverse)
library(ggimage)
library(shinyTime)
library(shinythemes)
library(ggrepel)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Indicatie voor fototherapie > of < 35 weken"),
  sidebarLayout(sidebarPanel(
    tabsetPanel(
      type = "tab",
      id = "main",
      tabPanel(
        "Patiëntengegevens",
        selectInput("prematuur", "Prematuur < 35 weken:",
                    c("nee" = "nee",
                      "ja" = "ja")),
        textInput("naam", "Naam", value = "naam"),
        dateInput(
          inputId = 'geboortedag',
          label = 'Geboortedag (yyyy-mm-dd)',
          value = Sys.Date()
        ),
        textInput("geboorteuur", "Geboorteuur", value = "00:00"),
      ),
      tabPanel(
        "Aterm - Tijdspunt 1",
        conditionalPanel(
          condition = "input.prematuur == 'nee'",
          dateInput(
            inputId = 'afnamedag1',
            label = 'Afnamedag (yyyy-mm-dd)',
            value = Sys.Date()
          ),
          textInput("afnameuur1", "Afnameuur", value = "10:00"),
          numericInput(
            "bili1",
            "Bilirubine in mg/dL ",
            0,
            min = 0,
            max = 100
          ),
        )
      ),
      tabPanel(
        "Aterm - Tijdspunt 2",
        conditionalPanel(
          condition = "input.prematuur == 'nee'",
          dateInput(
            inputId = 'afnamedag2',
            label = 'Afnamedag: ',
            value = Sys.Date()
          ),
          textInput("afnameuur2", "Afnameuur", value = "10:00"),
          numericInput(
            "bili2",
            "Bilirubine in mg/dL ",
            0,
            min = 0,
            max = 100
          ),
        )
      ),
      tabPanel(
        "Aterm - Tijdspunt 3",
        conditionalPanel(
          condition = "input.prematuur == 'nee'",
          dateInput(
            inputId = 'afnamedag3',
            label = 'Afnamedag: ',
            value = Sys.Date()
          ),
          #timeInput("afnameuur3", "Afnameuur", seconds = FALSE, value =  Sys.time()),
          textInput("afnameuur3", "Afnameuur", value = "10:00"),
          numericInput(
            "bili3",
            "Bilirubine in mg/dL ",
            0,
            min = 0,
            max = 100
          ),
        )
      ),
      tabPanel(
        "Preterm - Tijdspunt 1",
        conditionalPanel(
          condition = "input.prematuur == 'ja'",
          textInput(
            "PML1",
            "PML (voorbeeldformaat = \"23+1/7\", niet 24w1d, 24+1, oid)",
            value = "23+1/7"
          ),
          numericInput(
            "biliprem1",
            "Bilirubine in mg/dL ",
            0,
            min = 0,
            max = 100
          ),
        )
      ),
      tabPanel(
        "Preterm - Tijdspunt 2",
        conditionalPanel(
          condition = "input.prematuur == 'ja'",
          textInput("PML2", "PML (formaat = \"23+1/7\")", value = "23+1/7"),
          numericInput(
            "biliprem2",
            "Bilirubine in mg/dL ",
            0,
            min = 0,
            max = 100
          ),
        )
      ),
      tabPanel(
        "Preterm - Tijdspunt 3",
        conditionalPanel(
          condition = "input.prematuur == 'ja'",
          textInput("PML3", "PML (formaat = \"23+1/7\")", value = "23+1/7"),
          numericInput(
            "biliprem3",
            "Bilirubine in mg/dL ",
            0,
            min = 0,
            max = 100
          ),
        )
      ),
      tabPanel(
        "Geavanceerd",
        selectInput(
          "advanced",
          "Geavanceerde instellingen:",
          c("nee" = "nee",
            "ja" = "ja")
        ),
        conditionalPanel(
          condition = "input.advanced == 'ja'",
          textInput("geboorte_GET", "Geboorteuur in CSV", value = NA),
          textInput("afname_GET", "Afnameuur in CSV", value = NA),
          textInput("PML_GET", "Postmenstruele leeftijd in CSV", value = NA),
          textInput("bili_GET", "Bilirubine in mg/dL in CSV ", value = NA),
        )
      ),
    ),
  ),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Plot",
        plotOutput("bilicurve", height = "550px", width = "700px"),
        hr(),
        h4("Ingegeven waarden"),
        tableOutput("time_output1"),
        hr(),
        p(
          "This tool has not been extensively tested, so verify with the original curves before initiating therapy (included above). For questions or suggestions, e-mail ruben.vanpaemel@ugent.be. Source: Maisels MJ, Bhutani VK, Bogen D, Newman TB, Stark AR, Watchko JF. Hyperbilirubinemia in the newborn infant > or =35 weeks' gestation: an update with clarifications. Pediatrics. 2009;124(4):1193-1198. doi:10.1542/peds.2009-0329. The graph was digitised with WebPlotDigitizer: https://automeris.io/WebPlotDigitizer/."
        ),
        hr()
      ),
      tabPanel(
        "Original plot",
        img(
          src = 'bilicurve.jpeg',
          width = "100%",
          height = "100%"
        )
      ),
      tabPanel(
        "Documentation",
        p(
          "Values can be entered through the interface or through a GET request. Examples are"
        ),
        a(
          "http://rubenvp.shinyapps.io/bilicurve/?advanced=ja&naam=testbaby&geboorte_GET=2023-11-22%2010:00:00&afname_GET=2023-11-23%2010:00:00,2023-11-24%2010:00:00&bili_GET=10,9"
        ),
        br(),
        a(
          "http://rubenvp.shinyapps.io/bilicurve/?advanced=ja&naam=testbaby&prematuur=ja&PML_GET=23%2B1/7,24%2B1/7&bili_GET=10,9"
        )
      )
    )
  ))
)

server <- function(input, output, session) {
  
  # format e.g. 23+1/7 to 23.14
  calc <- function(x)
    eval(parse(text = x))
  
  # parse GET request
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['naam']])) {
      updateTextInput(session, "naam", value = query[['naam']])
    }
    if (!is.null(query[['advanced']])) {
      updateTextInput(session, "advanced", value = query[['advanced']])
    }
    if (!is.null(query[['prematuur']])) {
      updateTextInput(session, "prematuur", value = query[['prematuur']])
    }
    if (!is.null(query[['geboorte_GET']])) {
      updateTextInput(session, "geboorte_GET", value = query[['geboorte_GET']])
    }
    if (!is.null(query[['afname_GET']])) {
      updateTextInput(session, "afname_GET", value = query[['afname_GET']])
    }
    if (!is.null(query[['afnamedag1']])) {
      updateTextInput(session, "afnamedag1", value = query[['afnamedag1']])
    }
    if (!is.null(query[['afnameuur1']])) {
      updateTextInput(session, "afnameuur1", value = query[['afnameuur1']])
    }
    if (!is.null(query[['afnamedag2']])) {
      updateTextInput(session, "afnamedag2", value = query[['afnamedag2']])
    }
    if (!is.null(query[['afnameuur2']])) {
      updateTextInput(session, "afnameuur2", value = query[['afnameuur2']])
    }
    if (!is.null(query[['afnamedag3']])) {
      updateTextInput(session, "afnamedag3", value = query[['afnamedag3']])
    }
    if (!is.null(query[['afnameuur3']])) {
      updateTextInput(session, "afnameuur3", value = query[['afnameuur3']])
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
    if (!is.null(query[['PML_GET']])) {
      updateTextInput(session, "PML_GET", value = query[['PML_GET']])
    }
  })
  
  # format text to yyy-mm-dd hh:mm:ss
  vals <- reactiveValues()
  
  observe({
    testdatetime <- paste(input$geboortedag, input$geboorteuur)
    testdatetime <-
      as.POSIXct(testdatetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$initial_date <- testdatetime
  })
  
  observe({
    testdatetime2 <- paste(input$afnamedag1, input$afnameuur1)
    testdatetime2 <-
      as.POSIXct(testdatetime2, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date1 <- testdatetime2
  })
  
  observe({
    testdatetime3 <- paste(input$afnamedag2, input$afnameuur2)
    testdatetime3 <-
      as.POSIXct(testdatetime3, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date2 <- testdatetime3
  })
  
  observe({
    testdatetime4 <- paste(input$afnamedag3, input$afnameuur3)
    testdatetime4 <-
      as.POSIXct(testdatetime4, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date3 <- testdatetime4
    test4 <- inherits(testdatetime4, "POSIXct")
    print(test4)
  })
  
  # placeholder code to expand the manual input to 7 points
  observe({
   testdatetime5 <- paste(input$afnamedag4, input$afnameuur4)
   testdatetime5 <-
     as.POSIXct(testdatetime5, format = "%Y-%m-%d %H:%M", tz = "UTC")
   vals$to_date4 <- testdatetime5
  })
  
  
  observe({
    testdatetime6 <- paste(input$afnamedag5, input$afnameuur5)
    testdatetime6 <-
      as.POSIXct(testdatetime6, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date5 <- testdatetime6
  })

  observe({
    testdatetime7 <- paste(input$afnamedag6, input$afnameuur6)
    testdatetime7 <-
      as.POSIXct(testdatetime7, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date6 <- testdatetime7
  })

  observe({
    testdatetime8 <- paste(input$afnamedag7, input$afnameuur7)
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
  
  output$time_output1 <- renderTable({
    
    # difference between dob and date of sample in hours
    value <- as.character(vals$initial_date)
    value1 <-
      as.character(difftime(vals$to_date1, vals$initial_date, units = "hours"))
    value2 <-
      as.character(difftime(vals$to_date2, vals$initial_date, units = "hours"))
    value3 <-
      as.character(difftime(vals$to_date3, vals$initial_date, units = "hours"))
    value4 <-
      as.character(difftime(vals$to_date4, vals$initial_date, units = "hours"))
    value5 <-
      as.character(difftime(vals$to_date5, vals$initial_date, units = "hours"))
    value6 <-
      as.character(difftime(vals$to_date6, vals$initial_date, units = "hours"))
    value7 <-
      as.character(difftime(vals$to_date7, vals$initial_date, units = "hours"))

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
    
    # advanced is to parse all values through GET requests instead of the UI. Splits at , and changes all strings to dates or numbers
    # the code below can benefit from reworking to a reactive dataframe and reuse that dataframe during plot generation but for now it works
    if (input$advanced == "ja") {
      #testvalues
      #geboorte_GET <- c("2023-11-22T10:00:00")
      #afname_GET <- c("2023-11-23T10:00:00;2023-11-24T10:00:00")
      #bili_GET <- c("10,9")
      #PML_GET <- c("23+1/7,24+1/7")
      
      geboorte_GET <- as.character(input$geboorte_GET)
      afname_GET <- as.character(input$afname_GET)
      bili_GET <- as.character(input$bili_GET)
      PML_GET <- as.character(input$PML_GET)
      annotation <- "sample"
      
      if (input$prematuur == "nee") {
        geboorte_GET_POSIX <-
          as.POSIXct(unlist(strsplit(geboorte_GET, split = ",")), format = "%Y-%m-%d %H:%M", tz = "UTC")
        afname_GET_POSIX <-
          as.POSIXct(unlist(strsplit(afname_GET, split = ",")), format = "%Y-%m-%d %H:%M", tz = "UTC")
        bili_GET_split <-
          as.numeric(unlist(strsplit(bili_GET, split = ",")))
        df_GET <-
          tibble(
            geboorte = geboorte_GET_POSIX,
            afname = afname_GET_POSIX,
            bili = bili_GET_split,
            annotation = annotation
          )
        df_GET$diff_hours <-
          as.character(difftime(df_GET$afname, df_GET$geboorte, units = "hours"))
        df2 <-
          df_GET %>% mutate(
            afnamemoment =  as.character(
              as.POSIXct(afname_GET_POSIX, origin = "1970-01-01", tz = "UTC")
            ) ,
            `tijd in uren` = as.double(diff_hours),
            biliwaarde = as.double(bili),
            annotation = "sample"
          ) %>% select(afnamemoment, `tijd in uren`, biliwaarde, annotation)
        df2
      } else {
        df2 <- tibble(time_HR = NA,
                      value = NA,
                      annotation = "sample")
        bili_GET_split <-
          as.numeric(unlist(strsplit(bili_GET, split = ",")))
        PML_GET <-
          as.character(unlist(strsplit(PML_GET, split = ",")))
        preterm_df <- tibble(PML_GET = PML_GET,
                             biliprem = bili_GET_split)
        preterm_df <- preterm_df %>% rowwise() %>%
          mutate(PML = calc(PML_GET))
        preterm_df <-
          preterm_df %>% mutate(`postmenstruele leeftijd` = PML_GET,
                                biliwaarde = biliprem) %>% select(`postmenstruele leeftijd`, biliwaarde)
      }
    } else {
      if (input$prematuur == "nee") {
        df2 <-
          tibble(
            afnamemoment = c(
              vals$to_date1,
              vals$to_date2,
              vals$to_date3,
              vals$to_date4,
              vals$to_date5,
              vals$to_date6,
              vals$to_date7
            ),
            `tijd in uren` = c(value1, value2, value3, value4, value5, value6, value7),
            biliwaarde = c(bili1, bili2, bili3, bili4, bili5, bili6, bili7),
            annotation = "sample"
          )
        df2 <-
          df2 %>% mutate(afnamemoment = as.character(
            as.POSIXct(afnamemoment, origin = "1970-01-01", tz = "UTC")
          ))
        df2 %>% select(afnamemoment, `tijd in uren`, biliwaarde) %>% filter(biliwaarde >
                                                                              0) %>% arrange(`tijd in uren`)
      } else {
        preterm_df <-
          tibble(
            PML = c(calc(input$PML1), calc(input$PML2), calc(input$PML3)),
            PML_raw = c((input$PML1), (input$PML2), (input$PML3)),
            biliprem = c(input$biliprem1, input$biliprem2, input$biliprem3)
          )
        
        preterm_df <-
          preterm_df %>% mutate(`postmenstruele leeftijd` = PML,
                                biliwaarde = biliprem)  %>% select(`postmenstruele leeftijd`, biliwaarde)
      }
    }
    
  })
  
  
  
  output$bilicurve <- renderPlot({
    # This is more or less the same code as above to render the table, this can be made more efficient with a reactive dataframe
    preterm_df <-
      tibble(
        PML = c(calc(input$PML1), calc(input$PML2), calc(input$PML3)),
        biliprem = c(input$biliprem1, input$biliprem2, input$biliprem3)
      )
    
    name <- as.character(input$naam)
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
    
    if (input$advanced == "ja") {
      #geboorte_GET <- c("2023-11-22T10:00:00")
      geboorte_GET <- as.character(input$geboorte_GET)
      #afname_GET <- c("2023-11-23T10:00:00;2023-11-24T10:00:00")
      afname_GET <- as.character(input$afname_GET)
      #bili_GET <- c("10,9")
      bili_GET <- as.character(input$bili_GET)
      #PML_GET <- c("23+1/7,24+1/7")
      PML_GET <- as.character(input$PML_GET)
      annotation <- "sample"
      
      if (input$prematuur == "nee") {
        geboorte_GET_POSIX <-
          as.POSIXct(unlist(strsplit(geboorte_GET, split = ",")), format = "%Y-%m-%d %H:%M", tz = "UTC")
        afname_GET_POSIX <-
          as.POSIXct(unlist(strsplit(afname_GET, split = ",")), format = "%Y-%m-%d %H:%M", tz = "UTC")
        bili_GET_split <-
          as.numeric(unlist(strsplit(bili_GET, split = ",")))
        df_GET <-
          tibble(
            geboorte = geboorte_GET_POSIX,
            afname = afname_GET_POSIX,
            bili = bili_GET_split,
            annotation = annotation
          )
        df_GET$diff_hours <-
          as.character(difftime(df_GET$afname, df_GET$geboorte, units = "days"))
        df2 <-
          df_GET %>% mutate(time_HR = as.double(diff_hours),
                            value = as.double(bili)) %>% select(time_HR, value, annotation)
      } else {
        df2 <- tibble(time_HR = NA,
                      value = NA,
                      annotation = "sample")
        bili_GET_split <-
          as.numeric(unlist(strsplit(bili_GET, split = ",")))
        PML_GET <-
          as.character(unlist(strsplit(PML_GET, split = ",")))
        preterm_df <- tibble(PML_GET = PML_GET,
                             biliprem = bili_GET_split)
        preterm_df <- preterm_df %>% rowwise() %>%
          mutate(PML = calc(PML_GET))
      }
    } else {
      df2 <-
        tibble(
          time_HR = as.double(c(
            value1, value2, value3, value4, value5, value6, value7
          )),
          value = as.double(c(
            bili1, bili2, bili3, bili4, bili5, bili6, bili7
          )),
          annotation = "sample"
        )
      df2
    }
    
    # read the dataframe that wil be used for plotting LR, MR, HR
    df <-
      read_csv(
        "bilidf.csv",
        col_names = c(
          "time_HR",
          "infants at higher risk (35-37 6/7 wk + risk factors)",
          "time_MR",
          "infants at medium risk (>=38 wk + risk factors or 35-37 6/7 wk and well)",
          "time_LR",
          "infants at lower risk (>=38 wk and well)"
        ),
        skip = 2
      )
    df <-
      df %>% tidyr::gather(key = "annotation",
                           value = "value",
                           -c(time_HR, time_MR, time_LR))
    df <- df %>% select(-c(time_LR, time_MR))
    df <- bind_rows(df, df2)
    
    # extract the most recent entered value to annotate the corresponding thresholds for LR, MR and HR on the plot
    x_seq = df2 %>% filter(value > 0) %>% pull(time_HR)
    
    intersections <- df %>% filter(annotation != "sample") %>%
      group_by(annotation) %>%
      dplyr::reframe(interpolated = approx(x = time_HR, y = value, xout = x_seq)$y) %>%
      mutate(x_seq = rep(x_seq, 3)) %>%
      arrange(annotation, x_seq) %>%
      group_by(annotation) %>%
      summarise(across(everything(), last)) %>% mutate(interpolated = round(interpolated, 1))
    
    last_intersect <- intersections %>% pull(x_seq) %>% unique()
    
    if (input$prematuur == "nee") {
      g <-
        ggplot(df, aes(y = value, x = time_HR, col = annotation)) +  geom_vline(
          xintercept = last_intersect,
          color = "black",
          linetype = "dashed",
          alpha = 0.3
        ) + geom_text_repel(
          data = intersections,
          size = 5,
          aes(x_seq, interpolated, label = round(interpolated, 2)),
          min.segment.length = 0,
          seed = 42,
          box.padding = 0.5,
          max.overlaps =  Inf
        )  +
        geom_line(data = df %>% filter(annotation != "sample"))  + theme_bw() + xlim(0, 7) + ylim(0, 25) + xlab("age in days") + ylab("TSB, mg/dL") +
        geom_point(
          data = df %>% filter(annotation == "sample", value > 0),
          aes(y = value, x = time_HR, col = annotation),
          size = 3
        ) +
        labs(subtitle = paste0(name, " °", value)) + theme(
          text = element_text(size = 20),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = element_blank()
        ) + scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7)) +
        geom_point(data = intersections %>% filter(x_seq > 0) , aes(x = x_seq, y = interpolated))
      g + guides(color = guide_legend(nrow = 4))
    } else{
      ggplot(preterm_df %>% filter(biliprem > 0),
             aes(x = PML, y = biliprem)) +
        geom_point(size = 3,
                   color = "darkgreen",
                   alpha = 0.5) + ylim(0, 25) +
        scale_x_continuous(
          limits = c(23, 35),
          minor_breaks = seq(
            from = 1,
            to = 35,
            by = 1 / 7
          ),
          breaks = 1:35
        ) + theme_bw() +
        theme(
          text = element_text(size = 20),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = element_blank()
        ) +
        labs(caption = "orange = initiate phototherapy, red = exchange transfusion",
             x = "gestational age (week)",
             y = "TSB (mg/dL)") +
        annotate(
          geom = "rect",
          xmin = -Inf,
          xmax = 28,
          ymin = 5,
          ymax = 6,
          fill = "orange",
          alpha = 0.5
        ) +
        annotate(
          geom = "rect",
          xmin = 28,
          xmax = 30,
          ymin = 6,
          ymax = 8,
          fill = "orange",
          alpha = 0.5
        ) +
        annotate(
          geom = "rect",
          xmin = 30,
          xmax = 32,
          ymin = 8,
          ymax = 10,
          fill = "orange",
          alpha = 0.5
        )  +
        annotate(
          geom = "rect",
          xmin = 32,
          xmax = 34,
          ymin = 10,
          ymax = 12,
          fill = "orange",
          alpha = 0.5
        )  +
        annotate(
          geom = "rect",
          xmin = 34,
          xmax = 35,
          ymin = 12,
          ymax = 14,
          fill = "orange",
          alpha = 0.5
        ) +
        annotate(
          geom = "rect",
          xmin = -Inf,
          xmax = 28,
          ymin = 11,
          ymax = 14,
          fill = "red",
          alpha = 0.5
        ) +
        annotate(
          geom = "rect",
          xmin = 28,
          xmax = 30,
          ymin = 12,
          ymax = 14,
          fill = "red",
          alpha = 0.5
        ) +
        annotate(
          geom = "rect",
          xmin = 30,
          xmax = 32,
          ymin = 13,
          ymax = 16,
          fill = "red",
          alpha = 0.5
        )  +
        annotate(
          geom = "rect",
          xmin = 32,
          xmax = 34,
          ymin = 15,
          ymax = 18,
          fill = "red",
          alpha = 0.5
        )  +
        annotate(
          geom = "rect",
          xmin = 34,
          xmax = 35,
          ymin = 17,
          ymax = 19,
          fill = "red",
          alpha = 0.5
        )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
