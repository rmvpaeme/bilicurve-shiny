#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)
library(shinyTime)
library(shinythemes)
library(ggrepel)
library(DT)
library(Cairo)
library(shinyscreenshot)
library(shiny.i18n)
library(bslib)

# Vertaling NL (standaard) / EN. usei18n() wisselt statische UI-tekst client-side
# zodat de app-status behouden blijft bij het wisselen van taal. Server-zijde
# tekst (ggplot-labels, DT) gebruikt dezelfde translator via tr(): in een sessie
# geeft i18n$t() platte tekst terug (geen HTML-markup).
i18n <- Translator$new(translation_json_path = "translations.json")
i18n$set_translation_language("nl")

# Oorspronkelijk thema (zoals op GitHub): de Cosmo-bootswatch. Via bslib (BS5)
# zodat de card/accordion/tooltip-componenten blijven werken.
app_theme <- bs_theme(version = 5, bootswatch = "cosmo")

# timeInput levert een POSIXct; haal er veilig "HH:MM" uit. Geeft "" terug voor
# NULL of niet-tijdwaarden (bv. tijdens initialisatie) zodat de datum/uur-parsing
# nooit faalt.
hm <- function(x) {
  if (is.null(x) || !inherits(x, c("POSIXct", "POSIXlt"))) return("")
  format(x, "%H:%M")
}

# Ademruimte rond elke ggplot-figuur, voor een consistente look.
plot_frame <- theme(
  plot.margin = margin(18, 18, 18, 18)
)


# Define UI
ui <- fluidPage(
  theme = app_theme,
  usei18n(i18n),
  tags$head(
    tags$title("bilicurve"),
    # Minimale styling: enkel de positionering van de taalknop en wat fijnregeling.
    tags$style(HTML(
      ".lang-switch{position:fixed;top:14px;right:18px;z-index:1050;}
       .app-subtitle{color:#777;font-size:0.9rem;margin-top:2px;}
       .disclaimer{font-size:12px;color:#666;}"
    ))
  ),

  # Compacte taalknop, vast in de rechterbovenhoek. Het label toont de taal
  # waarnaar je overschakelt (NL -> "EN", EN -> "NL").
  div(class = "lang-switch", actionButton("toggle_lang", "EN")),

  div(
    class = "app-header",
    h1(i18n$t("Indicatie voor fototherapie")),
    p(class = "app-subtitle",
      i18n$t("Afkapwaarden voor fototherapie bij neonatale hyperbilirubinemie"))
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      tabsetPanel(
        type = "tab",
        id = "main",
        tabPanel(
          i18n$t("Patiëntengegevens"),
          value = "Patiëntengegevens",
          conditionalPanel(
            condition = "(input.advanced == 'nee')",
            selectInput("prematuur", "Prematuur < 35 weken:",
                        c("nee" = "nee",
                          "ja" = "ja")),
          ),
          selectInput("bili_risk", "Risicofactoren aanwezig:",
                      choices = c("maak een keuze" = "maak een keuze", "nee" = "nee",
                                  "ja" = "ja")),
          tooltip(
            span(
              icon("circle-info"),
              i18n$t("Welke risicofactoren voor neurotoxiciteit?"),
              style = "font-size: 0.85rem; color: var(--nord-blue); cursor: help;"
            ),
            i18n$t("Risicofactoren voor neurotoxiciteit: albumine < 3,0 g/dL; iso-immune hemolytische ziekte; G6PD-deficiëntie of andere hemolytische aandoeningen; sepsis; belangrijke klinische instabiliteit in de voorbije 24 uur."),
            placement = "right"
          ),
          conditionalPanel(condition = "input.geboortedag == '2022-00-00'",
                           textInput("naam", i18n$t("Naam"), value = "naam")),
          conditionalPanel(
            condition = "(input.advanced == 'ja')",
            p(
              i18n$t("Geavanceerde instellingen geselecteerd, manuele input niet mogelijk. Klik"),
              a(i18n$t("hier"), href = "http://rubenvp.shinyapps.io/bilicurve"),
              i18n$t("om naar de applicatie met manuele invoer te worden gebracht.")
            )
          ),
          conditionalPanel(
            condition = "(input.prematuur == 'nee' && input.advanced == 'nee')",
            card(
              card_header(i18n$t("Geboortegegevens")),
              dateInput(
                inputId = 'geboortedag',
                label = i18n$t("Geboortedag (yyyy-mm-dd)"),
                value = Sys.Date()
              ),
              timeInput("geboorteuur", i18n$t("Geboorteuur"),
                        value = strptime("00:01", "%H:%M"), seconds = FALSE),
              textInput("PML_geboorte", i18n$t("PML bij geboorte (formaat = \"36+1/7\"). Geeft error bij waarden < 35+0/7."), value = NA),
              fileInput(
                'file_aterm',
                i18n$t("Upload eerder opgeslagen tabel voor aterm"),
                accept = c(".xlsx")
              )
            ),
          ),
          conditionalPanel(
            condition = "(input.prematuur == 'ja' && input.advanced == 'nee')",
            fileInput(
              'file_preterm',
              i18n$t("Upload eerder opgeslagen tabel voor preterm"),
              accept = c(".xlsx")
            )
          )
        ),
        tabPanel(
          i18n$t("Aterm - biliwaarden"),
          value = "Aterm - biliwaarden",
          conditionalPanel(
            condition = "(input.prematuur == 'nee' && input.advanced == 'nee')",
            helpText(i18n$t("Vul één of meer afnamemomenten in. Lege of nulwaarden worden genegeerd.")),
            card(
              card_header(i18n$t("Afnamemoment 1")),
              fluidRow(
                column(7,
                       dateInput(
                         inputId = 'afnamedag1',
                         label = i18n$t("Datum"),
                         value = Sys.Date()
                       )),
                column(5,
                       timeInput("afnameuur1", i18n$t("Uur"),
                                 value = strptime("10:00", "%H:%M"), seconds = FALSE)),
                column(7,
                       numericInput(
                         "bili1",
                         i18n$t("Bilirubine (mg/dL)"),
                         0,
                         min = 0,
                         max = 100
                       )))
            ),
            card(
              card_header(i18n$t("Afnamemoment 2")),
              fluidRow(
                column(7,
                       dateInput(
                         inputId = 'afnamedag2',
                         label = i18n$t("Datum"),
                         value = Sys.Date()
                       )),
                column(5,
                       timeInput("afnameuur2", i18n$t("Uur"),
                                 value = strptime("10:00", "%H:%M"), seconds = FALSE)),
                column(7,
                       numericInput(
                         "bili2",
                         i18n$t("Bilirubine (mg/dL)"),
                         0,
                         min = 0,
                         max = 100
                       )))
            ),
            card(
              card_header(i18n$t("Afnamemoment 3")),
              fluidRow(
                column(7,
                       dateInput(
                         inputId = 'afnamedag3',
                         label = i18n$t("Datum"),
                         value = Sys.Date()
                       )),
                column(5,
                       timeInput("afnameuur3", i18n$t("Uur"),
                                 value = strptime("10:00", "%H:%M"), seconds = FALSE)),
                column(7,
                       numericInput(
                         "bili3",
                         i18n$t("Bilirubine (mg/dL)"),
                         0,
                         min = 0,
                         max = 100
                       )))
            ),
          ),
        ),
        tabPanel(
          i18n$t("Preterm - biliwaarden"),
          value = "Preterm - biliwaarden",
          conditionalPanel(
            condition = "(input.prematuur == 'ja' && input.advanced == 'nee')",
            h4(i18n$t("Afnamemoment 1")),
            textInput("PML1", i18n$t("PML (formaat = \"23+1/7\")"), value = "23+1/7"),
            numericInput(
              "biliprem1",
              i18n$t("Bilirubine in mg/dL "),
              0,
              min = 0,
              max = 100
            ),
            hr(),
            h4(i18n$t("Afnamemoment 2")),
            textInput("PML2", i18n$t("PML"), value = "23+1/7"),
            numericInput(
              "biliprem2",
              i18n$t("Bilirubine in mg/dL "),
              0,
              min = 0,
              max = 100
            ),
            hr(),
            h4(i18n$t("Afnamemoment 3")),
            textInput("PML3", i18n$t("PML"), value = "23+1/7"),
            numericInput(
              "biliprem3",
              i18n$t("Bilirubine in mg/dL "),
              0,
              min = 0,
              max = 100
            ),
          ),
        ),
        tabPanel(
          i18n$t("Geavanceerd"),
          value = "Geavanceerd",
          selectInput(
            "advanced",
            "Geavanceerde instellingen:",
            c("nee" = "nee",
              "ja" = "ja")
          ),
          conditionalPanel(
            condition = "input.advanced == 'ja'",
            h4(i18n$t("Error")),
            p(i18n$t("Geavanceerde instellingen geselecteerd, manuele input niet mogelijk. Klik"),
              a(i18n$t("hier"), href = "http://rubenvp.shinyapps.io/bilicurve"),
              i18n$t("om naar de applicatie met manuele invoer te worden gebracht. De waarden hieronder zijn automatisch gegenereerd en zijn niet aan te passen. ")
            ),
            textInput("geboorte_GET", i18n$t("Geboortedag en uur in CSV (enkel voor curve > 35 weken)"), value = NA),
            textInput("afname_GET", i18n$t("Afname dag en uur in CSV (enkel voor curve > 35 weken)"), value = NA),
            textInput("PML_geboorte_GET", i18n$t("Postmenstruele leeftijd bij geboorte (enkel voor curve > 35 weken)"), value = NA),
            textInput("PML_GET", i18n$t("Postmenstruele leeftijd bij afname (enkel nodig voor curve < 35 weken) in CSV"), value = NA),
            textInput("bili_GET", i18n$t("Bilirubine in mg/dL in CSV (beide curven)"), value = NA),
            textInput("PT_start_GET", i18n$t("Fototherapie start datum+uur in CSV "), value = NA),
            textInput("PT_stop_GET", i18n$t("Fototherapie stop datum+uur in CSV "), value = NA),
            textInput("PT_aantalLampen_GET", i18n$t("Aantal lampen bij fototherapie in CSV "), value = NA)
          )
        ),
      ),
    ),
    
    mainPanel(tabsetPanel(
      type = "tabs",
      id = "output",
      selected = "Maak een keuze",
      tabPanel(
        i18n$t("Bilicurve"),
        value = "Bilicurve",
        conditionalPanel(
          condition = "input.bili_risk != 'maak een keuze'",
          plotOutput("bilicurve", height = "750px", width = "100%"),
          # do.call forceert de evaluatie van i18n$t() voordat screenshotButton
          # zijn ... lui (in de shinyscreenshot-namespace) evalueert.
          do.call(screenshotButton, list(label = i18n$t("Figuur opslaan"), id = "bilicurve")),
          hr(),
          DT::dataTableOutput("time_output1"),
          hr(),
          accordion(
            open = FALSE,
            accordion_panel(
              i18n$t("Disclaimer"),
              value = "disclaimer",
              p(
                class = "disclaimer",
                i18n$t("Auteurs: Ruben Van Paemel, Kris De Coen, Sophie Vanhaesebrouck (NICU Ghent University Hospital). Deze tool is niet uitgebreid getest; verifieer steeds met de oorspronkelijke curves vooraleer therapie te starten (zie hierboven). Voor vragen, suggesties of bugs, mail naar ruben.vanpaemel@ugent.be. Voor baby's die rond 35 weken geboren zijn, werden de afkapwaarden van de atermecurve toegevoegd, waarbij de bovengrens van het vak = baby's > 35 weken zonder risicofactor en de ondergrens = baby's > 35 weken met risicofactoren (elk vak stelt 1 dag na 35 weken voor, eindigend op 35+6/7). Bron: Kemper AR, Newman TB, Slaughter JL, et al. Clinical Practice Guideline Revision: Management of Hyperbilirubinemia in the Newborn Infant 35 or More Weeks of Gestation. Pediatrics. 2022;150(3):e2022058859. doi:10.1542/peds.2022-058859 and Maisels MJ, Watchko JF, Bhutani VK, Stevenson DK. An approach to the management of hyperbilirubinemia in the preterm infant less than 35 weeks of gestation. Journal of Perinatology 2012;32:660-4. De Luca D, Romagnoli C, Tiberi E, Zuppa AA, Zecca E. Skin bilirubin nomogram for the first 96 h of life in a European normal healthy newborn population, obtained with multiwavelength transcutaneous bilirubinometry. Acta Paediatr. 2008 Feb;97(2):146-50. doi: 10.1111/j.1651-2227.2007.00622.x. PMID: 18254903. De code en documentatie zijn beschikbaar op https://github.com/rmvpaeme/bilicurve-shiny .")
              )
            )
          )
        )),
      tabPanel(
        i18n$t("Maak een keuze"),
        value = "Maak een keuze",
        p(i18n$t("Vul de velden in de linkerkolom in.")),
        tags$ol(
          tags$li(i18n$t("Prematuur: de curve verschilt voor PML > of < 35 weken")),
          tags$li(i18n$t("Vul geboortedag, geboorteuur en PML bij geboorte in")),
          tags$li(i18n$t("Beslis of de patiënt een hoog risico heeft op bilirubinetoxiciteit"))
        ),
        p(i18n$t("Risicofactoren voor hyperbilirubinemie-neurotoxiciteit zijn:")),
        tags$ul(
          tags$li(i18n$t("albumine < 3,0 g/dL")),
          tags$li(i18n$t("iso-immune hemolytische ziekte")),
          tags$li(i18n$t("glucose-6-fosfaatdehydrogenase (G6PD)-deficiëntie of andere hemolytische aandoeningen")),
          tags$li(i18n$t("sepsis")),
          tags$li(i18n$t("belangrijke klinische instabiliteit in de voorbije 24 uur"))
        )),
      tabPanel(
        i18n$t("Oorspronkelijke curves"),
        value = "Oorspronkelijke curves",
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
        i18n$t("Gebruik"),
        value = "Gebruik",
        p(i18n$t("Documentatie over het geavanceerde gebruik vind je op"), a("https://github.com/rmvpaeme/bilicurve-shiny/", href = "https://github.com/rmvpaeme/bilicurve-shiny/"))
      )
    ))
  )
)


server <- function(input, output, session) {options(shiny.usecairo=TRUE)

  # Huidige taal (NL standaard), gewisseld via de knop rechtsboven.
  lang <- reactiveVal("nl")
  observeEvent(input$toggle_lang, {
    lang(if (lang() == "nl") "en" else "nl")
  })

  # Reactieve translator voor server-zijde tekst (plots, tabelknop). In een
  # sessie geeft i18n$t() platte tekst terug (geen HTML-markup).
  tr <- reactive({
    i18n$set_translation_language(lang())
    i18n
  })

  # Houd statische UI-tekst, het knoplabel en de keuzelijsten met logica-waarden
  # (nee/ja/maak een keuze) synchroon met de gekozen taal. <option>-tekst wordt
  # niet client-side gewisseld, daarom updaten we die keuzelijsten hier expliciet.
  observeEvent(lang(), {
    update_lang(lang())
    updateActionButton(session, "toggle_lang",
                       label = if (lang() == "nl") "EN" else "NL")
    updateSelectInput(
      session, "prematuur",
      label = tr()$t("Prematuur < 35 weken:"),
      choices = setNames(c("nee", "ja"), c(tr()$t("nee"), tr()$t("ja"))),
      selected = input$prematuur
    )
    updateSelectInput(
      session, "bili_risk",
      label = tr()$t("Risicofactoren aanwezig:"),
      choices = setNames(
        c("maak een keuze", "nee", "ja"),
        c(tr()$t("maak een keuze"), tr()$t("nee"), tr()$t("ja"))
      ),
      selected = input$bili_risk
    )
    updateSelectInput(
      session, "advanced",
      label = tr()$t("Geavanceerde instellingen:"),
      choices = setNames(c("nee", "ja"), c(tr()$t("nee"), tr()$t("ja"))),
      selected = input$advanced
    )
  })

  observeEvent(input$prematuur, {
    if (input$prematuur == "ja") {
      showTab(inputId = "main", target = "Preterm - biliwaarden")
      hideTab(inputId = "main", target = "Aterm - biliwaarden")
    } else if (input$prematuur == "nee") {
      hideTab(inputId = "main", target = "Preterm - biliwaarden")
      showTab(inputId = "main", target = "Aterm - biliwaarden")
    }
  })
  
  
  observeEvent(input$bili_risk, {
    if (input$bili_risk == "maak een keuze") {
      hideTab(inputId = "output", target = "Bilicurve")
      showTab(inputId = "output", target = "Maak een keuze")
      updateTabsetPanel(session, "output", selected = "Maak een keuze")
    } else {
      showTab(inputId = "output", target = "Bilicurve")
      hideTab(inputId = "output", target = "Maak een keuze")
      updateTabsetPanel(session, "output", selected = "Bilicurve")
    }
  })
  
  observeEvent(input$advanced, {
    if (input$advanced == "ja") {
      hideTab(inputId = "main", target = "Preterm - biliwaarden")
      hideTab(inputId = "main", target = "Aterm - biliwaarden")
    }
  })
  
  # format text to yyy-mm-dd hh:mm:ss
  vals <- reactiveValues()
  
  
  observe({
    inFile <- input$file_aterm
    if (is.null(inFile)) {
      testdatetime <- paste(input$geboortedag, hm(input$geboorteuur))
      testdatetime <-
        as.POSIXct(testdatetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
      vals$initial_date <- testdatetime
    }
    else {
      df_datetime <- readxl::read_excel(inFile$datapath, skip = 2)
      vals$initial_date <-
        df_datetime %>% pull(geboorte) %>% first()
      updateDateInput(session, "geboortedag", value = vals$initial_date)
      updateTimeInput(session, "geboorteuur", value = as.POSIXct(vals$initial_date, tz = "UTC"))
      PML_update <-
        df_datetime %>% pull(`PML bij geboorte`) %>% first()
      updateTextInput(session, "PML_geboorte", value = PML_update)
      risk_update <- df_datetime %>% pull(`risicofactoren`) %>% first()
      updateSelectInput(session, "bili_risk", selected = risk_update)
    }
    
  })
  
  observe({
    testdatetime2 <- paste(input$afnamedag1, hm(input$afnameuur1))
    testdatetime2 <-
      as.POSIXct(testdatetime2, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date1 <- testdatetime2
  })
  
  observe({
    testdatetime3 <- paste(input$afnamedag2, hm(input$afnameuur2))
    testdatetime3 <-
      as.POSIXct(testdatetime3, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date2 <- testdatetime3
  })
  
  observe({
    testdatetime4 <- paste(input$afnamedag3, hm(input$afnameuur3))
    testdatetime4 <-
      as.POSIXct(testdatetime4, format = "%Y-%m-%d %H:%M", tz = "UTC")
    vals$to_date3 <- testdatetime4
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
  # format e.g. 23+1/7 to 23.14
  calc <- function(x)
    eval(parse(text = x))
  
  # GET-parameters worden bij het laden toegepast, los van de zichtbaarheid van
  # de output. (De parsing zat vroeger in newData(); daardoor werkten de
  # GET-parameters niet meer zodra de bilicurve standaard verborgen is.)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['naam']])) {
      updateTextInput(session, "naam", value = query[['naam']])
    }
    if (!is.null(query[['advanced']])) {
      updateSelectInput(session, "advanced", selected = query[['advanced']])
    }
    if (!is.null(query[['prematuur']])) {
      updateSelectInput(session, "prematuur", selected = query[['prematuur']])
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
      updateTimeInput(session, "afnameuur1", value = strptime(query[['afnameuur1']], "%H:%M"))
    }
    if (!is.null(query[['afnamedag2']])) {
      updateTextInput(session, "afnamedag2", value = query[['afnamedag2']])
    }
    if (!is.null(query[['afnameuur2']])) {
      updateTimeInput(session, "afnameuur2", value = strptime(query[['afnameuur2']], "%H:%M"))
    }
    if (!is.null(query[['afnamedag3']])) {
      updateTextInput(session, "afnamedag3", value = query[['afnamedag3']])
    }
    if (!is.null(query[['afnameuur3']])) {
      updateTimeInput(session, "afnameuur3", value = strptime(query[['afnameuur3']], "%H:%M"))
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
    if (!is.null(query[['PT_start_GET']])) {
      updateTextInput(session, "PT_start_GET", value = query[['PT_start_GET']])
    }
    if (!is.null(query[['PT_stop_GET']])) {
      updateTextInput(session, "PT_stop_GET", value = query[['PT_stop_GET']])
    }
    if (!is.null(query[['PT_aantalLampen_GET']])) {
      updateTextInput(session, "PT_aantalLampen_GET", value = query[['PT_aantalLampen_GET']])
    }
    if (!is.null(query[['PML_geboorte_GET']])) {
      updateTextInput(session, "PML_geboorte_GET", value = query[['PML_geboorte_GET']])
    }
  })

  newData <- reactive({
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
    
    if (length(value) == 1 && !is.na(value) &&
        nchar(value) == nchar(as.character(Sys.Date()))) {
      value <- paste(value, "00:00:00 ")
    }
    
    if (input$advanced == "ja") {
      geboorte_GET <- as.character(input$geboorte_GET)
      PML_geboorte_GET <- as.character(input$PML_geboorte_GET)
      afname_GET <- as.character(input$afname_GET)
      bili_GET <- as.character(input$bili_GET)
      PML_GET <- as.character(input$PML_GET)
      PT_start_GET <- as.character(input$PT_start_GET)
      PT_aantalLampen_GET <- as.character(input$PT_aantalLampen_GET)
      PT_stop_GET <- as.character(input$PT_stop_GET)
      annotation <- "staal"
      
      if (input$prematuur == "nee") {
        geboorte_GET_POSIX <-
          as.POSIXct(unlist(strsplit(geboorte_GET, split = ",")), format = "%Y-%m-%d %H:%M", tz = "UTC")
        afname_GET_POSIX <-
          as.POSIXct(unlist(strsplit(afname_GET, split = ",")), format = "%Y-%m-%d %H:%M", tz = "UTC")
        PML_geboorte_GET <- calc(unlist(strsplit(PML_geboorte_GET, split = ",")))
        
        PT_start_GET_POSIX <-
          as.POSIXct(unlist(strsplit(PT_start_GET, split = ",")), format = "%Y-%m-%d %H:%M", tz = "UTC")
        PT_stop_GET_POSIX <-
          as.POSIXct(unlist(strsplit(PT_stop_GET, split = ",")), format = "%Y-%m-%d %H:%M", tz = "UTC")
        PT_aantalLampen_GET_split  <- as.character(unlist(strsplit(PT_aantalLampen_GET, split = ",")))
        
        if ((length(PT_start_GET_POSIX) == length(PT_stop_GET_POSIX))) {
          geboorte_GET_POSIX <-
            as.POSIXct(unlist(strsplit(geboorte_GET, split = ",")),
                       format = "%Y-%m-%d %H:%M",
                       tz = "UTC")
          
          df_PT <- tibble(PT_start = PT_start_GET_POSIX,
                          PT_stop = PT_stop_GET_POSIX,
                          geboorte = geboorte_GET_POSIX, 
                          PT_aantalLampen = PT_aantalLampen_GET_split,
                          specified = TRUE)
          df_PT$diff_days_PT_start <-
            as.character(difftime(df_PT$PT_start, df_PT$geboorte, units = "days"))
          df_PT$diff_days_PT_stop <-
            as.character(difftime(df_PT$PT_stop, df_PT$geboorte, units = "days"))
        } else {
          df_PT <- tibble(PT_start = NA,
                          diff_days_PT_start = NA,
                          diff_days_PT_stop = NA,
                          PT_stop = NA,
                          geboorte = NA, 
                          PT_aantalLampen = NA, specified = FALSE)
        }
        
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
        df_GET$diff_days <-
          as.character(difftime(df_GET$afname, df_GET$geboorte, units = "days"))
        df2 <-
          df_GET %>% mutate(
            geboorte =  as.character(
              as.POSIXct(geboorte_GET_POSIX, origin = "1970-01-01", tz = "UTC")
            ) ,
            afnamemoment =  as.character(
              as.POSIXct(afname_GET_POSIX, origin = "1970-01-01", tz = "UTC")
            ) ,
            `tijd in uren` = as.double(diff_hours),
            `tijd in dagen` = as.double(diff_days),
            biliwaarde = as.double(bili),
            `PML bij geboorte` = as.double(PML_geboorte_GET),
            annotation = "staal"
          )   %>% select(geboorte,
                                                          afnamemoment,
                                                          `tijd in uren`,
                                                          `tijd in dagen`,
                                                          `PML bij geboorte`,
                                                          biliwaarde,
                                                          annotation)
        #df2
        list(df = df2, df_PT = df_PT)
      } else {
        
        PT_start_GET_split  <- as.character(unlist(strsplit(PT_start_GET, split = ",")))
        PT_stop_GET_split  <- as.character(unlist(strsplit(PT_stop_GET, split = ",")))
        PT_aantalLampen_GET_split  <- as.character(unlist(strsplit(PT_aantalLampen_GET, split = ",")))
        #if ((length(PT_start_GET_split) == length(PT_stop_GET_split))) {
        if (all(sapply(list(length(PT_start_GET_split),length(PT_stop_GET_split),length(PT_aantalLampen_GET_split)), function(x) x == length(PT_aantalLampen_GET_split)))) { 
          df_PT <- tibble(PT_start = PT_start_GET_split,
                          PT_stop = PT_stop_GET_split, 
                          PT_aantalLampen = PT_aantalLampen_GET_split,
                          specified = TRUE)
          df_PT <- df_PT %>% rowwise() %>%
            mutate(PT_start = calc(PT_start)) %>%
            mutate(PT_stop = calc(PT_stop) +0.035)
        } else {
          df_PT <- tibble(PT_start = NA,
                          PT_stop = NA, 
                          PT_aantalLampen = NA, specified = FALSE)
        }
        
        df2 <- tibble(time_HR = NA,
                      value = NA,
                      annotation = "staal")
        bili_GET_split <-
          as.numeric(unlist(strsplit(bili_GET, split = ",")))
        PML_GET <-
          as.character(unlist(strsplit(PML_GET, split = ",")))
        preterm_df <- tibble(PML_GET = PML_GET,
                             biliprem = bili_GET_split)
        preterm_df <- preterm_df %>% rowwise() %>%
          mutate(PML = calc(PML_GET))
        preterm_df <-
          preterm_df %>% mutate(`postmenstruele leeftijd` = PML,
                                biliwaarde = biliprem) %>% select(`postmenstruele leeftijd`, biliwaarde) %>% filter(biliwaarde > 0)
        list(df = preterm_df, df_PT = df_PT)
      }
    } else if (input$prematuur == "nee") {
      df2 <-
        tibble(
          geboorte = as.character(as.POSIXct(
            c(vals$initial_date), origin = "1970-01-01", tz = "UTC"
          )),
          afnamemoment = as.character(as.POSIXct(
            c(
              vals$to_date1,
              vals$to_date2,
              vals$to_date3,
              vals$to_date4,
              vals$to_date5,
              vals$to_date6,
              vals$to_date7
            ),
            origin = "1970-01-01",
            tz = "UTC"
          )),
          `tijd in dagen` = as.double(c(
            value1, value2, value3, value4, value5, value6, value7
          )),
          `tijd in uren` = as.double(c(
            value1, value2, value3, value4, value5, value6, value7
          )) * 24,
          biliwaarde = as.double(c(
            bili1, bili2, bili3, bili4, bili5, bili6, bili7
          )),
          annotation = "staal",
          `PML bij geboorte` = input$PML_geboorte
        )
      list(df = df2)
    }
    else if (input$prematuur == "ja") {
      preterm_df <-
        tibble(
          `postmenstruele leeftijd` = c(calc(input$PML1), calc(input$PML2), calc(input$PML3)),
          biliwaarde = c(input$biliprem1, input$biliprem2, input$biliprem3)
        )
      list(df = preterm_df)
    }
  })
  
  output$time_output1 <- DT::renderDataTable({
    tr_ <- tr()
    inFile_aterm <- input$file_aterm
    inFile_preterm <- input$file_preterm
    if (is.null(inFile_aterm) && is.null(inFile_preterm)) {
      df <- newData()$df
    }
    else {
      if (!is.null(inFile_aterm) && input$prematuur == "nee") {
        df <- readxl::read_excel(inFile_aterm$datapath, skip = 2)
        df$`PML bij geboorte` <- as.character(df$`PML bij geboorte`)
        df <- bind_rows(df, newData()$df)
      }
      else if (!is.null(inFile_preterm) &&
               input$prematuur == "ja") {
        df <- readxl::read_excel(inFile_preterm$datapath, skip = 2)
        df <- bind_rows(df, newData()$df)
      }
    }
    
    df <- df %>% filter(biliwaarde > 0)
    df <- df %>% mutate_if(is.numeric, ~ round(., 2))
    
    df$risicofactoren <- input$bili_risk
    DT::datatable({
      df
    },
    caption = tr_$t("Je kan de tabel opslaan via de Excel knop om nadien terug te importeren in de tool om extra waarden toe te voegen. Belangrijk: doe zelf geen aanpassingen aan de Excel."),
    extensions = 'Buttons',
    
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'frtBip',
      buttons = list(list(extend = "excel",
                          text = tr_$t("Opslaan als Excel-bestand")))
    ),
    rownames = FALSE,
    
    class = "display")
    
    
    
  })
  
  
  
  output$bilicurve <- renderPlot({
    tr_ <- tr()

    if (input$prematuur == "nee") {
      df <- tibble(`tijd in dagen` = NA, time = NA, biliwaarde = NA, bilirubin = NA, annotation = NA, highlight = NA)
      
      inFile <- input$file_aterm
      if (is.null(inFile)) {
        df2 <- newData()$df
      }
      else {
        #df2 <- readxl::read_excel("/Users/rmvpaeme/Downloads/Indicatie voor fototherapie-6.xlsx", skip = 2)
        df2 <- readxl::read_excel(inFile$datapath, skip = 2)
        df2$`PML bij geboorte` <- as.character(df2$`PML bij geboorte`)
        df2 <- bind_rows(df2, newData()$df)
      }
      ggplot_text <- tr_$t("Laden...")
      
      
      # read the dataframe 
      PML_geboorte <- df2 %>% pull(`PML bij geboorte`) %>% first()
      if (input$bili_risk == "nee"){
        highlight = NA
        ggplot_text <- tr_$t("Geen risicofactoren voor neurotoxiciteit")
        df <- read_tsv("./data/all_norisk.tsv")
        df <- df %>% filter(!is.na(bilirubin))
        df <- df %>% arrange(annotation,time)
        df <- df %>% 
          group_by(annotation) %>%
          filter(!duplicated(bilirubin))
        max_vals <- df  %>% group_by(annotation)  %>% summarise(bilirubin = max(bilirubin, na.rm = TRUE)) %>% mutate(time = 336) %>% filter(!is.na(annotation))
        df <- rbind(max_vals, df)
       
        if (PML_geboorte < 36 && PML_geboorte >= 35){
          #df <- df %>% filter(annotation == "35w_norisk")
          df$annotation <- sub("35w_norisk", "35 weken", df$annotation)
          highlight <- "35 weken"
        } else if (PML_geboorte < 37 && PML_geboorte >= 36){
          #df <- df %>% filter(annotation == "36w_norisk")
          df$annotation <- sub("36w_norisk", "36 weken", df$annotation)
          highlight <- "36 weken"
        } else if (PML_geboorte < 38 && PML_geboorte >= 37){
          #df <- df %>% filter(annotation == "37w_norisk")
          df$annotation <- sub("37w_norisk", "37 weken", df$annotation)
          highlight <- "37 weken"
        } else if (PML_geboorte < 39 && PML_geboorte >= 38){
          #df <- df %>% filter(annotation == "38w_norisk")
          df$annotation <- sub("38w_norisk", "38 weken", df$annotation)
          highlight <- "38 weken"
        } else if (PML_geboorte < 40 && PML_geboorte >= 39){
          #df <- df %>% filter(annotation == "39w_norisk")
          df$annotation <- sub("39w_norisk", "39 weken", df$annotation)
          highlight <- "39 weken"
        } else if (PML_geboorte >= 40){
          #df <- df %>% filter(annotation == "40w_norisk")
          df$annotation <- sub("40w_norisk", "≥ 40 weken", df$annotation)
          highlight <- "≥ 40 weken"
        }
      } else if (input$bili_risk == "ja"){
        highlight = NA
        ggplot_text <- tr_$t("Eén of meer risicofactoren voor neurotoxiciteit")
        df <- read_tsv("./data/all_risk.tsv")
        df <- df %>% filter(!is.na(bilirubin))
        df <- df %>% arrange(annotation,time)
        df <- df %>% 
          group_by(annotation, bilirubin) %>%
          filter(!duplicated(bilirubin))
        max_vals <- df  %>% group_by(annotation)  %>% summarise(bilirubin = max(bilirubin, na.rm = TRUE)) %>% mutate(time = 336) %>% filter(!is.na(annotation))
        df <- rbind(max_vals, df)
        
        if (PML_geboorte < 36 && PML_geboorte >= 35){
          #df <- df %>% filter(annotation == "35w_risk")
          df$annotation <- sub("35w_risk", "35 weken", df$annotation)
          highlight <- "35 weken"
        } else if (PML_geboorte < 37 && PML_geboorte >= 36){
          #df <- df %>% filter(annotation == "36w_risk")
          df$annotation <- sub("36w_risk", "36 weken", df$annotation)
          highlight <- "36 weken"
        } else if (PML_geboorte < 38 && PML_geboorte >= 37){
          #df <- df %>% filter(annotation == "37w_risk")
          df$annotation <- sub("37w_risk", "37 weken", df$annotation)
          highlight <- "37 weken"
        } else if (PML_geboorte >= 38){
          #df <- df %>% filter(annotation == "38w_risk")
          df$annotation <- sub("38w_risk", "≥ 38 weken", df$annotation)
          highlight <- "≥ 38 weken"
        }
      }
      
      df <-
        df %>% mutate(`tijd in dagen` = time/24, biliwaarde = bilirubin) %>% select(-c(bilirubin, time))

      df <- bind_rows(df, df2)
      df_TcB = tibble(
        annotation = "drempel serumbevestiging TcB zonder risicofactoren",
        `tijd in dagen` = c(1, 1.5, 2, 3, 4),
        biliwaarde = c(8, 10, 12, 14, 17)
      )
      df_all <- bind_rows(df, df_TcB)
      # extract the most recent entered value to annotate the corresponding thresholds for LR, MR and HR on the plot
      x_seq = df2 %>% filter(biliwaarde > 0) %>% pull(`tijd in dagen`)
      
      intersections <- df_all %>% filter(annotation != "staal") %>% filter(annotation == highlight | annotation == "drempel serumbevestiging TcB zonder risicofactoren") %>%
        group_by(annotation) %>%
        dplyr::reframe(interpolated = approx(x = `tijd in dagen`, y = biliwaarde, xout = x_seq)$y) %>%
        mutate(x_seq = rep(x_seq, 2)) %>%
        arrange(annotation, x_seq) %>%
        group_by(annotation) %>%
        summarise(across(everything(), last)) %>% mutate(interpolated = round(interpolated, 1))
      
      last_intersect <- intersections %>% pull(x_seq) %>% unique()
    }
    
    else if (input$prematuur == "ja") {
      inFile <- input$file_preterm
      if (is.null(inFile)) {
        preterm_df <- newData()$df
      }
      else {
        preterm_df <- readxl::read_excel(inFile$datapath, skip = 2)
        preterm_df <- bind_rows(preterm_df, newData()$df)
      }
      
    }
    
    
    
    if (input$prematuur == "nee") {
      df_PT <- newData()$df_PT
      if ((length(input$PT_start_GET) == length(input$PT_stop_GET)) &&
          (sum(!is.na(input$PT_stop_GET)) == sum(!is.na(input$PT_start_GET))) && !is.null(df_PT$diff_days_PT_start) ) {
        PT_ggplot <- geom_rect(data = df_PT, aes(xmin = c(as.numeric(diff_days_PT_start)), xmax = c(as.numeric(diff_days_PT_stop)),
                                                 ymin = -Inf, ymax =  Inf, fill = PT_aantalLampen), 
                               alpha = 0.7, inherit.aes = FALSE) 
        PT_legend <-  scale_fill_manual(labels = c(tr_$t("1 lamp"), tr_$t("2 lampen"), tr_$t("3 lampen")), name = tr_$t("intensiteit fototherapie"), values = c("1" = "#A3BE8C", "2" = "#EBCB8B", "3" = "#BF616A"))
        
        
      } else {
        PT_ggplot <- NULL
        PT_legend <- NULL
      }

      # Lege toestand: toon een vriendelijke boodschap (en vermijd de min/max
      # Inf-warnings) zolang er geen enkele bilirubinewaarde is ingevuld.
      validate(need(
        nrow(df %>% filter(annotation == "staal", biliwaarde > 0)) > 0,
        tr_$t("Voer minstens één bilirubinewaarde in om de curve te tonen.")
      ))

      if (df %>% filter(annotation == "staal", biliwaarde > 0) %>% pull(`biliwaarde`) %>% min() > 6) {
        lowest_ylim <- 6
      } else{ 
        lowest_ylim <- df %>% filter(annotation == "staal", biliwaarde > 0) %>% pull(`biliwaarde`) %>% min() 
      }

      if (df %>% filter(annotation == "staal", biliwaarde > 0) %>% pull(`biliwaarde`) %>% max() < 22.5) {
        highest_ylim <- 22.5
      } else{ 
        highest_ylim <- df %>% filter(annotation == "staal", biliwaarde > 0) %>% pull(`biliwaarde`) %>% max() 
      }      
      
      g <-
        ggplot(df, aes(y = biliwaarde, x = `tijd in dagen`, col = annotation)) +  geom_vline(
          xintercept = last_intersect,
          color = "black",
          linetype = "dashed",
          alpha = 0.3
        ) + 
        #stat_smooth(aes(y = biliwaarde, x = `tijd in dagen`), formula = y ~ s(x, k = 20), method = "gam", se = FALSE) + 
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
        geom_line(data = df %>% filter(annotation != "staal") %>% filter(annotation == highlight), linewidth = 1)  +
        geom_line(data = df %>% filter(annotation != "staal") %>% filter(annotation != highlight), aes(group = annotation, col = annotation), linewidth = 0.5, color = "gray80")  +
        theme_bw() + xlab(tr_$t("leeftijd in dagen")) + ylab(tr_$t("bilirubine, mg/dL")) +
        geom_line(data = df_TcB, linetype = "dashed", linewidth = 1) +
        geom_point(
          data = df %>% filter(annotation == "staal", biliwaarde > 0),
          aes(y = biliwaarde, x = `tijd in dagen`, col = annotation),
          size = 3, color = "#5E81AC"
        ) + geom_line(data = df %>% filter(annotation == "staal", biliwaarde > 0), aes(group = annotation), color = "#5E81AC") + labs(color = tr_$t("legende"), subtitle = paste0(ggplot_text, "\n", "°", df2 %>% pull(geboorte) %>% first()) ) + theme(
          text = element_text(size = 20),
          legend.position = "bottom",
          legend.direction="vertical",
          #legend.box = "horizontal",
          #legend.title = element_blank()
        ) + scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7,8,9,10,11,12,13,14),
                               limits = c(0, 14.1)) +
        scale_y_continuous(breaks = seq(0,30, by = 2),
                           limits = c(lowest_ylim, highest_ylim)) +
        scale_color_manual(values = c( "#4C566A", "#88C0D0")) +
        theme(plot.subtitle=element_text(size=18)) +
        geom_point(data = intersections %>% filter(x_seq > 0) , aes(x = x_seq, y = interpolated)) + PT_ggplot + PT_legend
      
      g + guides(color = guide_legend(nrow = 5)) + plot_frame
    } else{
      
      df_PT <- newData()$df_PT
      if ((length(input$PT_start_GET) == length(input$PT_stop_GET)) &&
          (sum(!is.na(input$PT_stop_GET)) == sum(!is.na(input$PT_start_GET))) && !is.null(df_PT$PT_start) ) {
        PT_ggplot <- geom_rect(data = df_PT, aes(xmin = c(as.numeric(PT_start)), xmax = c(as.numeric(PT_stop)),
                                   ymin = -Inf, ymax =  Inf, fill = PT_aantalLampen),
                    alpha = 0.7, inherit.aes = FALSE)
        PT_legend <-  scale_fill_manual(labels = c(tr_$t("1 lamp"), tr_$t("2 lampen"), tr_$t("3 lampen")), name = tr_$t("intensiteit fototherapie"), values = c("1" = "#A3BE8C", "2" = "#EBCB8B", "3" = "#BF616A"))
      } else {
        PT_ggplot <- NULL
        PT_legend <- NULL
      }

      # Lege toestand voor de prematuurcurve.
      validate(need(
        nrow(preterm_df %>% filter(biliwaarde > 0)) > 0,
        tr_$t("Voer minstens één bilirubinewaarde in om de curve te tonen.")
      ))

      fill_PT <- "#88C0D0"
      fill_ET <- "#5E81AC"
      alpha = 0.1
      
      ggplot(
        preterm_df %>% filter(biliwaarde > 0),
        aes(x = `postmenstruele leeftijd`, y = biliwaarde)
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
          #legend.box = "horizontal",
          #legend.title = element_blank()
        ) +
        labs(caption = tr_$t("gearceerde zone = overweeg fototherapie (onder) of wisseltransfusie (boven)"),
             x = tr_$t("gestationele leeftijd (weken)"),
             y = tr_$t("totaal serumbilirubine (mg/dL)")) +
        PT_ggplot + PT_legend +
        geom_vline(xintercept = 35, linetype="dashed", 
                   color = "grey", linewidth=0.8, ) +annotate(geom = "text", x=35, y=1, vjust = -0.2, label= tr_$t("aterme waarden"), angle = "90",  color = "gray20") +
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
        ) +
        plot_frame


    }

  })
}

# Run the application
shinyApp(ui = ui, server = server)

