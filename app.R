# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(dplyr)
library(ggplot2)
library(stringr)
library(shinya11y) # An accessibility checker

# Load additional dependencies and setup functions ----
## Load Data from GitHub
centralDataPath <- "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/songKnowledge_"
spring22 <- read.csv(
  file = paste0(centralDataPath, "Spring2022.csv")
)
fall22 <- read.csv(
  file = paste0(centralDataPath, "Fall2022.csv")
)
spring23 <- read.csv(
  file = paste0(centralDataPath, "Sp23.csv")
)

## Apply consistent column names and bind into one file
names(spring22) <- c("year", "score")
names(fall22) <- c("year", "score")
names(spring23) <- c("year", "score")
spring22$term <- "Spring '22"
fall22$term <- "Fall '22"
spring23$term <- "Spring '23"
songKnowledgeData <- rbind(spring22, fall22, spring23)
remove(list = c("spring22", "fall22", "spring23"))

## Clean Data
songKnowledgeData <- songKnowledgeData %>%
  mutate(
    year = str_to_sentence(year),
    year = case_when(
      year == "Other" ~ "Sophomore",
      year == "Senior" ~ "Senior+",
      .default = year
    )
  )
songKnowledgeData$year <- factor(
  x = songKnowledgeData$year,
  levels = c("Sophomore", "Junior", "Senior+"),
  ordered = TRUE
)

# Define UI for App ----
ui <- list(
  use_tota11y(), # place once in the UI to include accessiblity checking tools
  dashboardPage(
    skin = "red",
    ## Header ----
    dashboardHeader(
      title = "Neil's Intro",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Introducing_Neil_Hatfield")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Introducing Neil Hatfield"),
          tags$figure(
            class = "centerFigure",
            tags$img(
              src = "hatfield-neil-SQ.jpg",
              height = 276,
              alt = "Headshot of Neil Hatfield"
            ),
            tags$figcaption("Neil Hatfield, ", HTML("&copy;"), "M. Fleck")
          ),
          p("Hi! Welcome one of my Shiny apps where you can learn a bit about me."),
          p("I have been at Penn State since August 2019, previsously I was
            at the University of Northern Colorado and Arizona State University.
            My research interests mostly centered in the realm of Statistics
            Education. However, I also do some research in divesity, equity, and
            inclusion in STEM more broadly."),
          p("I've taught a variety of classes including Pre-calculus,
            Brief/Business Calculus, Introductory Statistics (3 different flavors),
            Intro Stats for Teaching (Master's level), Quantitative Methods
            (PhD level), Design of Experiments/ANOVA, Teaching Statistics
            (graduate seminar), and Statistics Education Seminar (graduate level).
            Additionally, I've worked with the Shiny app program since 2019."),
          p("In my spare time I like to cook/bake, ride my road bike, and read.
            I also continue to think about modifications/improvements to various
            courses that I teach. During the regular semester, you can find me
            wearing different tie knots."),
          h3("Instructions"),
          p("Besides learning a bit about me, you can use this app to explore
            three semesters' worth of data from a class activity I do with my
            STAT 461-ANOVA students. Head the Explore page."),
          p("Here is an example list of low contrast colors:"),
          tags$ul(
            tags$li(style = "color: #FF0000;", "Here is red text."),
            tags$li(style = "color: #00FF00;", "Here is green text."),
            tags$li(style = "color: #0000FF;", "Here is blue text.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goToExplore",
              label = "Go To Explore!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Neil J.
            Hatfield.",
            br(),
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 05/18/2023 by NJH.")
          )
        ),
        ### Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Explore the Song Knowledge Data"),
          p("Fill in later"),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              wellPanel(
                selectInput(
                  inputId = "plotType",
                  label = "Select a plot type",
                  choices = c("Box plot", "Histogram", "Density")
                ),
                selectInput(
                  inputId = "termPicked",
                  label = "Select a term",
                  choices = c("Spring '22", "Fall '22", "Spring '23", "All"),
                  selected = "All"
                ),
                checkboxInput(
                  inputId = "byYear",
                  label = "Show data by year in school",
                  value = FALSE
                ),
                bsButton(
                  inputId = "makePlot",
                  label = "Create plot",
                  size = "large",
                  style = "default"
                )
              )
            ),
            column(
              width = 8,
              offset = 0,
              plotOutput(outputId = "songPlot")
            )
          )
        ),
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p("You'll need to fill in this page with all of the appropriate
            references for your app."),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  print(head(songKnowledgeData))

  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "Learn about Neil and explore the Song Knowledge data."
      )
    }
  )

  ## Move to Explore Page ----
  observeEvent(
    eventExpr = input$goToExplore,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore"
      )
    }
  )

  ## Song Knowledge Plot ----
  observeEvent(
    eventExpr = input$makePlot,
    handlerExpr = {

    }
  )


}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
