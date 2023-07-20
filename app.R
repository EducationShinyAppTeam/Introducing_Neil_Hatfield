# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(dplyr)
library(ggplot2)
library(stringr)
# library(shinya11y) # An accessibility checker (won't be part of final app)
# library(shinyAce)
# library(knitr)
# library(shinydashboardPlus)
# library(fresh)
# library(howler)

# Create Custom Theme ----
# mytheme <- create_theme(
#   # adminlte_color(
#   #   light_blue = "#437C5E"
#   # ),
#   # adminlte_sidebar(
#   #   dark_bg = "#28DEE9",
#   #   dark_hover_bg = "#81A1C1",
#   #   dark_color = "#2E3440"
#   # ),
#   # adminlte_global(
#   #   content_bg = "#17FF88",
#   #   box_bg = "#18FF39", 
#   #   info_box_bg = "#D8DEE9"
#   # ),
#   bs_vars_button(
#     default_bg = "#18FF39"
#   )
# )

# source("aceText.R")
source("svgTest.R")

# audio_files_dir <- system.file("examples/_audio", package = "howler")
# addResourcePath("sample_audio", audio_files_dir)
# audio_files <- file.path("sample_audio", list.files(audio_files_dir, ".mp3$"))

# Load additional dependencies and setup functions ----
freedmanDiaconis <- function(x){
  binwidth <- ifelse(
    test = IQR(x) == 0,
    yes = 0.1,
    no = 2 * IQR(x) / (length(x)^(1/3))
  )
  return(binwidth)
}
## Load Data from GitHub
# centralDataPath <- "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/songKnowledge_"
# spring22 <- read.csv(
#   file = paste0(centralDataPath, "Spring2022.csv")
# )
# fall22 <- read.csv(
#   file = paste0(centralDataPath, "Fall2022.csv")
# )
# spring23 <- read.csv(
#   file = paste0(centralDataPath, "Sp23.csv")
# )
## Load Alt Text----
altText <- read.table(
  file = "altText.csv",
  header = TRUE,
  sep = ",",
  quote = "\"" # Allows for the ' in term
)

## Apply consistent column names and bind into one file
# names(spring22) <- c("year", "score")
# names(fall22) <- c("year", "score")
# names(spring23) <- c("year", "score")
# spring22$term <- "Spring '22"
# fall22$term <- "Fall '22"
# spring23$term <- "Spring '23"
# songKnowledgeData <- rbind(spring22, fall22, spring23)
# remove(list = c("spring22", "fall22", "spring23"))

## Clean Data
# songKnowledgeData <- songKnowledgeData %>%
#   mutate(
#     year = str_to_sentence(year),
#     year = case_when(
#       year == "Other" ~ "Sophomore",
#       year == "Senior" ~ "Senior+",
#       .default = year
#     )
#   )
# songKnowledgeData$year <- factor(
#   x = songKnowledgeData$year,
#   levels = c("Sophomore", "Junior", "Senior+"),
#   ordered = TRUE
# )

# Define UI for App ----
ui <- list(
  # use_tota11y(), # place once in the UI to include accessibility checking tools
  dashboardPage(
    # freshTheme = mytheme,
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
        menuItem("Testing Page", tabName = "testing", icon = icon("flask")),
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
          # howler(elementId = "sound", audio_files),
          # tags$p(
          #   howlerPlayPauseButton("sound"),
          #   "Play or pause the track"
          # ),
          # tags$p(
          #   howlerStopButton("sound"),
          #   "Stop the track and return to the start"
          # ),
          neuralNet,
          p("Hi! Welcome to one of my Shiny apps where you can learn a bit about me."),
          p("I have been at Penn State since August 2019, previsously I was
            at the University of Northern Colorado and Arizona State University.
            My research interests mostly centered in the realm of Statistics
            Education. However, I also do some research in diversity, equity, and
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
          h2("Instructions"),
          p("Besides learning a bit about me, you can use this app to explore
            three semesters' worth of data from a class activity I do with my
            STAT 461-ANOVA students. Head to the Explore page."),
          p("test 1"),
          # tags$figure(
          #   class = "centerGettyImage",
          #   HTML("<a id='nI7TweJuToNK2F--nVFLUQ' class='gie-single' href='http://www.gettyimages.com/detail/1254565557' target='_blank' style='color:#000000 !important;text-decoration:none;font-weight:normal !important;border:none;display:inline-block;'>Embed from Getty Images</a>
          #        <script>window.gie=window.gie||function(c){(gie.q=gie.q||[]).push(c)};gie(function(){gie.widgets.load({id:'nI7TweJuToNK2F--nVFLUQ',sig:'4c3HTKF5p9quZvkc01PiHJq4yLZvDQQGQ_rPM0J_e5E=',w:'509px',h:'339px',items:'1254565557',caption: true ,tld:'com',is360: false })});</script>
          #        <script src='//embed-cdn.gettyimages.com/widgets.js' charset='utf-8' async></script>"),
          #   tags$figcaption("This is a basketball photo.")
          # ),
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
          p("In my STAT 461-Analysis of Variance course, we design and carry out
            a one-factor study to explore whether a student's year in school
            impacts how well they do in a pub trivia style song quiz. For the
            quiz, approximately 20 seconds of 10 songs is played. Players have to
            correctly identify the full title of the song and the primary artist.
            Players earn one point for each correct title and each correct artist."),
          p("Explore the data sets below form a hypothesis for whether students'
            year in school impacts how well they perform."),
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
                ),
                sliderInput(
                  inputId = "temp1",
                  label = "year",
                  min = 2016,
                  max = 2023,
                  value = 2016,
                  sep = ""
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
        ### Testing Page ----
        tabItem(
          tabName = "testing",
          withMathJax(),
          h2("Neil's Code Testing Page"),
          tabsetPanel(
            id = "testing1",
            type = "tabs",
            tabPanel(
              title = 'Exercises', 
              value = 'panel2',
              br(),
              h3("Instructions"),
              p("You can try the following questions. Test your code with the
                following R script box with the R Markdown output under the
                'Knitted Output' header. In each turn, 10 questions will be 
                randomly drawn from the question bank. Uncomment the sample 
                code to start to explore."
              ),
              fluidRow(
                column(
                  width = 6,
                  h2("Exercises"),
                  uiOutput('progress'),
                  wellPanel(
                    uiOutput("question"),
                    uiOutput("options"),
                    br(),
                    selectInput(
                      inputId = "answer", 
                      label = "Select your answer from below", 
                      choices = c("","A", "B", "C")
                    ),
                    uiOutput("mark")
                  ),
                  bsButton(
                    inputId = 'submit', 
                    label = 'Submit',
                    disabled = TRUE, 
                    style = "success"
                  ),
                  bsButton(
                    inputId = "nextq",
                    label = "Next",
                    disabled = TRUE
                  ),
                  bsButton(
                    inputId = "reset",
                    label = "Restart", 
                    style = "danger",
                    disabled = TRUE
                  ),
                  h3("Try Your Code"),
                  # aceEditor(
                  #   outputId = "rmd",
                  #   mode = "markdown", 
                  #   value = initialAceText,
                  #   theme = "xcode",
                  #   wordWrap = TRUE
                  # ),
                  bsButton(
                    inputId = "eval", 
                    label = "Run"
                  )
                ),
                column(
                  width = 6,
                  h3("Knitted Output"),
                  uiOutput("knitDoc")
                )
              )
            )
          )
        ),
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p( # Each reference is in its own paragraph
            class = "hangingindent", # you must set this class argument
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v0.61.1). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield., N. J. (2023). boastUtils: BOAST utilities.
            (v0.1.11.2). [R Package]. Available from
            https://github.com/EducationShinyappTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2021). shinydashboard: Create dashboards
            with 'Shiny'. (v0.7.2). [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J.J., Sievert, C., Schloerke, B.,
            Xie, Y., Allen, J., McPherson, J., Dipert, A., and Borges, B. (2022).
            shiny: Web application framework for R. (v1.7.4). [R Package].
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Fleck, M. (2022). Picture of Neil J. Hatfield."
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2023). shinyWidgets: Custom
            inputs widgets for shiny. (v0.7.6). [R Package]. Availble from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            (v3.4.2). [R Package]. New York:Springer-Verlag. Available from
            https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2022). stringr: Simple, consistent wrappers for common
            string operations. (v1.5.0). [R Package] Available from
            https://CRAN.R-project.org/package=stringr"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K., and Vaughan, D.
            (2023). dplyr: A grammar of data manipulation. (v1.1.2). [R Package].
            Available from https://CRAN.R-project.org/package=dplyr"
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
  selectedAltText <- reactiveVal("Plot Coming")
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
  # observeEvent(
  #   eventExpr = input$makePlot,
  #   handlerExpr = {
  #     ### Create initial plot object
  #     displayPlot <- "NA"
  #     ### Filter Data, if necessary
  #     if (input$termPicked != "All") {
  #       plotData <- songKnowledgeData %>%
  #         filter(term == input$termPicked)
  #     } else {
  #       plotData <- songKnowledgeData
  #     }
  # 
  #     ### Base Plot with themeing
  #     basePlot <- ggplot(
  #       data = plotData,
  #       mapping = aes(x = score)
  #     )
  # 
  #     if (input$byYear) {
  #       basePlot <- basePlot + aes(fill = year)
  #     }
  # 
  #     basePlot <- basePlot +
  #       theme_bw() +
  #       theme(
  #         text = element_text(size = 18),
  #         legend.position = "bottom"
  #       ) +
  #       labs(
  #         x = "Score",
  #         fill = "Year"
  #       ) +
  #       scale_fill_manual(values = psuPalette)
  # 
  #     ### Add Plot Type
  #     if (input$plotType == "Density" & input$byYear) {
  #       displayPlot <- plotData %>%
  #         dplyr::filter(year != "Sophomore") %>%
  #         ggplot(mapping = aes(x = score, fill = year)) +
  #         geom_density(
  #           bounds = c(0, 20),
  #           alpha = 0.75,
  #           na.rm = TRUE
  #         ) +
  #         scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  #         theme_bw() +
  #         theme(
  #           text = element_text(size = 18),
  #           legend.position = "bottom"
  #         ) +
  #         labs(
  #           x = "Score",
  #           fill = "Year",
  #           caption = "The one Sophomore scoring 8 isn't shown."
  #         ) +
  #         scale_fill_manual(values = psuPalette)
  #     } else if (input$plotType == "Box plot") {
  #       displayPlot <- basePlot +
  #         geom_boxplot() +
  #         theme(
  #           axis.text.y = element_blank(),
  #           axis.ticks.y = element_blank()
  #         )
  #     } else if (input$plotType == "Histogram") {
  #       displayPlot <- basePlot +
  #         geom_histogram(
  #           color = "black",
  #           binwidth = freedmanDiaconis,
  #           closed = "left",
  #           boundary = 0,
  #           position = "identity",
  #           alpha = 0.75
  #         ) +
  #         scale_y_continuous(expand = expansion(add = c(0, 2)))
  #     } else if (input$plotType == "Density") {
  #       displayPlot <- basePlot +
  #         geom_density(
  #           bounds = c(0, 20),
  #           alpha = 0.75,
  #           na.rm = TRUE
  #         ) +
  #         scale_y_continuous(expand = expansion(mult = c(0, 0.01)))
  #     }
  # 
  #     ### Alt text ----
  #     selectedAltText(
  #       altText %>%
  #         filter(
  #           plotType == input$plotType &
  #             term == input$termPicked &
  #             byTerm == input$byYear
  #         ) %>%
  #         select(altText) %>%
  #         as.character()
  #     )
  # 
  #     ### Display the plot
  #     output$songPlot <- renderPlot(
  #       expr = {
  #         validate(
  #           need(
  #             expr = !is.na(displayPlot),
  #             message = "Plot could not be generated. Contact app developer."
  #           )
  #         )
  #         displayPlot
  #       },
  #       alt = selectedAltText()
  #     )
  #   },
  #   ignoreNULL = FALSE,
  #   ignoreInit = TRUE
  # )
  
  ## Testing Code Area ----
  observeEvent(
    eventExpr = input$eval,
    handlerExpr = {
      output$knitDoc <- renderUI(
        expr = {
          # HTML(
          #   knit2html(
          #     text = input$rmd,
          #     template = FALSE,
          #     quiet = TRUE
          #   )
          # )
        }
      )
    }
  )


}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
