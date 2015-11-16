#
# ArtemisWar2015: a Shiny application to show off Pardus War statistics.
# Front End component.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by 
# the Free Software Foundation, either version 3 of the License, or 
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#



library(shiny)
library(ggvis)

source("misc.R")


#
# Helper functions that construct and return panels for slotting into the UI
# placeholders.
#
instructionsPanel <- function()
{
    # The documentation should be thought of as whatever a user will need to
    # get started using your application.

    p1 <- 'This application is a first venture into Shiny application
           territory. The purpose of the application is to visualise plots
           based on data gathered from the online game, Pardus.'

    p2 <- 'From the web site:'

    p2b <- tags$blockquote('Pardus is a free graphic-based Massive Multiplayer
           Online Browser Game (MMOBG), also known as a Persistent Browser
           Based Game (PBBG). Set in a futuristic universe, traders, pirates,
           smugglers and other pilots of various professions, races and
           factions strive to gain wealth and fame in space.', cite='Pardus
           web site')

    p3 <- 'At the time of writing, war had broken out in the \'Artemis\' game
           universe amongst the three main factions: the powerful Federation
           versus both the Union (12-Sep-2015 through 11-Nov-2015) and the
           Empire (from 06-Oct-2015 through 14-Nov-2015). This application
           aimed to plot the daily progress of the factions during the war in
           the form of a graph and help visualise the progress of each of the
           three sides. Given the Federation\'s superiority of numbers in the
           game, the option to merge the Union and Empire (a.k.a. the
           \'Unpire\') data was also presented for the benefit of the
           curious.'

    p4a <- 'The application graphs the progress of war points gained in the
           following categories:'

    p4b <- 'Switch to the Plots tab and choose some options. The plot should
           be able to reflect your choices in something approximating real
           time. As the user, you are able to apply filters to the combatants,
           the points category and the time period. Scatter plot, line graph
           and smoothed trend lines are available as well as the ability to
           visualise daily changes.'

    p5 <- 'From a technical point of view, this application uses the R
           Language\'s Base graphics library for plotting. A second graphing
           option was later added - the ggvis library - to take advantage of
           some of its promised interactivity features such as data point
           tooltips. However, this version of the library appears to still be
           under development and fell short of being reliable. By all means,
           try it out but for reliable plots, use the Base Graphics option for
           less frustrating experience. I ran out of patience trying to
           overcome the shortcomings of ggvis and gave up on it, but I left
           the option for revisiting it on another day.'

    p6 <- 'Finally, thanks for taking the time to try out the application.'

    panel <- mainPanel(
               headerPanel(h1("Instructions")),
               p(p1),
               h3('About ', a(href='https://www.pardus.at', 'Pardus')),
               p(p2),
               p(p2b),
               p(p3),
               h3('The Application: Getting Started'),
               p(p4a),
               tags$ul(tags$li('Ship destruction (kills)'),
                       tags$li('Structures razed'),
                       tags$li('War missions completed'),
                       tags$li('Territorial sectors captured'),
                       tags$li('Total war points')),
               p(p4b),
               p(p5),
               p(p6)
             )

    return(panel)
}



dataPanel <- function()
{
    p1 <- 'Below is the raw data underlying the plots. They are split into
           three sections, each corresponding to the combatant filter options
           selectable in the plots.'

    p2 <- 'Note: The Union-Federation war started and ended on 12-Sep-2015 and
           11-Nov-2015 respectively. Unfortunately, the data capture for this
           war is incomplete and only contains the data from 22-Sep-2015
           onwards.'

    panel <- mainPanel(
               headerPanel(h1("Raw Data")),
               p(p1),
               p(p2),
               hr(),
               h3('Federation-Union War'),
               dataTableOutput('tableUF'),
               hr(),
               h3('Federation-Empire War'),
               dataTableOutput('tableFE'),
               hr(),
               h3('Federation vs The Rest War'),
               dataTableOutput('tableUFE'),
               hr()
             )

    return(panel)
}



mainPlotPanel <- function()
{
    panel <- pageWithSidebar(
                 headerPanel(h1('War Data Plots')),

                 sidebarPanel(h2("Plot Criteria"),

                              radioButtons(inputId  = 'plotlibId',
                                           label    = 'Plotting Library:',
                                           choices  = flipStructure(PlottingLibraryIds),
                                           selected = BASE_GRAPHICS,
                                           inline   = FALSE,
                                           width    = NULL),

                              radioButtons(inputId  = 'skirmishId',
                                           label    = 'Skirmish:',
                                           choices  = flipStructure(SkirmishIds),
                                           selected = 'UF',
                                           inline   = FALSE,
                                           width    = NULL),

                              radioButtons(inputId  = 'analysisId',
                                           label    = 'Points Analysis:',
                                           choices  = flipStructure(AnalysisIds),
                                           selected = 'Points',
                                           inline   = FALSE,
                                           width    = NULL),

                              checkboxGroupInput(inputId  = 'plotOptions',
                                                 label    = 'Plot Options:',
                                                 choices  = flipStructure(PlotOptionIds),
                                                 selected = c('SP'),
                                                 inline   = FALSE,
                                                 width    = NULL),

                              conditionalPanel(condition = "input.plotOptions.indexOf('SLG') >= 0",
                                               sliderInput(inputId      = 'loessSpan',
                                                           label        = 'Smoothness Level',
                                                           min          = 0.1,
                                                           max          = 0.95,
                                                           value        = 0.50,
                                                           step         = 0.05,
                                                           round        = FALSE,
                                                           ticks        = TRUE,
                                                           animate      = FALSE,
                                                           width        = NULL,
                                                           sep          = ",",
                                                           pre          = NULL,
                                                           post         = NULL,
                                                           timeFormat   = NULL,
                                                           timezone     = NULL,
                                                           dragRange    = TRUE)
                                              ),

                              dateRangeInput(inputId   = 'dateRange',
                                             label     = 'Date Range',
                                             start     = '2015-09-22',
                                             end       = NULL,
                                             min       = '2015-09-22',
                                             max       = NULL,
                                             format    = "yyyy-mm-dd",
                                             startview = "month",
                                             weekstart = 0,
                                             language  = "en",
                                             separator = " to ",
                                             width     = NULL),

                              radioButtons(inputId  = 'dataSet',
                                           label    = 'Data Selection:',
                                           choices  = flipStructure(dataSetIds),
                                           selected = c('RAW'),
                                           inline   = FALSE,
                                           width    = NULL)

                 ),

    mainPanel(h2("Plot Panel"),
              p('Data last updated:',
                 tags$code(strftime(file.info('FederationEmpireWar.Rda')[, 'mtime'], "%a, %d %b %Y %H:%M:%S %Z"))
               ),

              conditionalPanel(condition = "input.plotlibId == 'BASE'",
                               plotOutput(outputId       = "plotOutput",
                                          width          = "100%",
                                          height         = "600px",
                                          click          = NULL,
                                          dblclick       = NULL,
                                          hover          = NULL,
                                          hoverDelay     = NULL,
                                          hoverDelayType = NULL,
                                          brush          = NULL,
                                          clickId        = NULL,
                                          hoverId        = NULL,
                                          inline         = FALSE)),

              conditionalPanel(condition = "input.plotlibId != 'BASE'",
                               uiOutput("ggvis_ui"),
                               ggvisOutput("ggvis")
                              )
    )
  )

  return(panel)
}



aboutPanel <- function()
{
    p1 <- 'This application was developed using the R Language in conjunction
           with the Shiny library. Also features limited use of the ggvis
           library.'

    p2 <- 'The application was built using R version 3.2.2 (2015-08-14) --
           "Fire Safety", Shiny Version: 0.12.2 and ggvis Version: 0.4.2. It
           was tested using Firefox 41.0.1.'

    p3 <- 'Curious about Pardus? Check it out for yourself. More than just an 
           online game, Pardus produces rich data on social interaction for 
           scientific study and research:'

    p4 <- 'Last, but not least, the player-created Pardus news blog:'

    p5 <- 'Fly safely!'

    panel <- mainPanel(
               headerPanel(h1("About")),
               p(p1),
               tags$ul(tags$li(a(href='https://www.r-project.org', 'R Language')),
                       tags$li(a(href='http://shiny.rstudio.com', 'Shiny')),
                       tags$li(a(href='http://ggvis.rstudio.com', 'ggvis'))),
               p(p2),
               p(p3, a(href="http://www.pardus.at/index.php?section=about_coverage", "Pardus: Awards & Media Coverage")),
               p(p4, a(href='http://pardus-news.blogspot.co.uk', 'pardus-news.blogspot.co.uk')),
               p(p5)
             )

    return(panel)
}



shinyUI(
  mainPanel(width=12,
    headerPanel('Pardus: Total War in Artemis!'),
    tabsetPanel(
        tabPanel("Instructions", instructionsPanel()),
        tabPanel("Data",  dataPanel()),
        tabPanel("Plots", mainPlotPanel()),
        tabPanel("About", aboutPanel())
    )
  )
)


#
# Ends.
#
