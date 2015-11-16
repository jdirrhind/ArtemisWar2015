#
# ArtemisWar2015: a Shiny application to show off Pardus War statistics.
# Back End component.
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
options(scipen=10) # To ensure the y-axis is not labelled using scientific notation.
source("plotter.R")

#
# Populate Data Structures shared across all sessions.
#
load("UnionFederationWar.Rda")  # Dataframe 'uf'
load("FederationEmpireWar.Rda") # Dataframe 'fe'
ufe <- combineStats(uf, fe)     # Combined (Union & Empire) vs The Federation stats.

#
# Add primary key fields to each of these data frames.
# This is a hack that is necessary in order to allow ggvis tooltips to access the
# source data frame row. It came about after receiving corrupted values for
# CaptureDate in the ggvis tooltip function. It later proved entirely fruitless owing to
# buggy behaviour in ggvis (plotted points vanishing seconds after being renedered).
#
uf$Id  <- 1:nrow(uf)
fe$Id  <- 1:nrow(fe)
ufe$Id <- 1:nrow(ufe)



#
# Wrapper function for plot generation.
# Pick the underlying plotting engine here.
#
doPlot <- function(plotlib, skirmishID, attribute, plotOptions, dateRange, dataSet, loess_span)
{
    # Plotting Engine selection.
    plotIt <- plotAttribute_ggvis
    if (plotlib == BASE_GRAPHICS)
    {
        plotIt <- plotAttribute_base
    }
    
    # Select the war data of interest.
    if (skirmishID == 'UF')
    {
      df <- uf
    }
    else if (skirmishID == 'FE')
    {
      df <- fe
    }
    else if (skirmishID == 'UFE')
    {
      df <- ufe
    }

    plotIt(stats       = df,
           attribute   = attribute,
           title       = SkirmishIds[[skirmishID]],
           dateRange   = dateRange,
           plotOptions = plotOptions,
           dataSet     = dataSet,
           loess_span  = loess_span)
}



#
# The Shiny server entry point.
#
shinyServer(
  function(input, output) {
    output$tableUF     <- renderDataTable(uf)
    output$tableFE     <- renderDataTable(fe)
    output$tableUFE    <- renderDataTable(ufe)
    
    # Note the ugly, hacky implementation here:
    # It was infuriatingly difficult to get the ggvis and base plots reacting
    # to a change in input$plotlibId selection, even when explicitly using
    # reactive().  Decided to go for the following less than obvious approach
    # to achieve the goal: renderPlot works as normal when using the base
    # library but if ggvis is chosen by the user, give NULL to renderPlot
    # (which the UI swallows gracefully) and allow the ggvis binding magic to
    # handl the plotting itself (in a separate front-end panel). Not
    # impressed.
    output$plotOutput <- renderPlot(
    {
        chosen_plotlib <- as.character(input$plotlibId)
        returnValue    <- NULL
        
        if (chosen_plotlib == BASE_GRAPHICS)
        {
            returnValue <- doPlot(chosen_plotlib, input$skirmishId, input$analysisId, input$plotOptions, input$dateRange, input$dataSet, input$loessSpan)
        }
        else
        {
            p <- reactive({ doPlot(chosen_plotlib, input$skirmishId, input$analysisId, input$plotOptions, input$dateRange, input$dataSet, input$loessSpan) })
            p %>% bind_shiny('ggvis', 'ggvis_ui')
            returnValue <- NULL  # GGVIS will handle rendering using the bindings above.
        }

        return(returnValue)
    })
  }
)



#
# Ends.
#
