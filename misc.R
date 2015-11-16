#
# ArtemisWar2015: a Shiny application to show off Pardus War statistics.
# Miscellaneous items.
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



# 
# Symbolic Constants
#
FEDERATION <- 'Federation'
EMPIRE     <- 'Empire'
COMBINED   <- 'Unpire'

# Symbols identifying R's base graphics and the ggvis libraries.
BASE_GRAPHICS  <- 'BASE'
GGVIS_GRAPHICS <- 'GGVIS'



#
# Key-Value pair lists. Used for UI widgets.
# ------------------------------------------
PlottingLibraryIds <- c('BASE'         = 'Base Graphics Library',
                        'GGVIS'        = 'GGVIS Graphics Library (Experimental)')

SkirmishIds        <- c('UF'           = 'Federation vs Union',
                        'FE'           = 'Federation vs Empire',
                        'UFE'          = 'Federation vs The Rest')

AnalysisIds        <- c('Points'       = 'Total Points',
                        'Kills'        = 'Kills',
                        'Structures'   = 'Structures Razed',
                        'Missions'     = 'Missions Completed',
                        'Sectors'      = 'Sector Captures')

PlotOptionIds      <- c('SP'           = 'Scatter Plot',
                        'LG'           = 'Line Graph',
                        'SLG'          = 'Smoothed Line Graph')

dataSetIds         <- c('RAW'          = 'Cumulative Data',
                        'DER1'         = 'Daily Differences')



#
# Functions
#

#
# Given a named list, flip the structure such that the values
# now become the names and vice-versa.
#
flipStructure <- function(list)
{
    return( setNames(names(list), unname(list)) )
}



#
# Ends.
#
