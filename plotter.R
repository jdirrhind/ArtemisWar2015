#
# ArtemisWar2015: a Shiny application to show off Pardus War statistics.
# Plotting-related functions.
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



library(ggvis)



#
# A simple mapping from Faction name to its representative colour.
#
FactionColours = c('Empire'     = 'red',
                   'Union'      = 'gold',
                   'Federation' = 'blue',
                   'Unpire'     = 'orange')



#
# Faction -> Colour function.
#
getColour <- function(factionName)
{
    colour <- FactionColours[[factionName]]
    return(colour)
}



#
# A simple text capitalisation function.
#
simpleCap <- function(x)
{
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse="")
}



#
# Combine the two sets of war data during the period the two wars were
# concurrent. Used to answer "2v1" (combined Union & Empire vs Federation)
# type questions.
#
combineStats <- function(uf, fe)
{
    # Find concurrent war dates.
    commonDates  <- intersect(fe$CaptureDate, uf$CaptureDate)
    earliestDate <- min(commonDates)
    latestDate   <- max(commonDates)
    
    combined <- uf[uf$CaptureDate >= earliestDate & uf$CaptureDate <= latestDate, ]
    combined <- rbind(combined, fe[fe$CaptureDate >= earliestDate & fe$CaptureDate <= latestDate, ])

    # Rename the Union and Empire labels to some common value.
    combined[combined$Faction != FEDERATION, 'Faction'] = COMBINED
    combined[, 'SkirmishID'] = 'UFE'

    # Finally, aggregate the data to produce one row per team per date.
    combinedStats <- aggregate(cbind(Points, Kills, Structures, Missions, Sectors) ~ SkirmishID + CaptureDate + Faction, data=combined, sum)
    return (combinedStats)
}



#
# The initial "plotting engine" used for the application.
# Uses the R Base Plotting functions.
# Plots two factions' war data on the same chart for comparison.
# The attribute plotted can be one of points scored for Kills, Structures,
# Missions and Sectors and the grand total figure (Points).
# 
plotAttribute_base <- function(stats,
                               attribute,
                               title,
                               dateRange=NULL,
                               plotOptions=c('SP'),
                               dataSet='RAW',
                               loess_span=0.85)
{
    factionNames <- unique(stats$Faction)
    faction1     <- factionNames[1]
    faction2     <- factionNames[2]

    # Extract each faction's statistics
    f1Stats  <- stats[stats$Faction == faction1, ]
    f2Stats  <- stats[stats$Faction == faction2, ]

    # Use the raw data values or their first derivative?
    if (dataSet == 'DER1')
    {
      diff_f1 <- c(0, diff(f1Stats[, attribute]))
      diff_f2 <- c(0, diff(f2Stats[, attribute]))

      f1Stats[, attribute] <- diff_f1
      f2Stats[, attribute] <- diff_f2
    }

    # Determine axis extent.
    minX   <- min(f1Stats$CaptureDate, f2Stats$CaptureDate)
    maxX   <- max(f1Stats$CaptureDate, f2Stats$CaptureDate)
    minY   <- min(f1Stats[, attribute], f2Stats[, attribute]) - 100
    maxY   <- max(f1Stats[, attribute], f2Stats[, attribute]) + 100

    # Constrain minX and maxX to the supplied date range, if possible.
    if (! is.null(dateRange))
    {
        minX <- max(minX, dateRange[1])
        maxX <- min(maxX, dateRange[2])
    }

    # Arrange x-axis so that there are ten subdivisions.
    xaxis        <- seq(minX, maxX, length.out=10)
    xaxis.labels <- format(xaxis, "%d-%b")

    # Try the following as a line smoother.
    time1.as.n <- as.numeric(f1Stats[, "CaptureDate"])
    time2.as.n <- as.numeric(f2Stats[, "CaptureDate"])
    lw1 <- loess(f1Stats[, attribute] ~ time1.as.n, span=loess_span)
    lw2 <- loess(f2Stats[, attribute] ~ time2.as.n, span=loess_span)

    # Plot Options
    option.scatterplot     <- 'SP'  %in% plotOptions
    option.linegraph       <- 'LG'  %in% plotOptions
    option.smoothlinegraph <- 'SLG' %in% plotOptions

    # No actual plotting takes place here. Just setting up plot style, axes
    # etc.
    plot(x    = f1Stats$CaptureDate,
         y    = f1Stats[, attribute],
         type = 'n',
         main = sprintf("%s (%s)", title, attribute),
         xlab = "Time",
         ylab = simpleCap(attribute),
         xlim = c(minX, maxX),
         ylim = c(minY, maxY),
         xaxt = 'n'
         )
    axis(side=1, at=xaxis, labels=xaxis.labels)

    # Grid lines.
    # Note: the vertical gridlines only align with *default* axis
    # tickmarks. Since we've overridden these, we have to draw our own
    # vertical grid marks using abline().
    grid(NA, NULL)
    abline(v=xaxis, lty=3, col="lightgrey")

    # Legend.
    legend(x='topleft',
           legend = c(faction1, faction2),
           lty    = c(1, 2),
           pch    = c(1, 2),
           col    = sapply(c(faction1, faction2), getColour)
          )

    #
    # Data Plotting starts here.
    #

    # Option: Scatter Plot
    if (option.scatterplot)
    {
        points(x   = f1Stats$CaptureDate,
               y   = f1Stats[, attribute],
               pch = 1,
               col = getColour(faction1) )
        points(x   = f2Stats$CaptureDate,
               y   = f2Stats[, attribute],
               pch = 2,
               col = getColour(faction2) )
    }

    # Option: (simple) Line Graph
    if (option.linegraph)
    {
        lines(x   = f1Stats$CaptureDate,
              y   = f1Stats[, attribute],
              lty = 1,
              col = getColour(faction1) )
        lines(x   = f2Stats$CaptureDate,
              y   = f2Stats[, attribute],
              lty = 2,
              col = getColour(faction2) )
    }

    # Option: Smooth Line Graph
    if (option.smoothlinegraph)
    {
        lines(x   = f1Stats$CaptureDate,
              y   = lw1$fitted,
              lty = 1,
              lwd = 2,
              col = getColour(faction1))
        lines(x   = f2Stats$CaptureDate,
              y   = lw2$fitted,
              lty = 2,
              lwd = 2,
              col = getColour(faction2))
    }
}



#
# The second "plotting engine" used for the application.
# Uses the ggvis library (note: this is different to 'GoogleVis'.
# This library was quite tricky to use so don't be surprised at the
# heavy-handed conversion from the base graphics version) because
# I wanted to have a go at adding tooltips on each data point.
# This did not yield very satisfying results but it's done now so
# may as well keep it.
# 
plotAttribute_ggvis <- function(stats,
                                attribute,
                                title,
                                dateRange   = NULL,
                                plotOptions = c('SP'),
                                dataSet     = 'RAW',
                                loess_span  = 0.85)
{
    #
    # Tooltip hover function.
    # Hover the mouse over a plotted point for extra information.
    #
    all_values <- function(x) 
    {
        # Return immediately if hovering over non-scatterplot data.
        # Unfortunately, can't filter out straight-line graph hovers, resulting in "Points: 0"
        # being displayed to the user. Frustrating!
        if (is.null(x)) return(NULL)
        if ('pred_' %in% names(x)) return(NULL)
        if ('resp_' %in% names(x)) return(NULL)

        # We have to fetch the date from the source data frame, since the CaptureDate 
        # value passed in via x turns out to be a nonsensical number.
        y <- list()
        y[['Faction']] <- x[['Faction']]
        y[['Points']]  <- format(x[[attribute]], big.mark=',')
        # y[['Date']]    <- as.character(stats[stats$Id == x$Id, 'CaptureDate'])
        # It later turned out that using 'key' in layer_points() to associate the dataframe
        # row Id with the parameter 'x', above, may cause plot points and plot lines to 
        # mysteriously vanish in front of your eyes after a few seconds!
        # Had to disable that and consequently, cannot display the date after all.
        # What a waste of effort to get this working!

        paste0(names(y), ": ", y, collapse = "<br />")
    }

    # Plot Options
    option.scatterplot     <- 'SP'  %in% plotOptions
    option.linegraph       <- 'LG'  %in% plotOptions
    option.smoothlinegraph <- 'SLG' %in% plotOptions

    # Determine faction names.
    factionNames <- unique(stats$Faction)
    faction1     <- factionNames[1]
    faction2     <- factionNames[2]

    # Determine the faction colours for this plot.
    factionColours <- unname(sapply(c(faction1, faction2), getColour))

    # Extract each faction's statistics
    f1Stats  <- stats[stats$Faction == faction1, ]
    f2Stats  <- stats[stats$Faction == faction2, ]

    # Use the raw data values or their first derivative?
    if (dataSet == 'DER1')
    {
      diff_f1 <- c(0, diff(f1Stats[, attribute]))
      diff_f2 <- c(0, diff(f2Stats[, attribute]))

      f1Stats[, attribute] <- diff_f1
      f2Stats[, attribute] <- diff_f2
    }

    # Determine axis extent.
    minX <- min(f1Stats$CaptureDate, f2Stats$CaptureDate)
    maxX <- max(f1Stats$CaptureDate, f2Stats$CaptureDate)
    minY <- min(f1Stats[, attribute], f2Stats[, attribute]) - 100
    maxY <- max(f1Stats[, attribute], f2Stats[, attribute]) + 100

    # Constrain minX and maxX to the supplied date range, if possible.
    if (! is.null(dateRange))
    {
        minX <- max(minX, dateRange[1])
        maxX <- min(maxX, dateRange[2])
    }
    
    # 
    f_all <- rbind(f1Stats[f1Stats$CaptureDate >= minX & f1Stats$CaptureDate <= maxX, ],
                   f2Stats[f2Stats$CaptureDate >= minX & f2Stats$CaptureDate <= maxX, ])
    
    #
    # Construct the ggvis graph object.
    #
    p <- f_all %>% ggvis(x          = ~CaptureDate, 
                         y          = prop("y", as.name(attribute)),
                         stroke     = ~Faction, 
                         strokeDash = ~Faction)
    p <- p  %>% group_by(Faction)

    # Option: (simple) Line Graph
    if (option.linegraph)
    {
      p <- p  %>% layer_paths() 
    }
    
    # Option: Smooth Line Graph
    if (option.smoothlinegraph)
    {
      p <- p  %>% layer_smooths(span=loess_span)
    }

    # Option: Scatter Plot
    if (option.scatterplot)
    {
       # It seems that including 'key', below, causes plot points and lines to disappear
       # after a brief moment:
       # https://github.com/rstudio/ggvis/issues/330
       # https://groups.google.com/forum/?_escaped_fragment_=topic/ggvis/yV206NJsVWs#!topic/ggvis/yV206NJsVWs
       # This means that we have to forget about displaying the CaptureDate info in the tooltip.
       # which was the reason 'key' was introduced in the first place.
       #p <- p  %>% layer_points(size=1, fill=~Faction, shape=~Faction, key := ~Id) 
       p <- p  %>% layer_points(size=1, fill=~Faction, shape=~Faction) 
    }
    
    p <- p  %>% scale_nominal("fill",       range = factionColours)
    p <- p  %>% scale_nominal("stroke",     range = factionColours)
    p <- p  %>% scale_nominal("shape",      range = c('circle', 'triangle-up'))
    p <- p  %>% scale_nominal("strokeDash", range = c(1,  6))
    p <- p  %>% add_axis("x", title="Time", subdivide=6, ticks=10, orient="bottom", format="%d-%b")
    p <- p  %>% add_axis("y", title=attribute, title_offset=70, subdivide=1, ticks=10, orient="left", format=",d")
    p <- p  %>% hide_legend(c("fill", "stroke", "shape"))
    p <- p  %>% add_legend(c("fill"), orient='left')
    p <- p  %>% set_options(width = 600, height = 'auto')
    p <- p  %>% add_tooltip(all_values, "hover")

    return(p)
}



#
# Ends.
#
