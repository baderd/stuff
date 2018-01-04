#' ---
#' title: Inflation
#' author: Daniel Bader
#' output: html_notebook
#' ---
#' 

suppressPackageStartupMessages({
    library(data.table)
    library(plotly)
})
#' Load data about inflation in Germany from 
#' [statbureau.org](https://www.statbureau.org/en/germany/inflation-tables)
#' 
dt <- fread('../../data/germany.inflation.monthly-year-over-year.csv')

#' Compute 5 year average with sliding window and step size of 1 year.
#' 
lowy <- min(dt$Year)
rangey <- 4
highy <- lowy + rangey
avgwin <- c()
while(highy <= max(dt$Year)){
    midy <- (lowy + highy)/2
    infl <- mean(dt[lowy <= Year & Year <= highy, Total])
    avgwin <- rbind(avgwin, c(midy, infl))
    lowy <- lowy + 1
    highy <- highy + 1
}

#' Plot interactively
#' 
p <- plot_ly(dt, x=~Year, y=~Total) %>%
    add_markers(name='By year') %>% 
    add_lines(
        x=avgwin[,1], y=avgwin[,2], name='5 year mean'
        ) %>%
    add_lines(
        x=c(min(dt$Year), max(dt$Year)), 
        y=c(1.5, 1.5),
        name='Fixed 1.5%'
        ) %>%
    layout(
        title = 'Inflation in Germany',
        yaxis=list(title='Inflation [%]')
    )
#+ fig.width=10
p

#' Use Mouse to zoom into data!  

#+ END
#'