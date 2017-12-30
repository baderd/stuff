#' ---
#' title: Inflation
#' ---
#' 

library(data.table)
library(plotly)

dt <- fread('data/germany.inflation.monthly-year-over-year.csv')

lowy <- min(dt$Year)
rangey <- 5
highy <- lowy + rangey
avgwin <- c()
while(highy <= max(dt$Year)){
    midy <- mean(lowy, highy)
    infl <- mean(dt[lowy <= Year & Year <= highy, Total])
    avgwin <- rbind(avgwin, c(midy, infl))
    lowy <- lowy + 1
    highy <- highy + 1
}

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
p
    

#+ END
#'