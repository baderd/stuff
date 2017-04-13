#'---
#'title: "World map with multiple traces"
#'output: html_document
#'author: baderd
#'---



#+ message=F, warning=F
library(plotly)

#+
df = data.frame(
    country=rep(c('France', 'Germany', 'China'),2),
    year=c(rep(2012,3), rep(2013,3)),
    number_people=c(1,2,3,6,5,4)
)
df

#' # Chloropleth
#' 
#' Add one trace of counts by country per year. Second trace is not present. 
#' However, the colorbar shows two titles.
#' 
#+ warning=F
p <- plot_ly(
    subset(df, year==2012), type='choropleth',
    locationmode='country names', 
    locations=~country, 
    z=~number_people,
    showlegend=TRUE
) %>% add_trace(
    data=subset(df, year==2013), visible=F
) %>% layout(
    geo= list(scope='world'),
    updatemenus = list(list(
        buttons = list(
            list(method = "update",
                args = list("visible", list(TRUE, FALSE)),
                label = "2012"
            ),
            list(method = "update",
                args = list("visible", list(FALSE, TRUE)),
                label = "2013"
            )
        )
    ))
)
p

    
#' # Barplot
#' 
#' Works as expected. Shows legend at beginning for all, 
#' legend is gone upon selection.
#'
#+ warning=F
p <- plot_ly(
    subset(df, year==2012), 
    type='bar',
    x=~country, 
    y=~number_people
) %>% add_bars(
    data=subset(df, year==2013), 
    visible=F
) %>% layout(
    updatemenus = list(list(
        buttons = list(
            list(
                args = list("visible", list(TRUE, FALSE)),
                label = "2012"
            ),
            list(
                args = list("visible", list(FALSE, TRUE)),
                label = "2013"
            )
        )
    ))
)
p




