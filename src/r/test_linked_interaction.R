#' ---
#' author: "Daniel M Bader"
#' title: "test native linked interaction"
#' output: html_document
#' ---
#' 
library(rmarkdown)
library(knitr)
opts_chunk$set(message = F, warning = F, echo = F)
#+
library(plotly)
library(data.table)
library(crosstalk)

#' 
#' # Copy test examples 
#' 
d <- SharedData$new(mtcars)
scatterplot <- plot_ly(d, x = ~mpg, y = ~disp) %>%
    add_markers(color = I("black"))

subplot(
    plot_ly(d, y = ~disp, color = I("black")) %>% 
        add_boxplot(name = "overall"),
    scatterplot, shareY = TRUE
) %>% highlight("plotly_selected")


