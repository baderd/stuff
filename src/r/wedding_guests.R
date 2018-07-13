#'---
#' title: Hochzeit Susi und Daniel - Gaeste-Infos
#' output: html_document
#'---
#'
#+ warning=F, message=F, echo=F
library(data.table)
library(plotly)
library(googlesheets)
library(knitr)
opts_chunk$set(warning=F, message=F, cache=F, echo=F)


#+ load google sheet, results='hide'
gs_ls(regex = 'Gaesteliste')[, c('sheet_title', 'sheet_key', 'author')]
guest_wb <- gs_key("some_magic_key")
guest <- as.data.table(gs_read(guest_wb, ws='sheet1'))

# file_guestlist <- file.path(
#     "/Users/Bader.Daniel/Google Drive/susidani/unsere_hochzeit/Gaesteliste_tsv.tsv"
#     )
# guest <- fread(file_guestlist)


# Select invited guests
# 
#+ select guest range
guest <- guest[Rang>0 & Rang<4 
    & (is.na(Persons_confirmed) | Persons_confirmed==1)
    ]


#' # Wie viele Gaeste sind eingeladen?
#' 
df_size <- data.table(
    group_name = c(
        'fc_bayern_kader', 'american_football_team', 
        'hochzeit_william_kate', 'hochzeit_susi_daniel'
        ),
    group_size = c(31, 50, 1900, nrow(guest))
)
df_size[, group_name:= reorder(group_name, group_size)]

#+
ggp <- ggplot(
    df_size[!grepl('hochzeit_william_kate', group_name)], 
    aes(x=group_name, y=group_size)
) + geom_col()
ggp

#' Andere Hochzeiten aus 2018 zum Vergleich...
#+
ggp <- ggplot(df_size, aes(x=group_name, y=group_size)) + geom_col()
ggp


