#' ---
#' author: Susann und Daniel Bader
#' title: Hochzeit - Getraenkekonsum
#' output: html_document
#' ---
#' 
#+ echo = F, message = F
library(data.table)
library(plotly)
library(ggplot2)
library(knitr)
library(rmarkdown)
opts_chunk$set(message = F, warning = F, echo = F)

#+ config
# constants
file_drinks <- "~/Documents/getraenke_konsum.csv"
colors_wedding <- c("skyblue", "#4C7EF0")
dir_plot <- "/Users/Bader.Daniel/Google Drive/susidani/2018-06-16_unsere_hochzeit/"

# helper
theme_wedding <- function(){
    theme_bw() +
    theme(
        text = element_text(size = 20),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.major.y = element_line(color = "darkgray"),
        legend.position = "top",
        legend.direction = "horizontal"
        )
}

#' 
#' Daten einlesen
#' 
drinks <- fread(file_drinks)
drinks[, Alkoholisch := factor(Alkoholisch, labels = c("ohne", "mit"))]

#'
#' # Anzahl Bestellungen
#'
#' Kategorien
#' 
drinks[, Anzahl_pro_Kategorie := sum(Anzahl), keyby = Getraenkekategorie]

tab <- unique(drinks[, .(
    Getraenkekategorie = reorder(Getraenkekategorie, Anzahl_pro_Kategorie), 
    Alkoholisch, 
    Anzahl = Anzahl_pro_Kategorie
    )])
p <- ggplot(
    tab, 
    aes(x = Getraenkekategorie, y = Anzahl, fill = Alkoholisch)
) + geom_col() +
    theme_wedding() +
    scale_fill_manual(values = colors_wedding)
#+
p

#' Einzelgetraenke
#' 
tab <- drinks[
    order(Anzahl, decreasing = T), 
    .(Getraenk = reorder(Getraenk, Anzahl), Alkoholisch, Anzahl)
    ]
p <- ggplot(tab[1:15], aes(x = Getraenk, y = Anzahl, fill = Alkoholisch))+
    geom_col() +
    theme_wedding() +
    labs(x = "Top 15 Getraenke") +
    scale_fill_manual(values = colors_wedding)
#+
p
ggsave(filename = file.path(dir_plot, "drinks_top15.png"), plot = p)

#'
#' # Volumen
#'
drinks[, gesamtvolumen := as.double(Anzahl * `Volumen pro Einheit [ml]`)]
drinks[, 
    volumen_pro_kategorie := sum(gesamtvolumen)/1000, 
    keyby = .(Getraenkekategorie)
    ]
drinks[, 
    volumen_pro_alkohol := sum(gesamtvolumen), 
    keyby = .(Alkoholisch)
    ]


#' ## Mehr Alkohl oder Wasser?
#+
tab <- unique(drinks[, .(Alkoholisch, volumen_pro_alkohol)])
p <- plot_ly(data = tab) %>%
    add_pie(
        values = ~volumen_pro_alkohol, text = ~Alkoholisch, 
        insidetextfont = list(color = 'black', size = 22),
        marker = list(
            line = list(color = '#FFFFFF', width = 1), colors = colors_wedding
        )
    ) %>% 
    layout(
        title = "Gesamtvolumen nach Alkoholanteil", 
        titlefont = list(size = 26),
        margin = list(t = 50, r = 5, l = 5, b = 5)
    )
p
#+ 
devnull <- export(p = p, 
    file = file.path(dir_plot, "drinks_alcohol_percent.png"), 
    vwidth = 500, vheight = 400
    )


#' ## Nach Kategorie
#+
tab <- unique(drinks[, .(Alkoholisch, volumen_pro_kategorie, 
    Getraenkekategorie = reorder(Getraenkekategorie, volumen_pro_kategorie))
    ])
#+
plot_ly(
    data = tab, 
    x = ~Alkoholisch, y = ~volumen_pro_kategorie, color = ~Getraenkekategorie
    ) %>%
    add_bars() %>%
    layout(barmode= "stack") 
#+
ggplot(tab, aes(
    x = Getraenkekategorie, y = volumen_pro_kategorie, fill = Alkoholisch
)) +
    theme_wedding() +
    geom_col() + labs(y = "Volumen [Liter]") +
    scale_fill_manual(values = colors_wedding)


#' 
#' # Der durchschnittliche Gast
#' 
#' inklusive Kinder, Schwangere und stillende Muetter
#' 
tab <- unique(drinks[, .(Alkoholisch, volumen_pro_kategorie, 
    Getraenkekategorie = reorder(Getraenkekategorie, volumen_pro_kategorie))
    ])
tab[, volumen_pro_kategorie_pro_gast := volumen_pro_kategorie/41]

#+
ggplot(tab, aes(
    x = Getraenkekategorie, 
    y = volumen_pro_kategorie_pro_gast, 
    fill = Alkoholisch
)) +
    theme_wedding() +
    geom_col() +
    labs(y = "Volumen [Liter]") +
    scale_fill_manual(values = colors_wedding)

#'
#' # Daten-Grundlage
#'
#+
DT::datatable(drinks, rownames = F, filter = "top")

