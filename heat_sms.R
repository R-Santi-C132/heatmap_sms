#_______________________________________________________________________________

#-------------------------- HOURLY TEMPERATURE HEAT MAP ------------------------

# ---------------------    SETTING THE WORKING DIRECTORY   ---------------------
getwd()
setwd("C:/file path/temperature")

# -------------------------     LOADING PACKAGES    ----------------------------
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "scales", "fontawesome", "ggtext", "showtext", 
              "colorspace", "seecolor", "patchwork","ggplot2", "ggtext", 
              "paletteer", "here", "sysfonts")
ipak(packages)


# tidyverse: For data manipulation, visualization, and analysis.
# scales: Nice Scales for ggplot2
# fontawesome: Icons display in ggplot2
# ggtext: Markdown text support for ggplot2
# showtext: Display fonts in ggplot2
# colorspace: Lighten and Darken colors
# seecolor: To print and view colors
# patchwork: Combining plots
# ggplot2: Advanced and customizable visualizations.
# ggtext: For enhanced text rendering in ggplot2(e.g., supporting HTML or Markdown).
# paletteer: To access a wide range of color palettes for visualizations.
# here: For managing relative file paths
# sysfonts: To manage and add system fonts for use in plots.

# ------------------------ DOWNLOAD AND LOADING DATA ---------------------------
# Get data in .csv from https://open-meteo.com

rawdf <- read_csv(#Reads a CSV file into an object.
  here::here("sms.csv"),#Constructs the full path to the file <<<sms.csv>>>
  skip = 2#Skips the first 2 lines of the CSV file when reading it.
)
# ------------------------- DATA WRANGLING AND EDA -----------------------------

df <- rawdf |> 
  janitor::clean_names() |> #Cleans column names, converting them to snake_case and making them consistent.
  mutate(#Adds or modifies columns in the dataset.
    time_local = with_tz(time, "America/Mexico_City"),#Converts the time column to the "America/Mexico_City" time zone.
    year = year(time_local),#Extracts the year
    month = month(time_local, label = TRUE),#Extracts the month and as a labeled factor ("Jan", "Feb")
    hour = hour(time_local),#Extracts the hour
    day = day(time_local),#Extracts the day of the month
    .keep = "unused" #Keeps only the newly created columns and drops unused ones.
  )

# ------------------------- VISUALIZATION PARAMETERS ---------------------------


# Font for titles
font_add_google("Oswald",
                family = "title_font"
) 

# Font for the caption
font_add_google("Barlow Condensed",
                family = "caption_font"
) 

# Font for plot text
font_add_google("Kanit",
                family = "body_font"
) 

showtext_auto()

bg_col <- "white" #background color

text_col <- "grey15" #consistent text color
text_hil <- "grey15" #highlight text color

bts <- 90 #Defines a variable for the number of time blocks. 

# Caption stuff for the plot

sysfonts::font_add(
  family = "Font Awesome Brands",#descargar manualmente
  regular = here::here("docs", "fa-brands-400.ttf")
)


#Asigna códigos HTML Unicode para los íconos de GitHub y Twitter.
#Además, define los nombres de usuario para mostrar en las leyendas.
github <- "&#xf09b"
github_username <- "R-Santi-C132"
xtwitter <- "&#xe61b"
xtwitter_username <- "@xxxxx_xxxxx"

#Glue:Ensambla cadenas de texto combinando texto y variables.
social_caption_1 <- glue::glue("<span style='font-family:\"Font Awesome Brands\";'>{github};</span> <span style='color: {text_hil}'>{github_username}  </span>")

social_caption_2 <- glue::glue("<span style='font-family:\"Font Awesome Brands\";'>{xtwitter};</span> <span style='color: {text_hil}'>{xtwitter_username}</span>")
#<span style>: Usa HTML para formatear el texto. Aquí se especifica la fuente y el color.
# El texto final incluye el ícono de la red social y el nombre de usuario resaltado.

plot_title <- "San Miguel Suchixtepec:\nMapa de calor de la temperatura por hora (1998-2023)" 
#n\ permite hace un salto de línea

plot_caption <- paste0(#paste0: Une varias cadenas de texto en una sola.
  "**Data:** open-meteo.com", 
  " |  **Code:** ", 
  social_caption_1, 
  " |  **Gráfico:** ", 
  social_caption_2
)

#Elimina variables temporales para liberar memoria y evitar conflictos posteriores.
rm(github, github_username, xtwitter, 
   xtwitter_username, social_caption_1, 
   social_caption_2)

# ----------------------------    MAKING THE GRAPH -----------------------------

g <- df |> 
  ggplot(
    mapping = aes(
      x = day,
      y = hour,
      fill = temperature_2m_c #Los colores del gráfico representan la T°en grados Celsius.
    )
  ) +
  geom_tile( #Dibuja rectángulos (celdas)para representar valores
    linewidth = 0.01,#grosor de bordes de celdas
    colour = bg_col #usa el color de fondo definido prev como bg_col="white"
  ) +
  
  # Scales and Coordinates
  scale_x_continuous(
    breaks = c(0, 10, 20, 30),#etiquetas cada 10 días
    expand = expansion(0)#elimina espacio extra alrededor de los ejes
  ) +
  scale_y_continuous(
    expand = expansion(0),
    breaks = seq(0, 24, 6)#etiquetas c/6 h
  ) +
  paletteer::scale_fill_paletteer_c(#usa una escala de colores predefinida ("Temp")
    "grDevices::Temps",#para representar la temperatura
    # "ggthemes::Red-Green-Gold Diverging",
    direction = 1,#la escala va de frio a caliente
    breaks = seq(0, 50, 10)#intervalos de temperatura en múltiplos de 10
  ) +
  facet_grid(#divide el gráfico en paneles por año (fila)y mes(columna)
    year ~ month,
    scales = "free_x"#permite que el eje X se adapte a cada panel
  ) +
  coord_cartesian(
    clip = "off"#permite que los elementos fuera de los límites del gráfico sigan siendo visibles
  ) +
  
  labs(
    x = "Día del mes (1 - 31)",
    y = "Hora del día (24 hrs)",
    fill = "Temperatura (en grados Celcius)",
    title = plot_title,
    subtitle = NULL,
    caption = plot_caption
  ) +
  theme_minimal(
    base_family = "title_font",#define la fuente para el texto principal
    base_size = bts #tamaño base de los textos
  ) +
  theme(
    # Overall Plot
    panel.grid = element_blank(),#elimina las líneas de la cuadrícula
    legend.position = "bottom",
    text = element_text(
      margin = margin(0,0,0,0, "mm"),
      colour = text_col,
      lineheight = 0.3
    ),
    plot.title.position = "plot",
    plot.margin = margin(5,10,5,5, "mm"),
    
    # Strips and Axes
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "mm"),
    axis.text.x = element_text(
      margin = margin(1,0,0,0, "mm"),
      size = 0.3 * bts
    ),
    axis.text.y = element_text(
      margin = margin(0,1,0,0, "mm"),
      size = 0.2 * bts
    ),
    axis.title = element_text(
      margin = margin(-2,0,0,0, "mm")
    ),
    panel.spacing = unit(0.9, "mm"),
    strip.text.x = element_text(
      margin = margin(0,0,3,0, "mm"),
      size = 1.2 * bts
    ),
    strip.text.y = element_text(
      margin = margin(0,0,0,3, "mm"),
      angle = 0,
      size = 0.8 * bts
    ),
    
    # Labels
    plot.title = element_text(
      family = "title_font",
      margin = margin(10,0,5,0, "mm"),
      size = 1.75 * bts,
      hjust = 0.5,
      #lineheight = 1.2
    ),
    plot.subtitle = element_text(
      margin = margin(0,0,5,0, "mm"),
      hjust = 0.5
    ),
    plot.caption = element_textbox(
      family = "caption_font",
      hjust = 0.5,
      margin = margin(-10,0,0,0, "mm"),
      size = 0.6 * bts)
    ,
    
    # Legend
    legend.title.position = "top",
    legend.key.height = unit(5, "mm"),
    legend.key.width = unit(70, "mm"),
    legend.title = element_text(
      margin = margin(-30,0,5,0, "mm"),
      hjust = 0.5
    ),
    legend.text = element_text(
      margin = margin(2,0,0,0, "mm")
    )
  )

g #graph name

# _____________________________ SAVING THE GRAPH _______________________________

ggsave(
  filename ="heatmap_sms.png",
  plot = g,
  width = 210 * 2,    # Best Twitter Aspect Ratio = 5:4
  #Sets the plot width to 420 millimeters (double the width of A4 paper)
  height = 297 * 2,
  #Sets the plot height to 594 millimeters (double the height of A4 paper).
  units = "mm",
  bg = "white"
)









