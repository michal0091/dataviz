# :chart_with_upwards_trend: dataviz
Repositorio de GitHub con scripts de visualizaciones de datos en R. Incluye gráficos de barras, líneas, dispersión, anillos, mapas, árboles, burbujas, caja y probabilidad.

:star: Incluye gráficos de barras, líneas, dispersión, anillos, mapas, árboles, burbujas, caja y probabilidad.

:pencil: Los scripts están documentados y fáciles de usar.


## :point_up: Antes de nada...
Para el fastidio de muchos suelo utilizar fuentes personalizadas para los gráficos.
La fuente que suelo usar es la Lato disponible en las fonts de Google:
  * Para descargar: [font Lato](https://fonts.google.com/specimen/Lato)
  * Para instalar basata con descargar la familia, desempaquetar el .zip e instalar una apriendo los ficheros
  * Las variantes que más uso: `"Lato-Regular"` y `"Lato-Black"`


Para instalar las fuentes del pc (windows) en R:

```{r}
# Install 
install.packages("extrafont")
library(extrafont)

# Load fonts
font_import()
loadfonts(device = "win")
```


## :deciduous_tree: Estructura del proyecto

```
📦 scouting
 ┣ 📂 R                                      # R scripts
 ┃  ┗ 📂 2024                                # R scripts of data visualizations in 2024
 ┃     ┗ 📂 week_01                          # R script of visualization of the first week
 ┃        ┣ 📜 2024_w01_lollipop_eolica.R    # R R script thats makes a 🍭 lollipop plot of eolica data
 ┃        ┗ 📄 eolica.csv                    # Input data csv file
 ┣ 📜 .gitignore
 ┣ 📜 README.md
 ┣ 📜 dataviz.Rproj         # R project file

```

## :raised_hands: Contribuciones

:fire: Se aceptan contribuciones al repositorio. Si tiene un script de visualización de datos en R que desea compartir, envíe un pull request.

## :postbox: Contacto
Si tiene alguna pregunta o comentario, envíe un correo electrónico al propietario del repositorio.
  * :mailbox: michal.kinel@gmail.com
  * :octocat: [michal0091](https://github.com/michal0091)

## :copyright: Licencia

:copyright: El repositorio está licenciado bajo la licencia MIT.

