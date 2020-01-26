#             Data Visualization 2: Crimes in Denmark
#-------------------------------------------------------------------
#                             Big Data

# Group: Álvaro Arranz, Beatrice Olivari, Daniel Saiz

# - Prerequisites:
# Install necesary packages if not currently installed.

if (!require("shiny"))
  install.packages("shiny")
if (!require("dplyr"))
  install.packages("dplyr")
if (!require("devtools"))
  install.packages("devtools")
if (!require("usethis"))
  install.packages("usethis")

# enable reactlog recording
options(shiny.reactlog = TRUE)

library(shiny)

runApp("app")
