 ## Load packages beforehand to avoid cluttering the examples with package startup messages
for(x in c("methods", "devtools", "stringr", "knitr", "caret", "e1071",
        "terra", "sf", "randomForest", "pls", "grid", "gridExtra",
        "kernlab", "ggplot2", "RStoolbox")){
    suppressMessages(library(x, character.only = TRUE))
}

knitr::knit_rd(pkg = "RStoolbox")

