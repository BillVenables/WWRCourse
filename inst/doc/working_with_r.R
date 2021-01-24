## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "")
library(WWRGraphics)

## ---- fig.show='hold'---------------------------------------------------------
lazyData::requireData(doParallel)
par(mar=c(1,1,2,1))
pals <- paste0("pal_", cq(desert, green2brown, blue2red, sea2sand))
foreach(pal = pals, let = letters[1:4]) %do% {
  showColors(get(pal)(100), main = pal)
  text("top right", paste0("(", let, ")"), cex = 1.25, font = 2)
} %>% invisible()

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(head(mtcars, 10))

