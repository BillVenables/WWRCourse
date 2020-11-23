
#' cc
#'
#'
#' @format A data frame with 2000 rows and 2 columns:
#' \describe{
#'     \item{Longitude}{numeric: ... }
#'     \item{Latitude}{numeric: ... }
#' }
"cc"

#' data4
#'
#'
#' @format A data frame with 4 rows and 7 columns:
#' \describe{
#'     \item{Longitude}{numeric: ... }
#'     \item{Latitude}{numeric: ... }
#'     \item{Place}{factor: ... }
#'     \item{Sea}{numeric: ... }
#'     \item{Coast}{numeric: ... }
#'     \item{Depth}{array: ... }
#'     \item{Mud}{array: ... }
#' }
"data4"

#' FSemiData4
#'
#'
#' @format A data frame with 1460 rows and 9 columns:
#' \describe{
#'     \item{Longitude}{numeric: ... }
#'     \item{Latitude}{numeric: ... }
#'     \item{Place}{factor: ... }
#'     \item{Sea}{numeric: ... }
#'     \item{Coast}{numeric: ... }
#'     \item{Depth}{array: ... }
#'     \item{Mud}{array: ... }
#'     \item{DayOfYear}{integer: ... }
#'     \item{Fsemi}{array: ... }
#' }
"FSemiData4"

#' GladstoneBream
#'
#'
#' @format A data frame with 337 rows and 23 columns:
#' \describe{
#'     \item{Trip}{character: ... }
#'     \item{ZoneNo}{integer: ... }
#'     \item{Zone}{factor: ... }
#'     \item{SiteId}{integer: ... }
#'     \item{SiteF}{factor: ... }
#'     \item{Site}{factor: ... }
#'     \item{Date}{Date: ... }
#'     \item{Year}{factor: ... }
#'     \item{Month}{factor: ... }
#'     \item{Tidal}{factor: ... }
#'     \item{Depth}{numeric: ... }
#'     \item{Sand}{numeric: ... }
#'     \item{Mud}{numeric: ... }
#'     \item{Gravel}{numeric: ... }
#'     \item{Rock}{numeric: ... }
#'     \item{Long}{numeric: ... }
#'     \item{Lat}{numeric: ... }
#'     \item{Casts}{numeric: ... }
#'     \item{Pikey}{integer: ... }
#'     \item{Yellowfin}{integer: ... }
#'     \item{Bream}{integer: ... }
#'     \item{P1}{numeric: ... }
#'     \item{P2}{numeric: ... }
#' }
"GladstoneBream"

#' Headrope
#'
#'
#' @format A data frame with 8594 rows and 13 columns:
#' \describe{
#'     \item{YearF}{factor: ... }
#'     \item{Y2K}{integer: ... }
#'     \item{Stock}{factor: ... }
#'     \item{Vessel}{factor: ... }
#'     \item{Days}{integer: ... }
#'     \item{Head}{numeric: ... }
#'     \item{Hull}{integer: ... }
#'     \item{Power}{numeric: ... }
#'     \item{Catch}{numeric: ... }
#'     \item{Banana}{numeric: ... }
#'     \item{Tiger}{numeric: ... }
#'     \item{Endeavour}{numeric: ... }
#'     \item{King}{numeric: ... }
#' }
"Headrope"

#' LB2004
#'
#'
#' @format A data frame with 11779 rows and 14 columns:
#' \describe{
#'     \item{Date}{Date: ... }
#'     \item{Year}{integer: ... }
#'     \item{Month}{factor: ... }
#'     \item{Week}{factor: ... }
#'     \item{Vcode}{factor: ... }
#'     \item{Latitude}{numeric: ... }
#'     \item{Longitude}{numeric: ... }
#'     \item{StockTig}{factor: ... }
#'     \item{StockEnde}{factor: ... }
#'     \item{StockEnsi}{factor: ... }
#'     \item{StockMerg}{factor: ... }
#'     \item{StockIndi}{factor: ... }
#'     \item{StockKing}{factor: ... }
#'     \item{Season}{factor: ... }
#' }
"LB2004"

#' Locz
#'
#'
#' @format A data frame with  rows and  columns:
#' \describe{
#'     \item{}{complex: ... }
#'     \item{}{complex: ... }
#'     \item{}{complex: ... }
#'     \item{}{complex: ... }
#' }
"Locz"

#' NPFBigCoastline
#'
#'
#' @format A data frame with 210736 rows and 4 columns:
#' \describe{
#'     \item{x}{numeric: ... }
#'     \item{y}{numeric: ... }
#'     \item{Longitude}{numeric: ... }
#'     \item{Latitude}{numeric: ... }
#' }
#' @examples
#' if("WWRGraphics" %in% loadedNamespaces()) {
#' plot(Latitude ~ Longitude, NPFBigCoastline,
#'      asp = 1, type = "l", col = alpha("dark green", 0.5))
#' points(Latitude ~ Longitude, roundTrip)
#' text(Latitude ~ Longitude, roundTrip, Locality,
#'      pos = avoid(Longitude, Latitude), xpd=NA)
#' } else cat("Attach WWRGraphics package first\n")
"NPFBigCoastline"

#' ShortCC
#'
#' Coordinates defining a curve through the main
#' fishing areas of the NPF
#'
#' @format A data frame with 2001 rows and 2 columns:
#' \describe{
#'     \item{x}{numeric: ... }
#'     \item{y}{numeric: ... }
#' }
"ShortCC"

#' teff
#'
#'
#' @format A data frame with 7029 rows and 3 columns:
#' \describe{
#'     \item{Longitude}{numeric: ... }
#'     \item{Latitude}{numeric: ... }
#'     \item{TigerE}{numeric: ... }
#' }
"teff"

#' Tigers
#'
#'
#' @format A data frame with 12569 rows and 13 columns:
#' \describe{
#'     \item{Survey}{factor: ... }
#'     \item{Date}{factor: ... }
#'     \item{Longitude}{numeric: ... }
#'     \item{Latitude}{numeric: ... }
#'     \item{Pesc}{numeric: ... }
#'     \item{Psem}{numeric: ... }
#'     \item{Total}{numeric: ... }
#'     \item{Sea}{numeric: ... }
#'     \item{Coast}{numeric: ... }
#'     \item{Depth}{numeric: ... }
#'     \item{Mud}{numeric: ... }
#'     \item{DayOfYear}{integer: ... }
#'     \item{ElapsedDays}{integer: ... }
#' }
"Tigers"
