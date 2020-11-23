#' @title haversine calculates the distance between two lat-lon pairs
#'
#' @description haversine uses the Haversine formula to calculate the
#'     great circle distance between two pairsa of lat-lon values. No
#'     projection is used so this is only useful for points close
#'     together on the Earth's surface. In addition, it uses a NASA
#'     estimate of the volumetric radius of the Earth (6371). The radius
#'     estimate at the equator is 6378.137 and that of the polar radius
#'     is 6356.752. It produces a vector of all three of these values.
#'
#' @param lat1 The first latitude in decimal degrees
#' @param lon1 The first longitude in decimal degrees
#' @param lat2 The second latitude in decimal degrees
#' @param lon2 The second longitude in decimal degrees
#'
#' @author Dr Malcolm Haddon
#'
#' @return a matrix of the polar, volumetric, and equatorial distances in km
#' @export
#'
#' @examples
#' haversine(-43.31, 145.76, -43.19, 145.87) #16.09593, 16.132, 16.15007
haversine <- function(lat1, lon1, lat2, lon2) {
  k <- max(length(lat1), length(lon1), length(lat2), length(lon2))
  lat1 <- rep_len(lat1, length.out = k)
  lat2 <- rep_len(lat2, length.out = k)
  lon1 <- rep_len(lon1, length.out = k)
  lon2 <- rep_len(lon2, length.out = k)
  const <- 57.2958  # = 1/(pi/180)
  Lat1 <- lat1/const
  Lon1 <- lon1/const
  Lat2 <- lat2/const
  Lon2 <- lon2/const
  dlon <- Lon2 - Lon1
  dlat <- Lat2 - Lat1
  # haversine formula
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  r <- 6371 # Radius of earth in kilometers. Use 3956 for miles
  km <- c * r  # equatorial radius = 6378.137 polar radius = 6356.752
  po <- c * 6356.752
  eq <- c * 6378.137
  return (cbind(polar = po, volumetric = km, equatorial = eq))
} # end of haversine

