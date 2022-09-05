










mapproject(projection = "mercator")














normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}



provenienza_emigrati$inflows_normalized <- normalize(provenienza_emigrati$inflows)














data <- rbind(
  Buenos_aires=c(-58,-34),
  Paris=c(2,49),
  Melbourne=c(145,-38),
  Saint.Petersburg=c(30.32, 59.93),
  Abidjan=c(-4.03, 5.33),
  Montreal=c(-73.57, 45.52),
  Nairobi=c(36.82, -1.29),
  Salvador=c(-38.5, -12.97)
)  %>% as.data.frame()
colnames(data)=c("long","lat")

# Generate all pairs of coordinates
all_pairs <- cbind(t(combn(data$long, 2)), t(combn(data$lat, 2))) %>% as.data.frame()
colnames(all_pairs) <- c("long1","long2","lat1","lat2")

# background map
par(mar=c(0,0,0,0))
map('world',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80) )

# add every connections:











