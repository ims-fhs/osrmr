#nearest function
#
#
#
#
#
#
nearest <- function(lat, lng, localhost, solutions)
{
address <- server_address(localhost)

goal <- rjson::fromJSON(file = paste(address, "/nearest/v1/driving/", lng, ",", lat, "?number=",solutions, sep = "", NULL))

nearest <- data.frame(name="a", distance=NA, Lat=NA, Lng=NA,stringsAsFactors = F)

for (i in 1:solutions){
  nearest[i,1] <- data.frame(goal$waypoints[[i]]$name,stringsAsFactors = F)
  nearest[i,2] <- data.frame(goal$waypoints[[i]]$distance)
  nearest[i,3] <- data.frame(goal$waypoints[[i]]$location[2])
  nearest[i,4] <- data.frame(goal$waypoints[[i]]$location[1])
}
return(nearest)
}
