osrmr:::server_address(TRUE)

Sys.setenv("OSRM_PATH"="C:/OSRM_API5")
osrmr::run_server("switzerland-latest.osrm")

address <- "http://localhost:5000"

lng1 <- 9.382589
lat1 <- 47.428731
lng2 <- 9.240543
lat2 <- 47.49642
lng3 <- 9.252642
lat3 <- 47.412704

n <- 3


fastest <- rjson::fromJSON(file = paste(address, "/table/v1/driving/", lng1, ",", lat1, ";", lng2, ",", lat2, ";", lng3, ",", lat3, sep = "", NULL))


df.1 <- data.frame("Startpunkt", "Endpunkt", fastest$durations[[i]], stringsAsFactors = FALSE)


for(i in 1:n) {

df.1[i,1] <- fastest$sources[[1]]$name
df.1[i,2] <- fastest$destinations[[i]]$name

colnames(df.1) <- c("Startpunkt", "Endpunkt", "Durations")

}
df.1




df.2 <- data.frame("Startpunkt", "Endpunkt", fastest$durations[[2]], stringsAsFactors = FALSE)

for(i in 1:n) {

df.2[i,1] <- fastest$sources[[2]]$name
df.2[i,2] <- fastest$destinations[[i]]$name

colnames(df.2) <- c("Startpunkt", "Endpunkt", "Durations")
}
df.2


df.3 <- data.frame("Startpunkt", "Endpunkt", fastest$durations[[3]], stringsAsFactors = FALSE)


for(i in 1:n) {

df.3[i,1] <- fastest$sources[[3]]$name
df.3[i,2] <- fastest$destinations[[i]]$name

colnames(df.3) <- c("Startpunkt", "Endpunkt", "Durations")
}
df.3



m1 <- merge(df.1,df.2, all=TRUE, sort=FALSE)

m2 <- merge(m1,df.3, all=TRUE, sort=FALSE)
m2

