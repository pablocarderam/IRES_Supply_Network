#Getting Coordinates for Network
#Run all of this first
setwd("~/Desktop/fall_2014_work/Spring_2014/DirectedGraphs")
library(ggmap)
randomRows = function(df,n){return(df[sample(nrow(df),n),])}

#Subsetting data from all data to complete non-repeated cases.
data <- read.csv("snakebitehosp.csv")
mygeo <- read.csv("GeoComplete.csv", header=TRUE)
mygeocode <- cbind(mygeo, data[c(-4104,-4077),]) #Two cases gave errors from geocode()
colnames(mygeocode)[2] <- "lon"
mygeo.sub <- mygeocode[complete.cases(mygeocode[,1:2]),]
mygeo.sub2 <- mygeo.sub[!duplicated(mygeo.sub[,1:2]),]
mygeo.sub2[c(6,7,8,10,16:19)] <- list(NULL)
#Setting for mapdist function
#rm("mygeo.sub","mygeo.sub2","mygeo","data") #Clean Up before Loop
#Finding the distance between all combinations of hospitals
num.of.hosp <- 50
mygeo.sub3 <- randomRows(mygeo.sub2, num.of.hosp)
my.comb.index <- combn(num.of.hosp,2)
mydist.mat <- data.frame()
i <- 1
for(i in 1:5){
i1 <- as.numeric(my.comb.index[1,i])
i2 <- as.numeric(my.comb.index[2,i])
my.from <- as.character(mygeo.sub3[i1,7])
my.to   <- as.character(mygeo.sub3[i2,7])
my.hours <- mapdist(from=as.numeric(mygeo.sub3[i1,c(2,1)]),
        to=as.numeric(mygeo.sub3[i2,c(2,1)]),
        mode='driving')$hours
temp.mat <- data.frame(from = my.from, to = my.to, hours = my.hours)
mydist.mat <- rbind(mydist.mat, temp.mat)
}

#Implicitly assigning area_code to hospitals
#############################################################
# healthCenters #a list of all health centers
# numRows=100;
# numCol=numRows;
# startLat=1; 
# startLon=1; 
# for each healthCenter:
#   while healthCenter[i] < lat/lon
#       lat[i+1]
#############################################################

######################################################
# Geocoding script for large list of addresses.
# Shane Lynn 10/10/2013
# get the address list, and append "Ireland" to the end to increase accuracy 
# (change or remove this if your address already include a country etc.)
addresses = data$nombre
addresses = paste0(addresses, ", Colombia")

#define a function that will process googles server responses for us.
getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 4105
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0(infile, '_temp_geocoded.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)
  print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(addresses[ii])
  print(result$status)
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  #saveRDS(geocoded, tempfilename)
}


mapdist(from=as.numeric(geocode('the white house')),
        to= as.numeric(geocode('the white house')),
        mode='driving')

#now we add the latitude and longitude to the main data
data$lat <- geocoded$lat
data$long <- geocoded$long
data$accuracy <- geocoded$accuracy

#finally write it all to the output files
saveRDS(data, paste0("../data/", infile ,"_geocoded.rds"))
write.table(geocoded, file=paste0("my_geocoded2.csv"), sep=",", row.names=FALSE)
######################################################


#Plots Hospitals for dataset onto map of Colombia
gc <- geocode("Colombia")
map <- get_map(location=c(lon=gc[,1], lat=gc[,2]), zoom = 6, maptype="roadmap", source="google")
mapPoints <- ggmap(map) + geom_point(aes(x= long, y=lat), data=mygeo.sub2, alpha=.7)
mapPoints



(PointA <- as.numeric(geocode("University of the Andes, Carrera Primera #18A-12, Bogotá, Colombia")))
(PointB <- as.numeric(geocode("Colegio Marymount, Calle 169B, Bogotá, Colombia")))
mapdist(PointA, PointB, mode = c("driving"),
        output = "simple", messaging = FALSE, sensor = FALSE,
        language = "en-EN", override_limit = FALSE)

#Color according to state/region
#Color according to level

