
Name: Arun Krishnamurthy (50247445) (arunkris)
Teammate: Ashwin Vijayakumar (50249042) (ashwinvi)

# Libraries needed, install if not available
# install.packages("twitteR", repos = "http://cran.us.r-project.org")
# Twitter lib and oauth
library(twitteR)
setup_twitter_oauth("", "", "", "")

# Map related libraries
library(mapsapi)
library(devtools)
library(placement)
library(sp)
library(maps)
library(maptools)

install.packages("twitteR", repos = "http://cran.us.r-project.org")
library(twitteR)

# Setting up authentication
setup_twitter_oauth("", "", "", "")

# Search tweets for #fluvirus
# This was run for various hashtags and tweets were obtained
tweets2 <- searchTwitter('#fluvirus', n=5000, geocode='39.828830,-98.579668,1500mi') 

# Converting tweets to dataframe
tweets2.df2 = twListToDF(tweets2)

# Writing the tweets to CSV
write.csv(tweets2.df2, file = "C:/Users/Arun/Downloads/20180307geo.csv")

# Retrive screen names from the tweet dataframe
screenNames <- tweets2.df2$screenName

# Obtain user objects for the screennames
users <- lookupUsers(screenNames)

# Function to get location given a user
getLocation <- function(x)
{
    y <- getUser(x)
    location <- y$location
    return(location)
} 

# adding all the locations to a list
locations_flu <- sapply(screenNames, function(x) getLocation(x))

# creating a data frame from the locatinos list
Locations_DF <- ldply (locations_flu, data.frame)
names(Locations_DF)[1] <- paste("ID")
names(Locations_DF)[2] <- paste("Location")
Locations_DF2 <- Locations_DF[!(is.na(Locations_DF[2])| Locations_DF[2]==""),]
dim(Locations_DF2)
Locations_DF2

# Write the locations to a CSV
write.csv(Locations_DF2, file = "C:/Users/Arun/Downloads/fluvirus_Locations20180307geo.csv")

# read all the hashtag specific csvs generated previously
# here you can also see all the different hashtags that were used
hash_cdc <- read.csv("F:/Lab 1/Part 3/data/CDC_Locations20180307geo.csv")
hash_cold <- read.csv("F:/Lab 1/Part 3/data/cold_Locations20180307geo.csv")
hash_deaths <- read.csv("F:/Lab 1/Part 3/data/deaths_Locations20180307geo.csv")
hash_epedemic <- read.csv("F:/Lab 1/Part 3/data/epidemic_Locations20180307geo.csv")
hash_fightflu <- read.csv("F:/Lab 1/Part 3/data/fightflu_Locations20180307geo.csv")
hash_flushot <- read.csv("F:/Lab 1/Part 3/data/Flushot_Locations20180307geo.csv")
hash_flu <- read.csv("F:/Lab 1/Part 3/data/Flu_Locations20180307geo.csv")
hash_fluvirus <- read.csv("F:/Lab 1/Part 3/data/fluvirus_Locations20180307geo.csv")
hash_influenza <- read.csv("F:/Lab 1/Part 3/data/LocationsInfluenza20180307geo.csv")
hash_outbreaks <- read.csv("F:/Lab 1/Part 3/data/outbreaks_Locations20180307geo.csv")
hash_sneezing <- read.csv("F:/Lab 1/Part 3/data/sneezing_Locations20180307geo.csv")
hash_vaccination <- read.csv("F:/Lab 1/Part 3/data/vaccination_Locations20180307geo.csv")
hash_vaccines <- read.csv("F:/Lab 1/Part 3/data/vaccines_Locations20180307geo.csv")
hash_virus <- read.csv("F:/Lab 1/Part 3/data/virus_Locations20180307geo.csv")

# merge csvs together
locations_df <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                list(hash_cdc, hash_cold, hash_deaths, hash_epedemic, hash_fightflu, hash_flushot, hash_flu, hash_fluvirus, hash_influenza, hash_outbreaks, hash_sneezing, hash_vaccines, hash_vaccination, hash_virus))
print("CSV files read...")

# making a copy of the locations to work with
locations_copy <- locations_df$Location

# getting the number of locations to see how much loss is there in the pipeline
print("number of location items retreived:")
print(length(locations_copy))
    
# A total of 5970 were downloaded. 5000 was used for the analysis

# convert the locations list to a vector
locations_copy <- as.vector(locations_copy)

print("preparing data...")
# remove numerics, special characters, and NAs
locations_copy <- gsub("[^[:alpha:]///' ]", "", locations_copy)
locations_copy <- na.omit(locations_copy)

print("contacting google api to retreive lats and langs...")
# get a set of coordinates and formal addresses for all the unstructured text
#coordset <- geocode_url(locations_copy[1:2000], auth="standard_api", privkey="AIzaSyDXvHd59c3lMq-BO5H8Q740NeWNIu03uo8", clean=TRUE, verbose=TRUE)

#locations_list <- locations_copy[2501:5000]
# Due to constraints with api call limits 2500 tweets at a time.

# locations were split into chunks of 100
locations_list <- split(locations_list, ceiling(seq_along(testloc)/100))

getCoordset <- function(x) {
    interim_coordset <- geocode_url(x, auth="standard_api", privkey="", clean=TRUE, verbose=TRUE)
    return(interim_coordset)
}

coordset <- sapply(locations_list, function(x) getCoordset(x))

print("preparing coordinate list")
# obtain lat and lang from the coordset
lats <- as.vector(coordset[1])
langs <- as.vector(coordset[2])

# put them together in a dataframe called coordinates, and removing the NAs
coordinates <- data.frame(x = langs, y = lats)
coordinates <- na.omit(coordinates)

# defining a function to fetch state names from coordinate set
latlong2state <- function(pointsDF) {
    # Prepare SpatialPolygons object with one SpatialPolygon
    # per state (plus DC, minus HI & AK)
    states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
    IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
    states_sp <- map2SpatialPolygons(states, IDs=IDs,
                     proj4string=CRS("+proj=longlat +datum=WGS84"))

    # Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                    proj4string=CRS("+proj=longlat +datum=WGS84"))

    # Use 'over' to get _indices_ of the Polygons object containing each point 
    indices <- over(pointsSP, states_sp)

    # Return the state names of the Polygons object containing each point
    stateNames <- sapply(states_sp@polygons, function(x) x@ID)
    stateNames[indices]
}

print("retreiving state names from maptools...")
# consuming the latlong2state function with coordinates dataframe constructed earlier
states <- latlong2state(coordinates)
# omit the NAs
states <- na.omit(states)

# check the length to check the loss
print("number of location items classified to a state:")
print(length(states))
# putting it all together
STATENAME <- states
states_freq <- as.data.frame(table(STATENAME))
states_freq

write.csv(states_freq, file = "F:/UB/Sem 2/CSE 587 - Data Intensive Computing/Lab 1/Part 3/out/20180309new1600.csv")

library(ggmap)
library(maps)
library(plyr)
library(RColorBrewer)
data_frame = read.csv("F:/Lab 1/Part 3/final123.csv")
data_frame$STATENAME <- (tolower(data_frame$STATENAME))
all_states <- map_data("state")
names(all_states)[5] <- "STATENAME"

total <- merge(all_states , data_frame ,  by = intersect(names(all_states), names(data_frame)) , all= TRUE )
names(total)[1] <- "region"
names(all_states)[5] <- "region"
b<-toString(total$Freq)
b<- unlist(strsplit(b,','))
#total[total$region =="new mexico",]
b<-substr(b,nchar(b)-1, nchar(b))
b<-as.numeric(b)

p <- ggplot()
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')
p <- p + geom_map(data=map_data("state") , map=map_data("state") , aes(long, lat, map_id=region) , color="black" ,fill=NA , size=1)
p <- p + geom_map(data=total, map = map_data("state"), aes(long , lat , map_id=region , fill=b)  ,color="white", size=1 ) + 
    scale_fill_gradient(low="green" , high="red" , na.value="black")
   
P1 <- p + theme_bw()  + labs(title = "Heat Map of Tweets from each region")
P1 <- P1 + theme(legend.position=c(1, 0.2))
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())