library(tidyverse)
library(httr)
library(jsonlite)
library(data.table)

# reading in data 

dat = list.files(path = "/Users/burchm/Downloads/", pattern = "^2019.*-bluebikes-tripdata\\.csv$", full.names = TRUE)

data_19 = do.call(rbind, lapply(dat, function(file) fread(file, header=TRUE)))


# collecting business data for Tremont St / West St Station (lat: 42.35498, lon: -71.06335)


lat = 42.35498
lon = -71.06335

overpass_url = "http://overpass-api.de/api/interpreter"

# food
query1 = sprintf(
  '[out:json];
    (
        node["amenity"~"restaurant|cafe|fast_food|bar"](around:400,%s,%s);
        way["amenity"~"restaurant|cafe|fast_food|bar"](around:400,%s,%s);
        relation["amenity"~"restaurant|cafe|fast_food|bar"](around:400,%s,%s);

    );
    out center;', lat, lon, lat, lon, lat, lon
)

response1 = POST(overpass_url, body = list(data = query1), encode = "form")

# retail
query2 = sprintf(
  '[out:json];
    (
        node["shop"](around:400,%s,%s);
        way["shop"](around:400,%s,%s);
        relation["shop"](around:400,%s,%s);

    );
    out center;', lat, lon, lat, lon, lat, lon
)

response2 = POST(overpass_url, body = list(data = query2), encode = "form")

# work
query3 = sprintf(
  '[out:json];
    (
        node["amenity"="coworking_space"](around:400,%s,%s);
        way["amenity"="coworking_space"](around:400,%s,%s);
        relation["amenity"="coworking_space"](around:400,%s,%s);
        
        node["office"](around:400,%s,%s);
        way["office"](around:400,%s,%s);
        relation["office"](around:400,%s,%s);
        
        node["industrial"](around:400,%s,%s);
        way["industrial"](around:400,%s,%s);
        relation["industrial"](around:400,%s,%s);

    );
    out center;', lat, lon, lat, lon, lat, lon, lat, lon, lat, lon, lat, lon, lat, lon, lat, lon, lat, lon
)


response3 = POST(overpass_url, body = list(data = query3), encode = "form")

# recreation
query4 = sprintf(
  '[out:json];
    (
        node["amenity"~"cinema|theatre"](around:400,%s,%s);
        way["amenity"~"cinema|theatre"](around:400,%s,%s);
        relation["amenity"~"cinema|theatre"](around:400,%s,%s);
        
        node["leisure"~"park|fitness_centre|stadium"](around:804.67, 48.8588443, 2.2943506);
        way["leisure"~"park|fitness_centre|stadium"](around:804.67, 48.8588443, 2.2943506);
        relation["leisure"~"park|fitness_centre|stadium"](around:804.67, 48.8588443, 2.2943506);
  
    );
    out center;', lat, lon, lat, lon, lat, lon, lat, lon, lat, lon, lat, lon
)

response4 = POST(overpass_url, body = list(data = query4), encode = "form")

# education
query5 = sprintf(
  '[out:json];
    (
        node["amenity"~"school|college|university|library"](around:400,%s,%s);
        way["amenity"~"school|college|university|library"](around:400,%s,%s);
        relation["amenity"~"school|college|university|library"](around:400,%s,%s);

    );
    out center;', lat, lon, lat, lon, lat, lon
)

response5 = POST(overpass_url, body = list(data = query5), encode = "form")

# healthcare
query6 = sprintf(
  '[out:json];
    (
        node["amenity"~"hospital|clinic|doctors|pharmacy|dentist"](around:400,%s,%s);
        way["amenity"~"hospital|clinic|doctors|pharmacy|dentist"](around:400,%s,%s);
        relation["amenity"~"hospital|clinic|doctors|pharmacy|dentist"](around:400,%s,%s);

    );
    out center;', lat, lon, lat, lon, lat, lon
)

response6 = POST(overpass_url, body = list(data = query6), encode = "form")

parse = function(response) {
  if (response$status_code == 200) {
    result = content(response, "text")
    json_parsed = fromJSON(result)
  } else {
    stop("Error")
  }
  
  if (length(json_parsed$elements) > 0) {
    extracted_data = json_parsed$elements
  }
  
  place_lat = c()
  place_lon = c()
  place_name = c()
  place_type = c()
  
  for (i in 1:length(extracted_data[,1])) {
    
    print(extracted_data[,5])
    
    if (!is.na(extracted_data[,3][i])) {
      place_lat = c(place_lat, extracted_data[,3][i])
    } else if (extracted_data[,6]['lat'][i,] != "NA") {
      place_lat = c(place_lat, extracted_data[,6]['lat'][i,])
    } else {
      place_lat = c(place_lat, NA)
    }
    
    if (!is.na(extracted_data[,4][i])) {
      place_lon = c(place_lon, extracted_data[,4][i])
    } else if (extracted_data[,6]['lon'][i,] != "NA") {
      place_lon = c(place_lon, extracted_data[,6]['lon'][i,])
    } else {
      place_lon = c(place_lon, NA)
    }
    
    if (!is.null(extracted_data[,5]['name'][i,])) {
      place_name = c(place_name, extracted_data[,5]['name'][i,])
    } else {
      place_name = c(place_name, NA)
    }
    
    if ("amenity" %in% colnames(extracted_data[,5]) && !is.null(extracted_data[,5]['amenity'][i,])) {
      place_type = c(place_type, extracted_data[,5]['amenity'][i,])
    } else {
      place_type = c(place_type, NA)
    }
    
    if ("shop" %in% colnames(extracted_data[,5]) && !is.null(extracted_data[,5]['shop'][i,])) {
      place_type = c(place_type, extracted_data[,5]['shop'][i,])
    } else {
      place_type = c(place_type, "shop")
    }
    
    if ("leisure" %in% colnames(extracted_data[,5]) && !is.null(extracted_data[,5]['leisure'][i,])) {
      place_type = c(place_type, extracted_data[,5]['leisure'][i,])
    } else {
      place_type = c(place_type, "leisure")
    }
    
    if ("office" %in% colnames(extracted_data[,5]) && !is.null(extracted_data[,5]['office'][i,])) {
      place_type = c(place_type, extracted_data[,5]['office'][i,])
    } else {
      place_type = c(place_type, "office")
    }
    
    if ("industrial" %in% colnames(extracted_data[,5]) && !is.null(extracted_data[,5]['industrial'][i,])) {
      place_type = c(place_type, extracted_data[,5]['industrial'][i,])
    } else {
      place_type = c(place_type, "industrial")
    }
    
  }
  
  df = data.frame(
    Name = place_name,
    Latitude = place_lat, 
    Longitude = place_lon, 
    Type = factor(place_type),
    stringsAsFactors = FALSE
  )
  
  return(df)
}


data = rbind(parse(response1), parse(response2), parse(response3), parse(response4), parse(response5), parse(response6))


summary(data)



# creating a bar plot

library(dplyr)
library(ggplot2)

counts = data %>% count(Type)

ggplot(counts, aes(x = Type, y = n)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Business Frequencies within 0.5 miles of Tremont St Station", x = "Business Type", y = "Frequency") +
  coord_flip() + 
  theme_minimal()







