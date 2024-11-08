library(tidyverse)
library(httr)
library(jsonlite)
library(data.table)
library(dplyr)
library(ggplot2)

# READING IN 2019 BLUEBIKE DATA #

dat = list.files(path = "/Users/burchm/Downloads/", pattern = "^2019.*-bluebikes-tripdata\\.csv$", full.names = TRUE)

data_19 = do.call(rbind, lapply(dat, function(file) fread(file, header=TRUE)))


# GROUPING BLUEBIKE DATA BY LATITUDE AND LONGITUDE # # NOT DONE #

grouped_data <- data_19 %>%
  group_by("end station latitude", "end station longitude")

grouped = group_split(grouped_data)

for (group in grouped) {
  print(group["end station latitude"])
}


# QUERY CODE FOR 1 STATION #
# update to loop through stations after grouping

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
        
        node["sport"](around:400,%s,%s);
        way["sport"](around:400,%s,%s);
        relation["sport"](around:400,%s,%s);
        
        node["leisure"](around:804.67, 48.8588443, 2.2943506);
        way["leisure"](around:804.67, 48.8588443, 2.2943506);
        relation["leisure"](around:804.67, 48.8588443, 2.2943506);
  
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

# catch-all
query7 = sprintf(
  '[out:json];
    (
        node["amenity"](around:400,%s,%s);
        way["amenity"](around:400,%s,%s);
        relation["amenity"](around:400,%s,%s);

    );
    out center;', lat, lon, lat, lon, lat, lon
)

response7 = POST(overpass_url, body = list(data = query7), encode = "form")

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
      food = c("concession stand", "food sharing", "ice cream", "Internet cafe", "juice bar", "kitchen", "winery", 
               "restaurant", "cafe", "fast_food", "bar", "food_court")
      work = c("administration", "building yard", "carpet washing", "car rental", "car repair", 
               "car wash", "conference centre", "consulate", "courthouse", "coworking space", 
               "customs", "harbourmaste", "jobcentre", "lost property office", "mailroom", "office", 
               "police", "post office", "reception desk", "register office", "townhall", "workshop", "coworking_space")
      education = c("archive", "driver training", "education", "first aid school", "library", "mobile library", "mtb school", 
                    "planetarium", "preschool", "research institute", "sport school", "surf school", "swimming school", 
                    "youth centre", "school", "college", "university", "dancing_school", "kindergarten")
      healthcare = c("hospice", "hospital", "mobility scooter rental", "nursery", "nursing home", 
                     "clinic", "doctors", "pharmacy", "dentist", "veterinary")
      retail = c("shop")
      recreation = c("arts centre", "casino", "club", "community centre", "concert hall", "convention centre", 
                     "events centre", "events venue", "exhibition centre", "festival grounds", "fish spa", "gym", 
                     "kick-scooter rental", "lounge", "marae", "meditation centre", "music venue", "outfitter", 
                     "park", "rehearsal studio", "sanatorium", "sauna", "scooter rental", "ski rental", 
                     "ski school", "spa", "stables", "stage", "studio", "swimming pool", "theatre", "yacht club", "cinema", 
                     "fitness_centre", "stadium")
      other = c("alm", "boat storage", "coast guard", "coast radar station", "crematorium", "crypt", "embassy", 
                "financial advice", "freeshop", "funeral hall", "garages", "give box", "hotel", "kiosk", "lavoir", "left luggage", 
                "letter box", "lifeboat station", "locker", "luggage locker", "mortuary", "motorcycle rental", 
                "parcel locker", "place of mourning", "place of worship", "prison", "prison camp", "public building", 
                "ranger station", "rescue station", "rv storage", "social centre", "toilets", "tool library", 
                "warehouse")
      residential = c("dormitory", "refugee housing", "refugee site", "retirement home", "shelter", "student accommodation", 
                      "trailer park")
      
      if (extracted_data[,5]['amenity'][i,] %in% food) {
        place_type = c(place_type, "food")
      } else if (extracted_data[,5]['amenity'][i,] %in% work) {
        place_type = c(place_type, "work")
      }  else if (extracted_data[,5]['amenity'][i,] %in% education) {
        place_type = c(place_type, "education")
      }  else if (extracted_data[,5]['amenity'][i,] %in% healthcare) {
        place_type = c(place_type, "healthcare")
      }  else if (extracted_data[,5]['amenity'][i,] %in% retail) {
        place_type = c(place_type, "retail")
      }  else if (extracted_data[,5]['amenity'][i,] %in% recreation) {
        place_type = c(place_type, "recreation")
      }  else if (extracted_data[,5]['amenity'][i,] %in% other) {
        place_type = c(place_type, "other")
      }  else if (extracted_data[,5]['amenity'][i,] %in% residential) {
        place_type = c(place_type, "residential")
      } else {
        place_type = c(place_type, NA)
      }
    } else {
      place_type = c(place_type, NA)
    }
    
    if ("shop" %in% colnames(extracted_data[,5])) {
      food_shop = c("alcohol", "bakery", "beverages", "butcher", "cheese", "chocolate", "coffee", 
                    "confectionery", "dairy", "deli", "farm", "food", "frozen_food", "greengrocer", 
                    "health_food", "ice_cream", "nuts", "pasta", "pastry", "seafood", "spices", 
                    "tea", "tortilla", "wine")
      if (colnames(extracted_data[,5]["shop"][i,]) %in% food_shop && !is.null(colnames(extracted_data[,5]["shop"][i,]))) {
        place_type = c(place_type, "food")
      } else {
        place_type = c(place_type, "retail")
      }
    } 
    
    if ("leisure" %in% colnames(extracted_data[,5])) {
      place_type = c(place_type, "recreation")
    } 
    
    if ("sport" %in% colnames(extracted_data[,5])) {
      place_type = c(place_type, "recreation")
    } 
    
    if ("office" %in% colnames(extracted_data[,5])) {
      place_type = c(place_type, "work")
    } 
    
    if ("industrial" %in% colnames(extracted_data[,5])) {
      place_type = c(place_type, "work")
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


data = rbind(parse(response1), parse(response2), parse(response3), parse(response4), parse(response5), parse(response6), parse(response7))


summary(data)



# creating a bar plot



counts = data %>% count(Type)

ggplot(counts, aes(x = Type, y = n)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Business Frequencies within 0.5 miles of Tremont St Station", x = "Business Type", y = "Frequency") +
  coord_flip() + 
  theme_minimal()







