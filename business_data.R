library(tidyverse)
library(httr)
library(jsonlite)
library(data.table)
library(dplyr)
library(ggplot2)

# reading in station lat/lon/subscriber proportion from file created by group_bikedata.py
data = read.csv("output.csv", header=TRUE, sep=",")

overpass_url = "http://overpass-api.de/api/interpreter"

# updated query code from HW7.R

queries = function(lat, lon) {
  
  responses = list()
  
  queries_list = list(
    query1 = sprintf(
      '[out:json];
        (
            node["amenity"~"restaurant|cafe|fast_food|bar"](around:400,%s,%s);
            way["amenity"~"restaurant|cafe|fast_food|bar"](around:400,%s,%s);
            relation["amenity"~"restaurant|cafe|fast_food|bar"](around:400,%s,%s);
    
        );
        out center;', lat, lon, lat, lon, lat, lon
    ),
    
    # retail
    query2 = sprintf(
      '[out:json];
        (
            node["shop"](around:400,%s,%s);
            way["shop"](around:400,%s,%s);
            relation["shop"](around:400,%s,%s);
    
        );
        out center;', lat, lon, lat, lon, lat, lon
    ),
    
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
    ),
    
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
    ),
    
    # education
    query5 = sprintf(
      '[out:json];
        (
            node["amenity"~"school|college|university|library"](around:400,%s,%s);
            way["amenity"~"school|college|university|library"](around:400,%s,%s);
            relation["amenity"~"school|college|university|library"](around:400,%s,%s);
    
        );
        out center;', lat, lon, lat, lon, lat, lon
    ),
    
    # healthcare
    query6 = sprintf(
      '[out:json];
        (
            node["amenity"~"hospital|clinic|doctors|pharmacy|dentist"](around:400,%s,%s);
            way["amenity"~"hospital|clinic|doctors|pharmacy|dentist"](around:400,%s,%s);
            relation["amenity"~"hospital|clinic|doctors|pharmacy|dentist"](around:400,%s,%s);
    
        );
        out center;', lat, lon, lat, lon, lat, lon
    ),
    
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
  )
  
  for (i in seq_along(queries_list)) {
    responses[[i]] = POST(overpass_url, body = list(data = queries_list[[i]]), encode = "form")
  }
  
  return(responses)

}

# parsing code from HW7.R

parse = function(response) {
  if (response$status_code == 200) {
    result = content(response, "text")
    json_parsed = fromJSON(result)
  } else {
    stop("Error")
  }

  place_name = c()
  place_type = c()
  
  if (length(json_parsed$elements) > 0) {
    extracted_data = json_parsed$elements
    
    for (i in 1:length(extracted_data[,1])) {
      
      if ("name" %in% colnames(extracted_data[,5]) && !is.null(extracted_data[,5]['name'][i,])) {
        place_name = c(place_name, extracted_data[,5]['name'][i,])
      } else {
        place_name = c(place_name, "NA")
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
          place_type = c(place_type, "NA")
        }
      } else {
        place_type = c(place_type, "NA")
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
  } 
  
  df = data.frame(
    Name = place_name,
    Type = factor(place_type),
    stringsAsFactors = FALSE
  )
  
  return(df)
}


data$Food = 0
data$Healthcare = 0
data$Other = 0
data$N_A = 0
data$Work = 0
data$Education = 0
data$Retail = 0
data$Recreation = 0


for (i in 1:nrow(data)) {
  print(i)
  lat = data$Lat[i]
  lon = data$Lon[i]
  
  responses = queries(lat,lon)
  
  results = rbind(parse(responses[[1]]), parse(responses[[2]]), parse(responses[[3]]), parse(responses[[4]]), parse(responses[[5]]), parse(responses[[6]]), parse(responses[[7]]))
  
  all_categories = c("food", "healthcare", "other", "NA", "work", "education", "retail", "recreation")
  
  counts = results %>% count(Type) %>% complete(Type = all_categories, fill = list(n = 0))
  
  
  for (j in 1:nrow(counts)) {
    if (counts$Type[j] == "food") {
      data$Food[i] = counts$n[j]
    } 
    if (counts$Type[j] == "healthcare") {
      data$Healthcare[i] = counts$n[j]
    } 
    if (counts$Type[j] == "other") {
      data$Other[i] = counts$n[j]
    } 
    if (counts$Type[j] == "NA") {
      data$N_A[i] = counts$n[j]
    } 
    if (counts$Type[j] == "work") {
      data$Work[i] = counts$n[j]
    } 
    if (counts$Type[j] == "education") {
      data$Education[i] = counts$n[j]
    } 
    if (counts$Type[j] == "retail") {
      data$Retail[i] = counts$n[j]
    } 
    if (counts$Type[j] == "recreation") {
      data$Recreation[i] = counts$n[j]
    }
    
  }
      
  }
  
  

write.csv(data, file = "business.csv", row.names = TRUE)

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

