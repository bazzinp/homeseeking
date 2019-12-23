# ----- function: get_link_gmail() --------------
# Function to get the link from gmail so as to access the node of interest
# Unfortunately it is a redirection link so we cannot get the number of the Ad directly
# If it were that simple we could access the desired node like "https://www.homegate.ch/mieten/Ad_Number"
# We get the number of the Ad after we access the node
# We need it for the construction of the final table
# This funtion gives gives the datetime of the e-mail as well
# Expected input is an alphanumeric string with the ID of the e-mail (eg: "169e2a92f6a09a59")
# Output as list of two
# Unfortunately message() function takes only a string as an argument. It is not vectorized
# Therefore one has to use lapply (sapply)
get_link_gmail <- function(messageID){
  current_message <- messageID
  full_message <- message(current_message, user_id = "me", format = c("full"))
  timestamp <- as_datetime(as.numeric(full_message$internalDate)/1000, tz = "Europe/Zurich")
  full_message <- unlist(body(full_message))
  link <- str_extract(full_message, 'https://link.notifications.homegate.ch/wf/click\\?upn.*')
  extractables <- list(link = link, timestamp = timestamp)
}


# --------- function: value_generator() --------------
# After we have the TEXT of the id: "tatmKV", we want to extract the values of the fields of interest
# We do this by applying some REGEX
# To extract each value, we take as input an 1-word-character-string with the property name (eg: kvzip)
# It returns the value of the related property
value_generator <- function(prop, information){
  # Match the expression which has a { at start then it has none ([^\\{]+) and after kvstreet it has infinite characters  followed by a }:(?=\\})
  small_string <- eval(parse(text=paste("str_extract(information,'\\\\{[^\\\\{]+",
                                        prop, ".+?(?=\\\\})')", sep = "")))
  value <- str_extract(small_string, '(?<="value":).*')
  value <- str_extract_all(value, boundary("word"))
  value <- unlist(value)
  value <- str_c(value, collapse = " ")
  # if (prop %in% c("kvrentprice", "kvroom", "kvlivingarea", "kvyearbuilt", "kvyearrenovation")){
  #   value <- as.numeric(value)
  # }
}


# ---------- function: duration() ----------------

# Here we calculate the distance of the object to two locations using the Google Maps distance Matrix API
# If locations are put as addresses, special characters are not recognised and spaces should be substituded with
# a plus sign ("+").
# Inputs are two character strings with the street address and the zip code respectively
# Output of the distance is in [meters] and of the time in [seconds]
# For the future consider the following (probably better):

# address_raw <- saved_html_copy %>%
#   html_nodes("div.detail-address") %>%
#   html_text()

duration <- function(street, zip){
  # ad <- str_c(street, zip, sep = " ")
  # ad <- str_replace_all(ad," ","+")
  # ad <- str_replace_all(ad,"ä","a")
  # ad <- str_replace_all(ad,"ö","ö")
  # ad <- str_replace_all(ad,"ü","u")
  # ad <- str_replace_all(ad,"Ä","A")
  # ad <- str_replace_all(ad,"Ö","O")
  # ad <- str_replace_all(ad,"Ü","U")
  ad <- URLencode(str_c(street, zip, sep = " "), reserved = TRUE)
  Bills_loc <- "47.392569+8.495060"
  # Elis_loc <- "47.357843+8.521206"
  time <- gmapsdistance(origin=ad, destination=c(Bills_loc), mode = "transit")
  duration_transit <- list("Bill" = hms::as.hms(as.numeric(time$Time))#,
                           #"Eli" = hms::as.hms(as.numeric(time$Time[3]))
                           )
}

# ---------- function: definitely_read_html() ----------------
# It can occur that the page does not exist anymore and an error appears (404 error)
# Catch all the errors and convert them to Nans
# It can be that the link is not more active, because the advertisement is removed from
# the site. In this situation an error occurs and the execution of the program stops
# We try to prevent the incurrence of errors and creating NAs instead
# This is achieved by the possibly() function
definitely_read_html <- possibly(function(URL){
  read_html(URL, encoding = "utf8")
}, otherwise = NA)


