library("gmapsdistance")
# "Licence" for the use for google maps: distance matrix API
set.api.key("AIzaSyCBu0S7RNGFs132j2v_Ae0kzwULGsYHFlk")
library("tidyverse")
library("lubridate")
library("rvest")
library("mailR")
library("knitr")
library("kableExtra")
library("gmailr")
# File with credentials for gmail API
use_secret_file("client_secret_748962735150-5j67efo0a96lstkdj83dkglr2akon4eo.apps.googleusercontent.com.json")



# Get the new E-Mails from homegate from the last day (gmail)
# Check this also: messages("after:1554386100"). Works with Epoch Time
new_messages <- messages("from:noreply@notifications.homegate.ch newer_than:1d", user_id = "me")
new_messages <- new_messages[[1]]$messages
new_messages <- dplyr::bind_rows(new_messages)
# Can also be achieved like this:
# do.call(rbind.data.frame, new_messages)
# data.frame(t(sapply(new_messages,c)))

source("functions.R")
gmail_summary <- lapply(new_messages$id, get_link_gmail)
gmail_summary <- dplyr::bind_rows(gmail_summary)

saved_html_copies <- map(gmail_summary$link, definitely_read_html)
to_be_removed <- which(is.na(saved_html_copies))

saved_html_copies <- saved_html_copies[-to_be_removed]
email_time <- gmail_summary$timestamp[-to_be_removed]

# saved_html_copy <- saved_html_copies[[12]]
# tast <- saved_html_copy %>%
#   html_nodes("div") %>%
#   html_children()
# tast[50]
# saved_html_copy %>%
#   html_nodes("a") %>%
#   html_attrs()
# 
# saved_html_copy %>%
#   html_nodes("a") %>%
#   html_attr("href")
# 
# saved_html_copy %>%
#   html_nodes(".data") %>%
#   html_text()
# 
# 
# saved_html_copy %>%
#   html_nodes(".btn btn-primary margined--right-half") %>%
#   html_text()
# 
# 
# temp <- generate_new_row(saved_html_copy,email_time[11])
# tibble_as_html <- kable(temp, format = "html", escape = FALSE) %>%
#   kable_styling(bootstrap_options = c("striped", "hover",
#                                       "condensed", "responsive", "bordered", full_width = F))
# 
# wicket:container
# col-xs-12

generate_new_row <- function(saved_html_copy, mail_time){
  # Get a class with period character (.)
  # Get an ID with hashtag character (#)
  info <- saved_html_copy %>%
    html_nodes("#tatmKV") %>%
    html_text()
  
  # Get ALL classes of the node:
  # saved_html_copy %>%
  #   html_nodes("*") %>%
  #   html_attr("class") %>%
  #   unique()
  # Get the description of the object:
  # saved_html_copy %>%
  #   html_nodes(".description-content") %>%
  #   html_text()
  
  
  
  # To find the source of the images I retrieved all the attributes of "div":
  # images <- saved_html_copy %>%
  # html_nodes("div") %>%
  #   html_attrs()
  # And checked the text from each attribute (mainly classes). For example:
  # images <- saved_html_copy %>%
  #   html_nodes("div.box") %>%
  #   html_text()
  # FAILURE!
  # I followed the same proceedure with "figure". Also FAILURE
  # images <- saved_html_copy %>%
  #   html_nodes("figure") %>%
  #   html_attrs()
  # With "img" it worked:
  
  image_URL <- saved_html_copy %>%
    html_nodes("img") %>%
    html_attr("data-lazy")
  
  image_URL <- image_URL[!is.na(image_URL)][1]
  
  # Two random Photos:
  # image_URL <- sample(image_URL,2)
  
  object_Nr <- saved_html_copy %>%
    html_nodes("div.detail-page--with-ad") %>%
    html_attr("data-detail-id")
  
  codes <- c("kvrentprice", "kvroom", "kvlivingarea", "kvavailable_from", "kvstreet", "kvzip",
             "kvregion", "kvyearbuilt", "kvyearrenovation")
  
  values <- sapply(codes, value_generator, information=info)
  # That above is a character vector. Convert to tibble:
  values <- as_tibble(as.list(values))
  values <- type_convert(values, na = character(), col_types = cols(
    kvrentprice = col_double(),
    kvroom = col_double(),
    kvlivingarea = col_double(),
    kvavailable_from = col_date(),
    kvstreet = col_character(),
    kvzip = col_character(),
    kvregion = col_character(),
    kvyearbuilt = col_integer(),
    kvyearrenovation = col_integer()),
    locale = locale(
      date_format = "%d.%m.%Y",
      decimal_mark = ".",
      encoding = "UTF-8")
  )
  
  
  duration_transit <- duration(values$kvstreet, values$kvzip)
  
  new_row <- values %>%
    select(`Region`= kvregion,
           `Rent price [CHF]`= kvrentprice,
           `Rooms`= kvroom,
           `Area [m^2]`= kvlivingarea,
           `Available from`= kvavailable_from,
           `Year Built`= kvyearbuilt,
           `Renovation Year`= kvyearrenovation) %>%
    add_column(`Bill (Public Transport)`=duration_transit$Bill,
               `E-Mail Date`=format(mail_time, "%a %d %b, %H:%M")) %>%
    mutate_at(vars(`Available from`), list(~format(.,"%d %b %y")))
  # By applying the format, date and datetime variable are -logically- converted to text
  # It may want revision in the future
  # Check the last line. It is wonderful
  
  
  new_row$`First Picture` <- str_c('<a href="https://www.homegate.ch/mieten/', object_Nr,
                                   '/"> <img src="', image_URL,
                                   '" alt=" " style="width:250px;border:1;">', sep = "", collapse = "")
  return(new_row)
}

test <- map2(saved_html_copies, email_time, generate_new_row)
test <- dplyr::bind_rows(test)


tibble_as_html <- kable(test, format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover",
                                      "condensed", "responsive", "bordered", full_width = F))


daily_mail <- mime(
  To = "elisa.bartzoka@gmail.com, bazzinp@gmail.com",
  From = "bazzinp@gmail.com",
  Subject = paste("Daily Update:", nrow(test), "neue Mietobjekte am",
                  format(now(tzone = "Europe/Zurich"), "%A, %d %B %Y"))) %>%
  html_body(tibble_as_html)
send_message(daily_mail)



# temp2 <- '<table class="table table-striped table-hover table-condensed table-responsive table-bordered" style="margin-left: auto; margin-right: auto;">
#  <thead>
# <tr>
# <th style="text-align:left;"> Region </th>
# <th style="text-align:right;"> Rent price [CHF] </th>
# <th style="text-align:right;"> Rooms </th>
# <th style="text-align:right;"> Area [m^2] </th>
# <th style="text-align:left;"> Available from </th>
# <th style="text-align:right;"> Year Built </th>
# <th style="text-align:right;"> Renovation Year </th>
# <th style="text-align:left;"> Eli (Public Transport) </th>
# <th style="text-align:left;"> Bill (Public Transport) </th>
# <th style="text-align:left;"> Pictures </th>
# </tr>
# </thead>
# <tbody>
# <tr>
# <td style="text-align:left; border: 1px solid black; padding: 10px;"> Zurich Kreis 5 </td>
# <td style="text-align:right; border: 1px solid black; padding: 10px;"> 2250 </td>
# <td style="text-align:right; border: 1px solid black; padding: 10px;"> 2.5 </td>
# <td style="text-align:right; border: 1px solid black; padding: 10px;"> 58 </td>
# <td style="text-align:left; border: 1px solid black; padding: 10px;"> 2019-05-01 </td>
# <td style="text-align:right; border: 1px solid black; padding: 10px;"> 2014 </td>
# <td style="text-align:right; border: 1px solid black; padding: 10px;"> NA </td>
# <td style="text-align:left; border: 1px solid black; padding: 10px;"> 00:22:36 </td>
# <td style="text-align:left; border: 1px solid black; padding: 10px;"> 00:07:50 </td>
# <td style="text-align:left; border: 1px solid black; padding: 10px;"> <img src="https://media.homegate.ch/v1/listings/hgof/images/201903231035322928445.jpg/s/703/474" alt="" border=1 width=250></img> </td>
# </tr>
# </tbody>
# </table>'



# email <- send.mail(from = sender,
#                    to = recipients,
#                    subject="Subject of the email",
#                    body = deliverable, # can also point to local file (see next example),
#                    html = TRUE,
#                    smtp = list(host.name = "smtp.gmail.com",
#                                port = 465, user.name = "bazzinp",
#                                passwd = "...", ssl = TRUE),
#                    authenticate = TRUE,
#                    send = TRUE)



# saveRDS(something, "data_tibble.rds")
# data_as_tibble <- readRDS("data_tibble.rds")




















