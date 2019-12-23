# Example 1
results <- gmapsdistance(origin="Washington+DC", destination="New+York+City+NY", mode = "driving")
results
# Example 2
results <- gmapsdistance(origin="38.1621328+24.0029257", destination="37.9908372+23.7383394", mode="walking")
results
# Example 3
results <- gmapsdistance(origin = c("Seattle+WA", "Miami+FL"),
                         destination = c("Chicago+IL", "Philadelphia+PA"),
                         mode="bicycling", dep_date="2022-08-16", dep_time="20:40:00")
results
# Example 4
origin <- c("Washington+DC", "Miami+FL")
destination <- c("Los+Angeles+CA", "Austin+TX", "Chicago+IL")
results <- gmapsdistance(origin, destination, mode = "driving", shape="long")
results
# Example 5
origin <- c("40.431478+-80.0505401", "33.7678359+-84.4906438")
destination <- c("43.0995629+-79.0437609", "41.7096483+-86.9093986")
results <- gmapsdistance(origin, destination, mode="bicycling", shape="long")
results
# Example 6 (do not run – needs an API key)
results <- gmapsdistance(origin = c("Washington+DC", "New+York+NY"),
                         destination = c("Los+Angeles+CA", "Austin+TX"), mode = "driving",
                         departure = 1614742000,
                         traffic_model = "pessimistic",
                         shape = "long", key="AIzaSyCBu0S7RNGFs132j2v_Ae0kzwULGsYHFlk")
results
# Example 7
results <- gmapsdistance(origin=c("Washington+DC", "New+York+NY"),
                         destination=c("Los+Angeles+CA", "Austin+TX"), mode="driving",
                         avoid = "tolls", key="AIzaSyCBu0S7RNGFs132j2v_Ae0kzwULGsYHFlk")
results

# First example of mine
results <- gmapsdistance(origin="Neugasse+8005+Zürich", destination="Schwandenwiesen+8052+Zürich", mode = "driving")
results <- gmapsdistance(origin="Felsenrainstrasse+67+8052+Zurich", destination="Koschenrutistrasse+59+8052+Zurich", mode = "driving")
results <- gmapsdistance(origin="47.427357+8.539217", destination="47.427916+8.545794", mode = "driving")
