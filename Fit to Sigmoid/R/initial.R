library(httr)
library(jsonlite)

url = "https://api.datadrum.com/json/cv.IN_cases_cum" #Get data from ECDC (via Data Drum)
httpResponse <- GET(url, add_headers("token"="covid19"), accept_json())
raw_data <- fromJSON(content(httpResponse,"text"))

y=raw_data$data$cv___IN_cases_cum #Get it into y

i=0
ToDitch <- vector() # Create an empty array of values to ditch

for (value in y)
{
  if (value<=30) # Ditch days when number of cases is below a certain value
  {
    ToDitch <- c(ToDitch, value) # Add those values to the arrray
  }
  i = i+1;
}

y <- subset(y, !(y %in% ToDitch)) # Empty y of the ToDitch array
x = seq(1, length(y), by=1) # Create x

plot(x,y)

data <- data.frame(x,y)

# https://stackoverflow.com/a/33034608/13136079 Fit a logistic/sigmoid curve
# Asym/(1+exp((xmid-input)/scal))
#
# Looking at https://towardsdatascience.com/covid-19-infection-in-italy-mathematical-models-and-predictions-7784b4d7dd8d,
#
# scal = infection speed
# xmid = day (after day with 30 cases) with max cases
# Asym = total cases at end

fit <- nls(data$y ~ SSlogis(data$x, Asym, xmid, scal), data = data)
print(fit)



# x = seq(1:1000)
# y = 81109.475/(1+exp((22.313-x)/4.462))

# norm <- function(x) {x/max(x)}
# y_norm <- norm(y)
# 
# print (y_norm)

# require(nplr)
# 
# sigmoid_fit <- nplr(x=x, y=y_norm)
# plot(sigmoid_fit, main="China")
# print(sigmoid_fit)

