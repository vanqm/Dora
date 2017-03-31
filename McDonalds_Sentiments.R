# Data from https://www.crowdflower.com/data-for-everyone/
# Goal: classify why the locations received low reviews
# Options given were:
# Rude Service      Slow Service    Problem with Order  Bad Food
# Bad Neighborhood  Dirty Location  Cost                Missing Item

data <- read.csv('../Downloads/McDonalds-Yelp-Sentiment-DFE.csv')
names(data)

# Show 3-first rows
head(data[, 1:3], 3)
head(data[, 4:6], 3)
head(data[, 7:9], 3)
head(data[, 10], 3)

# There are some fields that there is no meaning.
# Such as: X_unit_id, X_last_judgment_at. So remove them
data <- data[, c(-1, -5)]
names(data)

# All values of policies_violated_gold are NA.
# So this field also no meaning, remove it
data$policies_violated_gold <- NULL

# Use 'reviews' variable to store reviews
#reviews <- data$review
#data$review <- NULL
head(data)

# 'X_trusted_judgments', 'X_golden' and 'X_unit_state' no meaning. Remove them
str(data)
unique(data$X_golden)
unique(data$X_unit_state)

data$X_golden <- NULL
data$X_unit_state <- NULL
data$X_trusted_judgments <- NULL

# There are relationship between 'policies_violated' vs. 'policies_violated.confidence'
# We should create new fields base on 'policies_violated' with
# value from 'policies_violated.confidence'

# policies_violated = 'na' or policies_violated = '' is no meaning.
# remove those records
dim(data[data$policies_violated == 'na', ])
data <- data[data$policies_violated != 'na', ]
dim(data[data$policies_violated == '', ])
data <- data[data$policies_violated != '', ]
head(data)

parse_policies_violated <- function(dat = data){
  # First: type of 'policies_violated' is factor. So convert it to characters
  dat$policies_violated <- as.character(dat$policies_violated)
  # Second: there are duplicates, just review uinque
  uni_policies_violated <- unique(data$policies_violated)
  for(options in uni_policies_violated){
    arr_option = strsplit(options, '\n')
    # type of arr_option is list, so we use [[1]] to get the value
    for(option in arr_option[[1]]){
      # Not do anything if 'option' exists
      if(any(option == names(dat))){
        #print(option)
      } else {
        new_col <- 0
        dat <- cbind(dat, new_col)
        names(dat)[length(names(dat))] <- option
      }
      
    }
  }
  
  return(dat)
}

data <- parse_policies_violated(data)

# Let's see the new fields
names(data)
head(data[, 5:13])

parse_policies_violated.confidence <- function(dat = data){
  # First: type of 'policies_violated' is factor. So convert it to characters
  dat$policies_violated.confidence <- as.character(dat$policies_violated.confidence)
  dat$policies_violated <- as.character(dat$policies_violated)
  for(i in 1:nrow(dat)){
    confid.list = strsplit(dat$policies_violated.confidence[i], '\n')
    option.list = strsplit(dat$policies_violated[i], '\n')
    # strsplit() return a list, so we use [[1]] to get the value
    for(j in 1:length(option.list[[1]])){
      dat[i, option.list[[1]][j]] <- confid.list[[1]][j]
    }
  }
  
  return(dat)
}

data <- parse_policies_violated.confidence(data)

# Review the result
names(data)
head(data[, 5:13])

# 'na' field has no meaning. Remove it.
data$na <- NULL
head(data[, 5:12])

# Convert all policies violated fields to number
for(i in 5:12){
  data[, i] <- as.numeric(data[, i])
}
summary(data)

# Groupby 'city', calculate the mean
avg.city.policies.violated <- function(dat = data){
  cities <- unique(dat$city)
  # Remove empty city
  cities <- cities[cities != '']
  avg.df <- data.frame()
  
  for(city in cities){
    # apply() - option: 1-row, 2-column
    avg.col.by.city <- apply(dat[dat$city == city, 5:12], 2, function(x) {
      mean(x[x != 0])
    })
    avg.df <- rbind(avg.df, avg.col.by.city)
  }
  # Add city column at the end data frame
  avg.df <- cbind(avg.df, as.character(cities))
  names(avg.df) <- names(dat)[c(5:12, 3)]
  
  return(avg.df)
}

avg.cities <- avg.city.policies.violated(dat = data)

# Visualize the cleaned data
library(ggplot2)
g <- ggplot(avg.cities, aes(x = RudeService, colour = factor(city))) + geom_histogram()
g <- g + facet_grid(. ~ city)
g

