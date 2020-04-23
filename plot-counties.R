library(tidyverse)
library(usmap)
library(ggplot2)
library(rerddap)
library(maps)

plot_usmap(data = statepop, values = "pop_2015", color = "red") + 
    scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
    theme(legend.position = "right")

plot_usmap(regions = "counties") + 
    labs(title = "US Counties",
         subtitle = "This is a blank map of the counties of the United States.") + 
    theme(panel.background = element_rect(color = "black", fill = "lightblue"))
?plot_usmap

# read CSVs
columns <- c('district', 'fin_sub', 'chargeable_fin_number', 'po_name',
             'unit_name', 'property_address', 'county', 'city', 'state', 'zip', 
             'property_state', 'ownership', 'fdb_id', 'ams_locale',
             'fdb_facility_type', 'fdb_facility_subtype', 
             'building_ownership_description', 'land+description', 
             'space_certified_indicator', 'building_occurrence_date',
             'area_sq_ft')

data_directory <- '/Users/PJ/Documents/covid-and-voting/data'
csv_names <- list.files(data_directory)

setwd(data_directory)

data <- read_csv("ak.csv", col_names=columns, skip=4)

for (csv_file in csv_names[2:length(csv_names)]) {
    new_data <- read_csv(csv_file, col_names=columns, skip=4)
    data <- rbind(data, new_data)
}

#str(data)
#head(data)
write.csv(data, file="./data_final.csv", quote = FALSE, row.names = FALSE)

# join state and county
data$county <- str_to_title(data$county); head(data$county)
state_county <- data %>% select(state, county) %>% unite(state_county, sep=", ")
data$state_county <- state_county$state_county

# manually expand county names for counties with three or more words
temp <- str_replace(data$county, 'Fairbanks N Star', 'Fairbanks North Star')
temp <- str_replace(data$county, 'Prince Of Wales-Outer Ketchikan', 'Prince Of Wales-Outer Ketchikan')
temp <- str_replace(data$county, 'Dist Of Columbia', 'District of Columbia')
temp <- str_replace(data$county, 'St. Louis City', 'St. Louis city')
temp <- str_replace(data$county, 'Lake Of The Woods', 'Lake of the Woods')
temp <- str_replace(data$county, 'Isle Of Wight ', 'Isle of Wight ')
temp <- str_replace(data$county, 'Lewis And Clark', 'Lewis and Clark')
temp <- str_replace(data$county, 'Fond Du Lac', 'Fond du Lac')
temp <- str_replace(data$county, 'And', 'and')
temp <- str_replace(data$county, 'Of', 'of')
temp <- str_replace(data$county, 'The', 'the')
data$county <- temp

data %>% select(state_county) %>% mutate(fips=fipscounty(county = state_county)) %>% top_n()

get_fips <- function(state_counties, progress=TRUE){
    fips <- c()
    index <- c()
    for (i in 1:length(state_counties)) {
        if(progress==TRUE){
            cat('\014')
            cat(paste0(round(i / (length(state_counties)) * 100, 4), '% completed\n'))
        }
        index <- c(index, i)
        fip_code <- tryCatch(
            {
                fipscounty(county = state_counties[i])
            },
            error=function(cond) {
                return(NA)
            }
        )
        fips <- c(fips, fip_code)
    }
    return(list(fips, index))
}

fips <- get_fips(data$state_county)
tail(fips)

data$fips <- fips

project_directory <- '/Users/amandeeprathee/work/corona-visualization/v2/'
setwd(project_directory)
save(data, file="usps_presence.Rda")

# filter data
data$county <- str_to_title(data$county); head(data$county)
state_county <- data %>% select(state, county) %>% unite(state_county, sep=", ")
data$state_county <- state_county$state_county

county_names <- us_map(regions = "counties") %>% group_by(county, fips) %>% summarise(count = n())
data$county_suffix <- rep("County", times=nrow(data))
data$county <- (data %>% select(county, county_suffix) %>% unite(county_new, sep=" "))$county_new
str(data)

county_names[county_names %in% c('Lewis And Clark County'), ]
county_names
print(county_names, n=1000)

# group by counties and summarize count

# plot count and counties
plot_usmap(regions = "counties") + 
    labs(title = "US Counties",
         subtitle = "This is a blank map of the counties of the United States.") + 
    theme(panel.background = element_rect(color = "black", fill = "lightblue"))

c <- data$state_county[38]; c
fipscounty(county = c)
fipscounty(county = 'WI, Fond du Lac')


num_words <- sapply(data$county, function(x){length(str_split(x, ' ')[[1]])})
summary(num_words)
which(num_words >= 3)
length(num_words[num_words >= 3])


data$building_type <- ifelse(str_detect(data$unit_name, '.*MAIN OFFICE.*'), "post_office", "not_post_office")
table(data$building_type)

data <- data[data$building_type == 'post_office', ]
str(data)

num_post_offices <- data %>% select(state_county) %>% group_by(state_county) %>% summarise(num_post_offices = n())
str(num_post_offices)

get_fips <- function(state_counties, progress=TRUE){
    fips <- c()
    index <- c()
    for (i in 1:length(state_counties)) {
        if(progress==TRUE){
            cat('\014')
            cat(paste0(round(i / (length(state_counties)) * 100, 4), '% completed\n'))
        }
        index <- c(index, i)
        fip_code <- tryCatch(
            {
                fipscounty(county = state_counties[i])
            },
            error=function(cond) {
                return(NA)
            }
        )
        fips <- c(fips, fip_code)
    }
    return(list(fips, index))
}

fips <- get_fips(num_post_offices$state_county)
table(is.na(fips[[1]]))
tail(fips)
str(data)

str(num_post_offices)
str(fips)
num_post_offices$fips <- fips[[1]]
num_post_offices <- num_post_offices[!is.na(num_post_offices$fips), ]
str(num_post_offices)

project_directory <- '/Users/amandeeprathee/work/corona-visualization/v2/'
setwd(project_directory)
save(num_post_offices, file = "post_offices.Rda")
#save(data, file="usps_presence.Rda")

# extract states
states <- sapply(num_post_offices$state_county, function(x){str_extract(x, "..")})
num_post_offices$abb <- states

merged <- merge(num_post_offices, state.fips, by="abb")
merged$fips.y <- as.character(merged$fips.y)
merged$fips.y <- sapply(merged$fips.y, function(x){str_pad(x, 2, "left", pad='0')})
merged$fips <- (merged %>% select(fips.x, fips.y) %>% unite(fips, sep=''))$fips
merged$fips <- as.integer(merged$fips)
merged$values <- merged$num_post_offices
str(merged)

sub <- merged[, c('fips.x', 'values')]
str(sub)

# plot count and counties
exclude <- c("AK, Aleutians East", "AK, Aleutians West", "AK, Anchorage", "AK, Bethel", "AK, Bristol Bay", "AK, Denali", "AK, Dillingham", "AK, Haines", "AK, Juneau", "AK, Kenai Peninsula", "AK, Kodiak Island", "AK, Nome", "AK, North Slope", "AK, Northwest Arctic", "AK, Sitka", "AK, Valdez-Cordova", "AK, Wade Hampton", "AK, Wrangell-Petersburg", "AK, Yukon-Koyukuk", "AL, Autauga", "AL, Baldwin", "AL, Barbour", "AL, Blount", "AL, Bullock", "AL, Butler", "AL, Calhoun", "AL, Chambers", "AL, Chilton", "AL, Clarke", 
             "AL, Coffee", "AL, Colbert", "AL, Conecuh", "AL, Coosa", "AL, Covington", "AL, Crenshaw", "AL, Cullman", "AL, Dale", "AL, Dallas", "AL, De Kalb", "AL, Elmore", "AL, Escambia", "AL, Etowah", "AL, Fayette", "AL, Franklin", "AL, Greene", "AL, Hale", "AL, Jackson", "AL, Jefferson", "AL, Lauderdale", "AL, Lee", "AL, Limestone", "AL, Madison", "AL, Marengo", "AL, Marion", "AL, Marshall", "AL, Mobile", "AL, Monroe", "AL, Morgan", "AL, Perry", "AL, Pickens", "AL, Pike", "AL, Russell") 
plot_usmap(data=sub, 
           regions = "counties", 
           values = "values") + 
    labs(title = "US Counties",
         subtitle = "This is a blank map of the counties of the United States.") + 
    theme(panel.background = element_rect(color = "black", fill = "lightblue"))

# get fips codes for states
fipscounty(county = "OR, Multnomah")
state.fips
sub <- county.fips
sub$values <- rep(1:nrow(sub))
names(sub) <- c('fips', 'values')
str(sub)

plot_usmap(data=sub[, c('fips', 'values')], 
           regions = "counties", 
           values = "values") + 
    labs(title = "US Counties",
         subtitle = "This is a blank map of the counties of the United States.") + 
    theme(panel.background = element_rect(color = "black", fill = "lightblue"))

str(state.fips)


plot_usmap(data = statepop[, c('fips', 'pop_2015')], values = "pop_2015", color = "red") + 
    scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
    theme(legend.position = "right")


nrow(merged)
sum(merged$fips.x %in% county.fips$fips)
merged[!county.fips$fips %in% merged$fips.x, c('fips.x')]
merged$fips.x %in% county.fips$fips
sum(county.fips$fips %in% merged$fips.x)
county.fips[!county.fips$fips %in% merged$fips.x, c('fips')]


plot_usmap(data=merged2, regions = "counties", values = 'values')

str(countypop)
str(sub)
merged2 <- merge(countypop, sub, by='fips', all.x = TRUE, all.y = FALSE)
merged2 <- anti_join(countypop, sub, by='fips')
str(merged2)
summary(merged2)

sub %>% select(fips, values) %>% group_by(fips) %>% summarise(count = n()) %>% arrange(desc(count)) %>% top_n(10)


install.packages(c("choroplethr", "choroplethrMaps")) 
library(choroplethr)
library(choroplethrMaps)
data(df_pop_county)
head(df_pop_county)
str(df_pop_county)
str(sub)

names(sub) <- c('region', 'value')
sub$region <- as.integer(sub$region)


data %>% group_by(county) %>%summarise(count=n())
length(unique(data$county))
county_choropleth(sub)

load("../post_offices.Rda")
write.csv(num_post_offices, file = 'num_post_offices.csv', quote = FALSE, row.names = FALSE)
str(num_post_offices)
