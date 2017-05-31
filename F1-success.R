# devtools::install_github("baptiste/ggflags")
library(dplyr)
library(ggplot2)
library(stargazer)
library(WDI)
library(data.table)
library(gridExtra)
library(ggflags)

load(file = "f1.rdata")

# Aggregation helper constants
days_before_race <- 5
days_after_race <- 5

# For each race and driver we aggregate the wikipedia views:
# before and after a few days. Basically it will act as a popularity increase indicator.
race <- left_join(x = race, y = schedule[, c("id", "date", "raceName")], by="id")
race$wiki_start_date <- race$date - days_before_race
race$wiki_end_date <- race$date + days_after_race

# create a function that will return the views between two dates for a driver
get_views <- function(driver, start, end) {
  v <- c()
  vi <- 1
  for(i in 1:length(driver)) {
    d <- driver[i]
    s <- start[i]
    e <- end[i]
    df <- subset(wiki, driver_wiki_name==d & date >= s & date <= e)
    v[vi] <- sum(df$views)
    vi <- vi + 1
  }
  return(v)
}

race$wiki_views <- get_views(race$driver_wiki_name, race$wiki_start_date, race$wiki_end_date)
race$log_wiki_views <- log(race$wiki_views + 1) # Log correction

# Let's see the distribution of the success
g1 <- ggplot(data = race, aes(x = wiki_views)) +
  geom_histogram(bins = 30) +
  scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})

g2 <- ggplot(data = race, aes(x = log_wiki_views)) +
  geom_histogram(bins = 30)


grid.arrange(g1, g2, ncol=2, top="Distribution of wiki_views")

# Position vs wiki
ggplot(data = race, aes(x = position, y = log_wiki_views)) +
  geom_point() +
  geom_smooth(method = "loess") +
  ggtitle("Wiki views on position", " +local polynomial smoothing")

reg1 <- lm(data = race, formula = log_wiki_views ~ driver_nationality)

stargazer(reg1, type = "text")

coeffs <- stack(reg1$coefficients[2:length(reg1$coefficients)])

# interesting, maybe average population can be a variable too

# Country population to get
unique(as.character(race$driver_nationality))
# Map nationality to country

wdi <- WDI(country = "all", start = 2008, end = 2008, extra = TRUE, cache = NULL,
           indicator = c("SP.POP.TOTL"))

wdi$nationality <- ""
wdi[wdi$country=="United Kingdom",]$nationality <- "British"
wdi[wdi$country=="Germany",]$nationality <- "German"
wdi[wdi$country=="Spain",]$nationality <- "Spanish"
wdi[wdi$country=="Finland",]$nationality <- "Finnish"
wdi[wdi$country=="Japan",]$nationality <- "Japanese"
wdi[wdi$country=="France",]$nationality <- "French"
wdi[wdi$country=="Poland",]$nationality <- "Polish"
wdi[wdi$country=="Brazil",]$nationality <- "Brazilian"
wdi[wdi$country=="Italy",]$nationality <- "Italian"
wdi[wdi$country=="Australia",]$nationality <- "Australian"
wdi[wdi$country=="Switzerland",]$nationality <- "Swiss"
wdi[wdi$country=="Russian Federation",]$nationality <- "Russian"
wdi[wdi$country=="India",]$nationality <- "Indian"
wdi[wdi$country=="Austria",]$nationality <- "Austrian"
wdi[wdi$country=="Belgium",]$nationality <- "Belgian"
wdi[wdi$country=="Venezuela, RB",]$nationality <- "Venezuelan"
wdi[wdi$country=="Mexico",]$nationality <- "Mexican"
wdi[wdi$country=="Netherlands",]$nationality <- "Dutch"
wdi[wdi$country=="Denmark",]$nationality <- "Danish"
wdi[wdi$country=="Sweden",]$nationality <- "Swedish"
wdi[wdi$country=="United States",]$nationality <- "American"
wdi[wdi$country=="Indonesia",]$nationality <- "Indonesian"

wdi <- wdi[, c("SP.POP.TOTL", "nationality")]
colnames(wdi) <- c("driver_country_population", "nationality")

race <- left_join(race, wdi, by=c("driver_nationality"="nationality"))
race$driver_country_population <- race$driver_country_population / 1000000 # convert to million people

reg2 <- lm(data = race, formula = log_wiki_views ~ driver_country_population)
stargazer(reg2, type = "text", report = "vcp")

# There is no connection between the country population and the success

# Lets calculate the cummulative points in the grand championship from race to race
race <- data.table(race)
race [, cum_points_in_championship:=cumsum(points), by= .(Year, driverId)]

reg3 <- lm(data = race, formula = log_wiki_views ~ cum_points_in_championship)
stargazer(reg3, type = "text")

ggplot(data = race, aes(x=cum_points_in_championship, y=log_wiki_views)) +
         geom_point() + 
          geom_smooth(method = "loess")

# What about the actual standing in the grand championship?
race <- race[order(Year, Round, -rank(cum_points_in_championship))]
race[, current_standing_in_championship:= 1:.N, by= .(Year, Round)]
# Create categories to analyze:
race[current_standing_in_championship > 8, standing_cat_:="Also run"]
race[current_standing_in_championship %in% c(6, 7, 8), standing_cat_:="Has some points"]
race[current_standing_in_championship %in% c(4, 5), standing_cat_:="Almost there"]
race[current_standing_in_championship == 3, standing_cat_:="Third"]
race[current_standing_in_championship == 2, standing_cat_:="Second"]
race[current_standing_in_championship == 1, standing_cat_:="First"]
levels_to_use <- rev(c("First", "Second", "Third", "Almost there", "Has some points", "Also run"))
race[, standing_cat_ := factor(standing_cat_,
                    levels = levels_to_use, ordered = F)]


reg4 <- lm(data = race, formula = log_wiki_views ~  standing_cat_)
stargazer(reg4, type = "text")
# Nice thrend, let's extract the coefficients to plot them
coeffs <- stack(reg4$coefficients[2:6])
coeffs_base <- data.frame(0, "standing_cat_Also run")
colnames(coeffs_base) <- c("values", "ind")
coeffs <- rbind(coeffs_base, coeffs)

coeffs$ind <- levels_to_use
coeffs$ind <- factor(as.character(coeffs$ind), levels=levels_to_use)

colnames(coeffs) <- c("Views", "Category")
coeffs$Views <- coeffs$Views * 100


ggplot(data=coeffs, aes(x=Category, y=Views, label=sprintf("%.02f", Views))) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Additional views in %") +
  ggtitle("Additional views of categories after each race", "Compared to the also run category") +
  geom_text(colour= "red", size=5, nudge_y = 10)


# What is the difference between constructors?

reg5 <- lm(data = race, formula = log_wiki_views ~ constructor_name)
stargazer(reg5, type = "text")

# Acording to the regression some of the constructors can boost their pilots success.
# The coefficients for some of the dummies are not statistically signficant.
# We will create categories here too
# ConstructorClassA : top 25%
# ConstructorClassB : middle 50%
# ConstructorClassC : lower 25%

coeffs <- stack(reg5$coefficients[2:22])
coeffs$ind <- gsub(x=coeffs$ind, pattern = "constructor_name(.*)", replacement = "\\1")
coeffs_base <- data.frame(0, "BMW Sauber")
colnames(coeffs_base) <- c("values", "ind")
coeffs <- rbind(coeffs_base, coeffs)

# Calculate relative popularity boost (100 = BMW Sauber)
coeffs$values <- (coeffs$values + 1) * 100
s <- summary(coeffs$values)
q1 <- s[["1st Qu."]]
q3 <- s[["3rd Qu."]]
coeffs$constructor_category <- ifelse(coeffs$values < q1, "ClassC", ifelse(coeffs$values < q3, "ClassB", "ClassA"))

# Class A
subset(coeffs, constructor_category == "ClassA")

# Class B
subset(coeffs, constructor_category == "ClassB")

# Class C
subset(coeffs, constructor_category == "ClassC")

race <- left_join(x = race, y = coeffs[, c("ind", "constructor_category")], by=c("constructor_name" = "ind"))
race$constructor_category <- factor(as.character(race$constructor_category), levels=c("ClassC", "ClassB", "ClassA"))

reg6 <- lm(data = race, formula = log_wiki_views ~ constructor_category)
stargazer(reg6, type = "text", report = "vcs*", ci = TRUE, omit.stat = "f")

# What if we measure next year contract?
unique(race$status)

race$crashed <- as.numeric( race$status == "Accident" | race$status == "Collision" | race$status == "Disqualified" )

annual <- race %>% group_by(Year, driver_wiki_name) %>% summarize( gc_points=sum(points), gc_avg_points=mean(points), crashed=sum(crashed))

annual$Views <- get_views(annual$driver_wiki_name, as.Date(paste(annual$Year, "-01-01", sep = "")), paste(annual$Year, "-12-31", sep = ""))
annual$log_views <- log(annual$Views)

drove_in_year <- function(year, driver) {
  v <- c()
  vi <- 1
  for(i in 1:length(driver)) {
    d <- driver[i]
    y <- year[i]

    df <- subset(annual, driver_wiki_name==d & Year == y)
    v[vi] <- nrow(df)>0
    vi <- vi + 1
  }
  return(v)  
}

annual <- data.table(annual)
annual[, contract_renewed:=drove_in_year(Year+1, driver_wiki_name)]
annual[Year==2016, contract_renewed:=NA]

reg7 <- lm(data = annual, formula = as.numeric(contract_renewed) ~ gc_avg_points)
stargazer(reg7, type = "text", report = "vcs*", ci = TRUE, omit.stat = "f")

renew_stat <- annual %>% group_by(Year) %>% summarize( renewals=sum(contract_renewed), dismissals=sum(!contract_renewed))

ggplot(data=renew_stat, aes(x=Year)) +
  geom_bar(aes(y=renewals, fill="renewals"), stat = "identity") +
  geom_bar(aes(y=dismissals, fill="dismissals"), stat = "identity") +
  scale_fill_manual(values=c("red", "navy")) +
  ylab("Count") +
  ggtitle("Count of annual renewals / dismissals") +
  guides(fill=guide_legend(title=NULL))

# Does winning on a specific track increase popularity?

race$winner <- as.numeric(race$position == 1)
race$raceName <- as.factor(race$raceName)

reg8 <- lm(data = race, formula = log_wiki_views ~ raceName * winner)
stargazer(reg8, type = "text", report = "vcs*", ci = TRUE, omit.stat = "f")

levels(race$raceName)
coeffs <- stack(reg8$coefficients[2:length(reg8$coefficients)])

flags <- subset(coeffs, ind != "winner")
flags$country <- c("au", "at", "bh", "be", "br", "gb", "ca", "cn", "eu", "fr", "de", "hu", "in", "it", "jp", "kr", "my", "mx", "mc", "ru", "sg", "es", "tr", "us")

racing <- subset(flags, !endsWith(as.character(ind), ":winner"))
winning <- subset(flags, endsWith(as.character(ind), ":winner"))

flags <- left_join(racing, winning[, c(1, 3)], by="country")
colnames(flags) <- c("racing", "track", "country", "winning")
flags$track <- gsub(x=flags$track, pattern = "raceName(.*)", replacement = "\\1")

abu <- data.frame(0, "Abu Dhabi Grand Prix", "ae", 0)
colnames(abu) <- c("racing", "track", "country", "winning")
flags <- rbind(flags, abu)

flags$racing <- 100 * flags$racing
flags$winning <- 100 * flags$winning


ggplot(data=flags, aes(x=racing, y=winning)) +
  geom_vline(xintercept = 0, colour="orange") +
  geom_hline(yintercept = 0, colour="orange") +
  geom_flag(size=12, aes(country=flags$country)) +
  geom_text(label=flags$country, nudge_y = -10) +
  ggtitle("Popularity changes caused by racing on a track", "Relative to Abu Dhabi [%]")

flags$total_effect <- flags$racing + flags$winning
flags$top4_track <- as.numeric(flags$total_effect > -30)

race <- left_join(race, flags[, c("track", "top4_track")], by=c("raceName" = "track"))

reg_final <- lm(data=race, formula = log_wiki_views ~ winner + current_standing_in_championship + constructor_category + top4_track)
stargazer(reg_final, type = "html", report = "vcs*", ci = TRUE, omit.stat = "f")
