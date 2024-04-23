library(tidyverse)

sub_airline <- read.csv("C:/Users/chawl/OneDrive - University of Cincinnati/Prissha/IBM DATA ANALYSIS/lax_to_jfk/lax_to_jfk.csv")
head(sub_airline, 3)
dim(sub_airline)
write_csv(sub_airline, "lax_to_jfk.csv")
sapply(sub_airline, typeof) 
sub_airline %>% 
  filter(Month == 1) %>%
  group_by(Reporting_Airline) %>%
  summarize(avg_carrier_delay = mean(CarrierDelay, na.rm = TRUE))

# group_by / summarize workflow example
sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(avg_carrier_delay = mean(CarrierDelay, na.rm = TRUE)) # use mean value


# group_by / summarise workflow example
sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(sd_carrier_delay = sd(CarrierDelay, na.rm = TRUE)) # use standard deviation

sub_airline %>% 
  group_by(Reporting_Airline) %>%
  summarize(avg_arr_delay = mean(ArrDelay, na.rm = TRUE))

is.na(c(1, NA))        #> FALSE  TRUE
is.na(paste(c(1, NA))) #> FALSE FALSE

anyNA(c(1, NA))   

map(sub_airline, ~sum(is.na(.)))


# counting missing values
sub_airline %>%
  summarize(count = sum(is.na(CarrierDelay)))

# Check dimensions of the dataset
dim(sub_airline)

drop_na_cols <- sub_airline %>% select(-DivDistance, -DivArrDelay)
dim(drop_na_cols)
head(drop_na_cols)

# Drop the missing values
drop_na_rows <- drop_na_cols %>% drop_na(CarrierDelay)
dim(drop_na_rows)
head(drop_na_rows)

# Replace the missing values in five columns
replace_na <- drop_na_rows %>% replace_na(list(CarrierDelay = 0,
                                               WeatherDelay = 0,
                                               NASDelay = 0,
                                               SecurityDelay = 0,
                                               LateAircraftDelay = 0))
head(replace_na)


carrier_mean<- mean(drop_na_rows$CarrierDelay)
cardel_rep_mean <- sub_airline %>%
  replace_na(list(CarrierDelay=carrier_mean))

head(cardel_rep_mean)

# Convert CarrierDelay column from integer to double in sub_airline dataframe
sub_airline$CarrierDelay <- as.double(sub_airline$CarrierDelay)

# Check the structure of the dataframe after conversion
str(sub_airline)


sub_airline %>% 
  summarize_all(class) %>% 
  gather(variable, class)

glimpse(sub_airline)
glimpse(drop_na_rows)


date_airline <- replace_na %>% 
  separate(FlightDate, sep = "-", into = c("year", "month", "day"))

head(date_airline)


library(dplyr)

date_airline %>%
  select(year, month, day) %>%
  mutate_all(type.convert, as.is = TRUE) %>%
  mutate_if(is.character, as.numeric)

simple_scale <- sub_airline$ArrDelay / max(sub_airline$ArrDelay)
head(simple_scale)

simple_scale_2 <- sub_airline$DepDelay / max(sub_airline$DepDelay)
head(simple_scale_2)

minmax_scale <- (sub_airline$ArrDelay - min(sub_airline$ArrDelay)) /
  (max(sub_airline$ArrDelay) - min(sub_airline$ArrDelay))
head(minmax_scale)

z_scale <- (sub_airline$ArrDelay - mean(sub_airline$ArrDelay)) / sd(sub_airline$ArrDelay)
head(z_scale)

z_scale_2<-(sub_airline$DepDelay - mean(sub_airline$DepDelay))/sd(sub_airline$DepDelay)
head(z_scale_2)

ggplot(data = sub_airline, mapping = aes(x = ArrDelay)) +
  geom_histogram(bins = 100, color = "white", fill = "red") +
  coord_cartesian(xlim = c(-73, 682))

binning <- sub_airline %>%
  mutate(quantile_rank = ntile(sub_airline$ArrDelay,4))

head(binning)

ggplot(data = binning, mapping = aes(x = quantile_rank)) +
  geom_histogram(bins = 4, color = "white", fill = "red")

sub_airline %>%
  mutate(dummy = 1) %>% # column with single value
  spread(
    key = Reporting_Airline, # column to spread
    value = dummy,
    fill = 0) %>%
  slice(1:5)

sub_airline %>% 
  spread(Reporting_Airline, ArrDelay) %>% 
  slice(1:5)

sub_airline %>% # start with data
  mutate(Reporting_Airline = factor(Reporting_Airline,
                                    labels = c("AA", "AS", "DL", "UA", "B6", "PA (1)", "HP", "TW", "VX")))%>%
  ggplot(aes(Reporting_Airline)) +
  stat_count(width = 0.5) +
  labs(x = "Number of data points in each airline")

sub_airline %>%
  mutate(dummy = 1) %>% # column with single value
  spread(
    key = Month, # column to spread
    value = dummy,
    fill = 0) %>%
  slice(1:5)

sub_airline %>% 
  spread(Month, DepDelay) %>% 
  slice(1:5)

###lab3

ggplot(data = sub_airline, mapping = aes(x = Reporting_Airline, y = ArrDelay)) +
  geom_boxplot(fill = "bisque",color = "black", alpha = 0.3) +
  geom_jitter(aes(color = 'blue'), alpha=0.2) +
  labs(x = "Airline") +
  ggtitle("Arrival Delays by Airline") +
  guides(color = FALSE) +
  theme_minimal() +
  coord_cartesian(ylim = quantile(sub_airline$ArrDelay, c(0, 0.99)))

# Load Alaska data, deleting rows that have missing departure delay or arrival delay data
alaska_flights <- sub_airline %>%
  filter(Reporting_Airline == "AS") %>%
  filter(!is.na(DepDelay) & !is.na(ArrDelay)) %>%
  filter(DepDelay < 40)

ggplot(data = alaska_flights, mapping = aes(x = DepDelay, y = ArrDelay)) +
  geom_point() +
  ggtitle("Alaska Flight Depature Delays vs Arrival Delays")

# list the data types for each column
str(sub_airline)
class(sub_airline$ArrDelayMinutes)

sub_airline %>%
  cor.test(~DepDelayMinutes+ArrDelayMinutes,data=.)


ggplot(data = sub_airline, mapping = aes(x = WeatherDelay, y = ArrDelayMinutes)) +
  geom_point() + 
  geom_smooth(method = "lm", na.rm = TRUE)

cor(sub_airline$WeatherDelay, sub_airline$ArrDelayMinutes, use = "complete.obs")

cor(sub_airline$CarrierDelay, sub_airline$ArrDelayMinutes)

cor(sub_airline$CarrierDelay, 
    sub_airline$ArrDelayMinutes, 
    use = "complete.obs")

ggplot(data = sub_airline, mapping = aes(x = CarrierDelay, y = ArrDelayMinutes)) +
  geom_point() + 
  geom_smooth(method = "lm", na.rm = TRUE)  



summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(count = n(), 
            mean = mean(ArrDelayMinutes, na.rm = TRUE),
            std_dev = sd(ArrDelayMinutes, na.rm = TRUE), 
            min = min(ArrDelayMinutes, na.rm = TRUE), 
            median = median(ArrDelayMinutes, na.rm=TRUE),
            iqr = IQR(ArrDelayMinutes, na.rm = TRUE), 
            max = max(ArrDelayMinutes, na.rm = TRUE))

summary_airline_delays

sapply(sub_airline, typeof)

sub_airline %>%
  count(Reporting_Airline)

avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarize(mean_delays = mean(ArrDelayMinutes), .groups = 'keep')

head(avg_delays)


sorted <- avg_delays %>% 
  arrange(desc(mean_delays))

head(sorted)

avg_delays %>% 
  ggplot(aes(x = Reporting_Airline, 
             y = DayOfWeek, 
             fill = mean_delays)) +
  # set the tile's borders to be white with size 0.2
  geom_tile(color = "white", size = 0.2) +
  # define gradient color scales
  scale_fill_gradient(low = "yellow", high = "red")





library(lubridate)
# Let's take a simple average across Reporting_Airline and DayOfWeek
avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarize(mean_delays = mean(ArrDelayMinutes), .groups = 'keep') %>%
  # create a new variable "bins" from mean_delays
  # make the first range -0.1 to 0.1 to include zero values
  mutate(bins = cut(mean_delays,breaks = c(-0.1,0.1,10,20,30,50, max(mean_delays)),
                    labels = c("0","0-10","10-20","20-30","30-50",">50"))) %>%
  mutate(bins = factor(as.character(bins),levels = rev(levels(bins))))


ggplot(avg_delays, aes(x = Reporting_Airline, 
                       y = lubridate::wday(DayOfWeek, label = TRUE), 
                       fill = bins)) +
  geom_tile(colour = "white", size = 0.2) +
  geom_text(aes(label = round(mean_delays, 3))) +
  guides(fill = guide_legend(title = "Delays Time Scale"))+
  labs(x = "Reporting Airline",y = "Day of Week",title = "Average Arrival Delays")+
  # Define color palette for the scale
  scale_fill_manual(values = c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4"))


x<-sub_airline %>% 
  group_by(Reporting_Airline) %>%
  summarize(mean_delays=mean(ArrDelayMinutes))

x

sub_airline %>% 
  select(DepDelayMinutes, ArrDelayMinutes) %>% 
  cor(method = "pearson")

sub_airline %>% 
  cor.test(~DepDelayMinutes + ArrDelayMinutes, data = .) 


correlation <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, 
         CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay) %>% 
  cor(use = "pairwise.complete.obs", method = "pearson")

correlation

install.packages("corrplot")

library(corrplot)

numerics_airline <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, CarrierDelay,
         WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay)

airlines_cor <- cor(numerics_airline, method = "pearson", use='pairwise.complete.obs')

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(airlines_cor, method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 45, #Text label color and rotation
)


summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(Average_Delays = mean(ArrDelayMinutes, na.rm = TRUE))

summary_airline_delays %>%  
  ggplot(aes(x = Reporting_Airline, y = Average_Delays)) + 
  geom_bar(stat = "identity") +
  ggtitle("Average Arrival Delays by Airline")

aa_as_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'AS')

ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_as_subset)
summary(ad_aov)

aa_pa_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'PA (1)')

ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_pa_subset)
summary(ad_aov)