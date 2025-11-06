### Growth, Emissions, and Renewables: A Cross-Country Analysis (1960–2022)"

# clear all variables and values
rm(list=ls()) 

# set working directory
main.wd <- "C:/Users/Nils/Desktop/Uni/Data analytics/projects/r_project"
setwd(main.wd)

# libraries
library(dplyr)
library(fixest)
library(ggplot2)
library(modelsummary)
library(kableExtra)

### import the datasets
# co2 per capita
data_co2_c <- read.csv("co2_pc_data.csv", header=TRUE)

# gdp per capita
data_gdp_c <- read.csv("gdp_per_capita.csv", header=TRUE)

# energy use per capita
data_energy_c <- read.csv("energy_use_per_capita.csv", header=TRUE)

# share consumptions of renewable energy
data_consumption_renewable <- read.csv("share_consumption_renewable_energy.csv", header=TRUE)

# overview 
summary(data_co2_c)
summary(data_gdp_c)
summary(data_energy_c)
summary(data_consumption_renewable)

range(data_co2_c$Year)
range(data_gdp_c$Year)
range(data_energy_c$Year)
range(data_consumption_renewable$Year)

##### data cleaning

colnames(data_co2_c)
colnames(data_gdp_c)
colnames(data_energy_c)
colnames(data_consumption_renewable)

# rename columns
data_co2_c <- data_co2_c %>% rename(Annual_Co2_emissions_pc = Annual.CO..emissions..per.capita.)
data_gdp_c <- data_gdp_c %>% rename(GDP_pc = GDP.per.capita)
data_energy_c <- data_energy_c %>% rename (energy_pc_kwh = Primary.energy.consumption.per.capita..kWh.person.)
data_consumption_renewable <- data_consumption_renewable %>% rename(renewables_share = Renewables....equivalent.primary.energy.)


# Due to data availability, I will restrict my analysis for the range from 1965 until 2022
data_co2_c <- data_co2_c %>% filter(data_co2_c$Year > 1965)
data_gdp_c <- data_gdp_c %>% filter(data_gdp_c$Year > 1965)

# check the entities and their occurence
country_counts_co2 <- data_co2_c %>% 
  group_by(Entity) %>% 
  summarize(count=n()) %>%
  arrange(desc(count))

print(country_counts_co2, n = 231)

country_counts_gdp_c <- data_gdp_c %>% 
  group_by(Entity) %>% 
  summarize(count=n()) %>%
  arrange(desc(count))

print(country_counts_gdp_c, n = 231)

country_counts_energy_c <- data_energy_c %>% 
  group_by(Entity) %>% 
  summarize(count=n()) %>%
  arrange(desc(count))

print(country_counts_energy_c, n = 231)

country_counts_consumption_renewabe <- data_consumption_renewable %>% 
  group_by(Entity) %>% 
  summarize(count=n()) %>%
  arrange(desc(count))

print(country_counts_consumption_renewabe, n = 231)

## delete all the aggregates (e.g. Africa, Europe) from the dataset to deal with country level data only.
## delete all the empty rows, NA and 0

clean_data <- function(df, col) {
  before <- nrow(df)   # count rows before
  df_clean <- df %>%
    filter(Code != "",
           !is.na(.data[[col]]),
           .data[[col]] != "",
           .data[[col]] != 0)
  after <- nrow(df_clean)   
  cat("Dropped", before - after, "rows from", col, "\n")
  return(df_clean)
}

data_co2_c_new <- clean_data(data_co2_c, "Annual_Co2_emissions_pc")
data_gdp_c_new <- clean_data(data_gdp_c, "GDP_pc") %>% select(-"X900793.annotations")
data_energy_c_new <- clean_data(data_energy_c, "energy_pc_kwh")
data_consumption_renewable_new <- clean_data(data_consumption_renewable, "renewables_share")

## join the dataframes together 
# gdp and co2 datasets 
data_gdp_c02 <- data_gdp_c_new %>% 
  left_join(data_co2_c_new, by = c("Code", "Year", "Entity"))

# co2 and energy consumption 
data_co2_energy <- data_co2_c_new %>% 
  left_join(data_energy_c_new, by = c("Code", "Year", "Entity"))

# all 4 datasets together
data_combined <- data_gdp_c_new %>%
  inner_join(data_co2_c_new, by = c("Code", "Year", "Entity")) %>%
  inner_join(data_energy_c_new, by = c("Code", "Year","Entity")) %>%
  inner_join(data_consumption_renewable_new, by = c("Code", "Year", "Entity"))

#### Analysis
# Regression of GDP p.c. on Annual co2 emissions p.c.
r1 <- feols(log(Annual_Co2_emissions_pc) ~ log(GDP_pc),
            data = data_gdp_c02
)
summary(r1)
## highly significant correlation between GDP p.c. and annual co2 emissions p.c.

# Regression with year fixed effects
r2 <- feols(log(Annual_Co2_emissions_pc) ~ log(GDP_pc) | Year,
             data = data_gdp_c02)
summary(r2)
## still highly signficant. Richer countries pollute more

# Regression of annual co2 emissions on Energy consumption
r3 <- feols(log(Annual_Co2_emissions_pc) ~ log(energy_pc_kwh),
            data = data_co2_energy)
summary(r3)

# Regression with year fixed effects
r4 <- feols(log(Annual_Co2_emissions_pc) ~ log(energy_pc_kwh) | Year,
            data = data_co2_energy)
summary(r4)

## plot
# Basic scatter plot of GDP vs CO2
# group the global averages per year to avoid too many data points in my plot

avg_trends <- data_gdp_c02 %>%
  group_by(Year) %>%
              summarise(
                avg_gdp = mean(GDP_pc, na.rm = TRUE),
                avg_co2 = mean(Annual_Co2_emissions_pc, na.rm = TRUE)
              )
              
ggplot(avg_trends, aes(x = `avg_gdp`, y = `avg_co2`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_x_log10() +
  labs(
    x = "GDP per Capita",
    y = "Annual CO emissions per capita",
    title = "GDP per Capita vs CO2 Emissions per Capita"
  ) +
  theme_minimal()


# Regression of share renewables on gdp
r5 <- feols(log(renewables_share) ~ log(GDP_pc), 
            data = data_combined
        )
summary(r5)

# including year fixed effects
r6 <- feols(log(renewables_share) ~  log(GDP_pc) |Year, 
            data = data_combined
)
summary(r6)

## International comparison: Which countries have the highest share of renewable energies?
data_combined_sorted_GDP <- data_combined %>% filter(data_combined$Year == 2022) %>% slice_max(order_by = GDP_pc, n = 20)
View(data_combined_sorted_GDP)

data_combined_sorted_renewable <- data_combined %>% filter(data_combined$Year == 2022) %>% 
  select(Entity, Year, GDP_pc, Annual_Co2_emissions_pc, energy_pc_kwh, renewables_share) %>% 
  slice_max(order_by = renewables_share, n = 20) %>%
  rename(
    Country            = Entity,
    `Year`             = Year,
    `GDP per capita`   = GDP_pc,
    `CO2 emissions (tons per capita)` = Annual_Co2_emissions_pc,
    `Energy use (kWh per capita)`     = energy_pc_kwh,
    `Renewables share (%)`            = renewables_share
  )
View(data_combined_sorted_renewable)


## Specific country-comparison

# check for what countries there is sufficient data
country_counts_combined <- data_combined %>% 
  group_by(Entity) %>% 
  summarize(count=n()) %>%
  arrange(desc(count))

highlighted <- c("China", "Germany", "Colombia")
highlighted_data <- data_combined %>% 
  filter(Entity %in% highlighted)

## China vs. Germany vs. Colombia

# gdp 
ggplot(highlighted_data, aes(x = Year, y = GDP_pc, color = Entity)) +
  geom_line(size = 1.2) +      
  geom_point() +    
  scale_y_log10() +
  labs(
    x = "Year",
    y = "GDP per capita",
    title = "GDP per capita over time (international comparison)",
    color = "Country"
  ) +
  theme_minimal()


# energy consumption
ggplot(highlighted_data, aes(x = Year, y = energy_pc_kwh, color = Entity)) +
  geom_line(size = 1.2) +      # connects points for each country
  geom_point() +               # shows points
  labs(
    x = "Year",
    y = "Energy consumtpion per capita in kwh",
    title = "Energy Consumption (kwh) over time (international comparison)",
    color = "Country"
  ) +
  theme_minimal()

# share renewable energy
ggplot(highlighted_data, aes(x = Year, y = renewables_share, color = Entity)) +
  geom_line(size = 1.2) +      # connects points for each country
  geom_point() +               # shows points
  labs(
    x = "Year",
    y = "Share of renewable Energy",
    title = "Share of renewable Energy over time (international comparison)",
    color = "Country"
  ) +
  theme_minimal()



























