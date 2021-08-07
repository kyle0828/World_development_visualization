library("dplyr")
library("tidyr")
library("ggplot2")
library("maps")
library("mapproj")
library("RColorBrewer")
library("wbstats")
options(scipen = 999) #prevent scientific notation


### CO2 Emissions by Country ###
top_6_co2_countries <- 
  wb(country = "countries_only", indicator = c("EN.ATM.CO2E.KT"), mrv = 1) %>%
  arrange(-value) %>%
  head(6)

# create the plot
top_6_c02_plot <- ggplot(data = top_6_co2_countries) +
# bar chart
  geom_col(mapping = aes(x = reorder(iso3c, value), y = value)) +
  labs(title = "Top 6 Countries by CO2 Emissions", x = "Country (iso3)", y = top_6_co2_countries$indicator) +
  theme(plot.title = element_text(color="black", size=16, face="bold"))

### India electricity production ###
india_electric <- 
  wb(country = 'IND', indicator = c("EG.ELC.FOSL.ZS", "EG.ELC.RNWX.ZS"),
     mrv = 3, return_wide = TRUE) %>%
  mutate(electric_OGC = EG.ELC.FOSL.ZS, electric_renewable = EG.ELC.RNWX.ZS, other = 100-(EG.ELC.FOSL.ZS+EG.ELC.RNWX.ZS)) %>%
  gather(key= category, value = value, other, electric_renewable, electric_OGC)

### create bar chart ###
india_electric_plot <- 
  ggplot(india_electric, aes(fill=category, y=value, x=date)) + 
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=round(value, 1)), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(title = "India Electricity Production from 2013 to 2015", x = "Year", y= "Percentage") +
  theme(plot.title = element_text(color="black", size=14, face="bold")) + 
  scale_fill_discrete(name= "Source of Production", labels = c("oil, gas, and coal", "renewable", "other"))


### India Income Equality over Time ###
# data wrangling
india_income_years <- 
wb(country = "IND", indicator = c("SI.DST.04TH.20", "SI.DST.10TH.10", "SI.DST.FRST.20"),
   mrv = 20, return_wide = TRUE) %>%
# convert the date column into numeric value
  mutate(date = as.numeric(date)) %>%
# top 10% & bottom 40%
  mutate(wealth_top_10 = SI.DST.10TH.10, wealth_bottom_40 = SI.DST.04TH.20 + SI.DST.FRST.20) %>%
  gather(key= category, value = value, wealth_top_10, wealth_bottom_40)

 ##create line plot##
india_wealth_plot <- ggplot(data = india_income_years) +
  geom_line(mapping = aes(x = date, y = value, color = category))+
  geom_point(mapping = aes(x = date, y = value, color = category))+
  labs(title = "India Wealth Equlity Overtime", x = "Year", y= "Percentage of income held") +
  theme(plot.title = element_text(color="black", size=14, face="bold")) +
  scale_color_discrete(name= "Group", labels = c("Bottom 40% of Pop.", "Top 10% of Pop."))


### Map: Changes in Forestation around the World ###
# data wrangling
forest_area <- 
  wb(country = "countries_only", indicator = c("AG.LND.FRST.ZS"), mrv = 20) %>% 
  spread(key = date, value = value)
  
forest_area <- mutate(forest_area, forest_change = forest_area$`2018` - forest_area$`2000`) %>%
  select(iso3c, country, forest_change) #create a new column of the calculation between 2018 and 2000

world_map <- map_data("world")
world_map <- world_map %>% mutate(iso3c = iso.alpha(region, n=3)) %>%
  select(iso3c, region, group, long, lat)

res_map_df <- left_join(world_map, forest_area, by = "iso3c") %>%
              subset(select = -c(country))
res_map_df <- mutate(res_map_df, forest_change = round(res_map_df$forest_change)) %>%
              mutate(cut_change = cut(res_map_df$forest_change, breaks = c(-Inf, -20, -10, -5, 0, 5, 10, Inf),
                labels = c("<-20%", "-20% to -10%", "-10% to -5%", "-5% to 0%", "0% to 5%", "5% to 10%",">10%")))

#create map#
world_forest<- ggplot(data= res_map_df) +
  geom_polygon(
    mapping = aes(x = long, y = lat, 
                  fill = cut_change, 
                  group = group))

world_forest_plot <- world_forest+
  coord_quickmap() +
  scale_fill_brewer(palette = "RdYlGn", name="Change")+
  theme_bw()+
  theme(axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(), 
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()
        ) +
  labs(title ="Change in Forested Area 2000-2018")


### Japan's aging society ###

population_growth <- wb(country = "JP", indicator = c("SP.POP.0014.TO.ZS", "SP.POP.65UP.TO.ZS"), mrv = 25) %>% 
    select(date, value, indicatorID) %>% 
    spread(key = indicatorID, value = value) %>% 
    rename("Population ages 0-14"="SP.POP.0014.TO.ZS",
           "Population ages 65-"="SP.POP.65UP.TO.ZS") %>% 
    gather(key= indicator, value = value, -date)


 ## create plot ##
population_plot <- ggplot(data = population_growth , mapping = aes(x = date, y = value, color = indicator, group = indicator)) +
  geom_point()+
  geom_line()+
  labs(title = "Toddler and Elder population in Japan", x = "Year", y = "Population (%)") +
  theme(legend.position= c(.05, .95), legend.justification = c("left", "top"), 
        axis.text.x = element_text(face="plain", color="#616161",size= 7, angle=0),
        plot.title = element_text(color="black", size=16, face="bold")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_color_discrete(name= "Age", labels = c("0-14", "65 and above"))