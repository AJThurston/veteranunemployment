# Libraries
library(ggplot2)
library(jsonlite)
library(blsAPI)
library(tidyverse)
library(Cairo)
library(lubridate)
library(scales)
library(imager)

# Import Data from BLS API
df.raw <- blsAPI(list(
  'startyear' = 2000,
  'endyear' = 2020,
  'annualaverage'=TRUE,
  'registrationKey'= apikey,
  'seriesid' = c('LNU04049526', 'LNU04066408','LNU04049601')
), 2, TRUE)

# Separate out Monthly and Annual Datasets
df <- df.raw %>% 
  filter(periodName != "Annual")

df.annual <- df.raw %>% 
  filter(periodName == "Annual")

# Variable Recode ----
df$value <- as.numeric(df$value)/100
df$yearperiod <- paste0(df$year,df$period,"-01")
df$yearperiod <- gsub("M", "-", df$yearperiod)
df$yearperiod <- ymd(df$yearperiod)
df$vettype <- recode_factor(df$seriesID, 
                     "LNU04066408" = "Post-9/11 Veterans",
                     "LNU04049601" = "Nonveterans",
                     "LNU04049526" = "All Veterans", 
                      .ordered = TRUE)

# Color palette for veteran and nonveteran groups ---
palette <- c("Post-9/11 Veterans" = "#E69F00", 
             "Nonveterans" = "#999999",
             "All Veterans" = "#56B4E9"
             )

# Plotting in ggplot ---
p1 <- ggplot(df, aes(x=yearperiod, y=value, group=vettype)) +
      annotate("text", x = as.Date("2003-06-01"), y = .0725, label = "@AJThurston", color = "#DDDDDD", size = 4) +
      annotate("text", x = as.Date("2019-04-01"), y = .017, label = "Lowest 2019-04: 1.7%", color = "#E69F00", size = 2.5, vjust = 2, hjust = .75) +
      annotate("text", x = as.Date("2011-01-01"), y = .152, label = "Highest 2011-01: 15.2%", color = "#E69F00", size = 2.5, vjust = -.5) +
      labs(title = "Veteran and Nonveteran Unemployment rate",
           subtitle = "Not Seasonally Adjusted (2000-2020)", 
           caption = "Source: Bureau of Labor Statistics Current Population Survey (retrieved: 2020-03-06) 
           (Unadj) Unemployment Rate - Veterans, Gulf War Era II, 18 years and over: data.bls.gov/timeseries/LNU04066408
           (Unadj) Unemployment Rate - Nonveterans, 18 years and over: data.bls.gov/timeseries/LNU04049601
           (Unadj) Unemployment Rate - Total veterans, 18 years and over: data.bls.gov/timeseries/LNU04049526") +
      geom_point(aes(color=vettype), size = .5) +
      stat_smooth(aes(color=vettype, fill = vettype), method = "lm", formula = y ~ poly(x, 8), se = TRUE) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0), limits = c(0,.16), breaks = seq(.00,.16,.02)) +
      scale_x_date(limits = c(as.Date("2000-01-01"),as.Date("2020-01-01")), date_breaks = "2 year", date_labels = "%Y") +
      scale_colour_manual(values = palette) +
      scale_fill_manual(values = palette) +
      theme(
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "#DDDDDD"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_line(color = "#DDDDDD"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.position = c(.85,.75),
        legend.text=element_text(size=rel(.75)),
        axis.text.x = element_text(color = "#000000"),
        axis.text.y = element_text(color = "#000000"),
        plot.caption = element_text(size = rel(.50)),
        plot.subtitle = element_text(size = rel(.75))
      )
p1

# ggsave as Cairo-png for anti-aliased raster ----
ggsave("C:\\Users\\Owner\\Documents\\GitHub\\veteranunemployment\\veteranunemployment.png", 
       plot = p1,
       scale = .75,
       width = 8,
       height = 6,
       units = "in",
       dpi = 300,
       type = "cairo-png")

ggsave("C:\\Users\\Owner\\Documents\\GitHub\\veteranunemployment\\veteranunemployment.twitter.png", 
       plot = p1,
       scale = 1,
       width = 8,
       height = 4,
       units = "in",
       dpi = 300,
       type = "cairo-png")

# View raster image in Rstudio using imager package ---
par(mar=c(0,0,0,0))
plot(load.image("C:\\Users\\Owner\\Documents\\GitHub\\veteranunemployment\\veteranunemployment.png"))
plot(load.image("C:\\Users\\Owner\\Documents\\GitHub\\veteranunemployment\\veteranunemployment.twitter.png"))
