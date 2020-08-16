library(ggplot2)
library(jsonlite)
library(blsAPI)
library(tidyverse)
library(Cairo)
library(lubridate)
library(scales)
library(imager)
library(openxlsx)

setwd("C:\\Users\\Owner\\Documents\\GitHub\\veteranunemployment")

source("registrationkey.R")

# Import Data from BLS API
df.tmp1 <- blsAPI(list(
  'startyear' = 2000,
  'endyear' = 2010,
  'registrationKey'= apikey,
  'seriesid' = c('LNU04049526', 'LNU04066408','LNU04049601')
), 2, TRUE)

df.tmp2 <- blsAPI(list(
  'startyear' = 2011,
  'endyear' = 2020,
  'registrationKey'= apikey,
  'seriesid' = c('LNU04049526', 'LNU04066408','LNU04049601')
), 2, TRUE)

df.raw <- full_join(df.tmp1,df.tmp2)
rm(df.tmp1,df.tmp2)

# Separate out Monthly and Annual Datasets
df <- df.raw 

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

# Min and Max Unemployment Values for Post-9/11 Veterans
min.p911 <- df %>%
  filter(vettype == 'Post-9/11 Veterans') %>%
  slice(which.min(value))

max.p911 <- df %>%
  filter(vettype == 'Post-9/11 Veterans') %>%
  slice(which.max(value))

min.lab <- paste0("Lowest ", substr(min.p911$yearperiod,1,7),": ", percent(min.p911$value, accuracy = .1))
max.lab <- paste0("Highest ", substr(max.p911$yearperiod,1,7),": ", percent(max.p911$value, accuracy = .1))

# Color palette for veteran and nonveteran groups ---
palette <- c("Post-9/11 Veterans" = "#E69F00", 
             "Nonveterans" = "#999999",
             "All Veterans" = "#56B4E9"
             )

# Plotting in ggplot ---
p <- ggplot(df, aes(x=yearperiod, y=value, group=vettype))
p <- p + annotate("text", x = as.Date("2002-04-01"), y = .01, label = "@AJThurston", color = "#DDDDDD", size = 4)
p <- p + annotate("text", x = as.Date("2019-04-01"), y = .017, label = min.lab, color = "#E69F00", vjust = 1.5, hjust = .75, size = 3)
p <- p + annotate("text", x = as.Date("2011-01-01"), y = .152, label = max.lab, color = "#E69F00", vjust = -2, size = 3)
p <- p + labs(title = "Veteran and Nonveteran Unemployment rate",
              subtitle = "Not Seasonally Adjusted (2000-2020)", 
              caption = "Source: Bureau of Labor Statistics Current Population Survey (retrieved: 2020-08-16) 
           (Unadj) Unemployment Rate - Veterans, Gulf War Era II, 18 years and over: data.bls.gov/timeseries/LNU04066408
           (Unadj) Unemployment Rate - Nonveterans, 18 years and over: data.bls.gov/timeseries/LNU04049601
           (Unadj) Unemployment Rate - Total veterans, 18 years and over: data.bls.gov/timeseries/LNU04049526")
p <- p + geom_point(aes(color=vettype), size = .5)
p <- p + stat_smooth(aes(color=vettype, fill = vettype), method = "lm", formula = y ~ poly(x, 8), se = TRUE)
p <- p + scale_y_continuous(labels = scales::percent, expand = c(0,0), limits = c(0,.2), breaks = seq(.00,.20,.02))
p <- p + scale_x_date(limits = c(min(df$yearperiod),max(df$yearperiod)), date_breaks = "2 year", date_labels = "%Y")
p <- p + scale_colour_manual(values = palette)
p <- p + scale_fill_manual(values = palette)
p <- p + theme(
  panel.background = element_blank(),
  panel.grid.major.y = element_line(color = "#DDDDDD"),
  panel.grid.major.x = element_blank(),
  axis.ticks = element_line(color = "#DDDDDD"),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  legend.key=element_blank(),
  legend.title = element_blank(),
  legend.position = c(.2,.8),
  legend.text=element_text(size=rel(.75)),
  axis.text.x = element_text(color = "#000000"),
  axis.text.y = element_text(color = "#000000"),
  plot.caption = element_text(size = rel(.50)),
  plot.subtitle = element_text(size = rel(.75))
)

# ggsave as Cairo-png for anti-aliased raster ----
ggsave("C:\\Users\\Owner\\Documents\\GitHub\\veteranunemployment\\veteranunemployment.png", 
       plot = p,
       scale = .65,
       width = 8,
       height = 6,
       units = "in",
       dpi = 300,
       type = "cairo-png")

# View raster image in Rstudio using imager package ---
par(mar=c(0,0,0,0))
plot(load.image("veteranunemployment.png"))
