rm(list=ls())

library(tidyr)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

flows <- read_csv('UN_MigFlow_Totals.csv')
flows_caus <- flows[flows$cntname %in% c('United States of America', 'Canada'),]
flows_caus <- flows_caus[flows_caus$coverage != 'Citizens',]

pop <- read_csv('pop_us_can.csv')
colnames(pop)[colnames(pop)=="Year"] <- "year"
pop<- gather(data = pop, key = 'cntname', value = 'population', 2:3)

ggplot(flows_caus, aes(x = year, y = num, col = cntname)) +
  geom_line()

flows_caus <- merge(flows_caus, pop, by = c("year", "cntname"))
flows_caus$rate <- (flows_caus$num / flows_caus$population)*100

ggplot(flows_caus, aes(x = year, y = rate, col = cntname)) +
  geom_line()

flows_caus <- flows_caus %>%
  arrange(cntname, year) %>%
  group_by(cntname) %>%
  mutate(diff = rate - lag(rate),
         diff_raw = num - lag(num))

canada_pre2010 <- na.omit(flows_caus$diff[(flows_caus$year <= 2009 & flows_caus$cntname == "Canada")])
canada_post2010 <- flows_caus$diff[(flows_caus$year > 2009 & flows_caus$cntname == "Canada")]
us_pre2010 <- na.omit(flows_caus$diff[(flows_caus$year <= 2009 & flows_caus$cntname == "United States of America")])
us_post2010 <- flows_caus$diff[(flows_caus$year > 2009 & flows_caus$cntname == "United States of America")]

ca_pre2010raw <- na.omit(flows_caus$diff_raw[(flows_caus$year <= 2009 & flows_caus$cntname == "Canada")])
ca_post2010raw <- na.omit(flows_caus$diff_raw[(flows_caus$year > 2009 & flows_caus$cntname == "Canada")])
us_pre2010raw <- na.omit(flows_caus$diff_raw[(flows_caus$year <= 2009 & flows_caus$cntname == "United States of America")])
us_post2010raw <- na.omit(flows_caus$diff_raw[(flows_caus$year > 2009 & flows_caus$cntname == "United States of America")])

cor_pre2010raw <- cor(ca_pre2010raw, us_pre2010raw)
cor_post2010raw <- cor(ca_post2010raw, us_post2010raw)

cor_pre2010 <- cor(canada_pre2010, us_pre2010)
cor_post2010 <- cor(canada_post2010, us_post2010)

for (i in 1984:2013) {
  name <- paste0("cor", i)
  tempca <- flows_caus %>%
    filter(year <= i & year >= i-3) %>%
    filter(cntname == "Canada")
  tempus <- flows_caus %>%
    filter(year <= i & year >= i-3) %>%
    filter(cntname == "United States of America")
  tempca <- tempca$diff
  tempus <- tempus$diff
  assign(name, cor(tempca, tempus))
}

flows_caus_pre <- flows_caus %>%
  filter(year <= 2009)
flows_caus_post <- flows_caus %>%
  filter(year >= 2010)

ggplot(flows_caus_pre, aes(x = year, y = rate, col = cntname)) +
  geom_line()
ggplot(flows_caus_post, aes(x = year, y = rate, col = cntname)) +
  geom_line()
