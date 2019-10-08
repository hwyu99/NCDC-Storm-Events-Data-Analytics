require(ggplot2)
require(dplyr)
require(RMySQL)
require(DBI)

StormEvents <- readr::read_csv(
  "G:/StormEvents_details-ftp_v1.0_d2018_c20181017.csv",
  col_names = TRUE)

glimpse(StormEvents)

StormEvents$EVENT_TYPE %>% 
  unique() %>% 
  sort()

FW_SE <- StormEvents %>% 
  filter(EVENT_TYPE %in% "Flood" |
           EVENT_TYPE %in% "Wildfire") %>% 
  mutate(EVENT_TYPE = as.factor(EVENT_TYPE)) %>% 
  select(EVENT_TYPE, MONTH_NAME)

FW_SE$MONTH_NAME %>% 
  unique()

#Scatterplot
FW_SE$MONTH_NAME <- as.factor(FW_SE$MONTH_NAME)
levels(FW_SE$MONTH_NAME) <- c("January", "February", "March",
                              "April", "May", "June", "July")
FW_SE %>% 
  group_by(MONTH_NAME) %>% 
  count(EVENT_TYPE) %>% 
  rename(Count = n) %>% 
  mutate(Color = c("#1874CD")) %>% 
  ggplot() +
  geom_point(aes(x = MONTH_NAME, y = Count, 
                 shape = EVENT_TYPE, color = Color),
             stroke = 2.0) +
  scale_shape_manual(values = rep(c(15, 4), times = 7)) +
  scale_color_identity() + 
  labs(x = "Month Name", shape = "Event") +
  theme(
    legend.key = element_rect(colour = NA, fill = NA),
    legend.position = c(0.14, 0.77),
    legend.background = element_rect(fill = "#EDEDED", color = "#CFCFCF"),
    legend.text.align = 1,
    legend.title.align = 1,
    panel.grid.minor = element_blank()
  )


#Boxplot
FT_SE <- StormEvents %>% 
  filter(EVENT_TYPE %in% "Flood" |
           EVENT_TYPE %in% "Tornado") %>% 
  mutate(EVENT_TYPE = as.factor(EVENT_TYPE)) %>% 
  mutate(DurationDays = END_DAY - BEGIN_DAY) %>% 
  select(EVENT_TYPE, DurationDays); glimpse(FT_SE)

bp_Outline <- function(lwd = 2) {
  windowsFonts(
    A = windowsFont("Arial Black")
  )
  op <- par(lwd = lwd, las = 1, family = "A") # 
  boxplot(DurationDays ~ EVENT_TYPE,
          data =  FT_SE,
          outline = TRUE)
  rect(par("usr")[1], par("usr")[3], 
       par("usr")[2], par("usr")[4], 
       col = "#DBDBDB")
  boxplot(DurationDays ~ EVENT_TYPE, 
          data =  FT_SE,
          outline = TRUE, 
          whisklty = 1,
          col = "#8C8C8C",
          main = "Days of Duration for Flood and Tornado",
          ylab = "Days of Duration",
          add = TRUE)
  
  par(op)
}
bp_Outline(lwd = 2)


bp_NoOut <- function(lwd = 2) {
  windowsFonts(
    A = windowsFont("Arial Black")
  )
  op <- par(lwd = lwd, las = 1, family = "A") # 
  boxplot(DurationDays ~ EVENT_TYPE,
          data =  FT_SE,
          outline = FALSE)
  rect(par("usr")[1], par("usr")[3], 
       par("usr")[2], par("usr")[4],
       col = "#DBDBDB")
  boxplot(DurationDays ~ EVENT_TYPE, 
          data =  FT_SE,
          outline = FALSE, 
          whisklty = 1,
          col = "#8C8C8C",
          main = "Days of Duration for Flood and Tornado",
          ylab = "Days of Duration",
          add = TRUE)
  
  par(op)
}
bp_NoOut(lwd = 2)

#Correlation
StormEvents$MONTH_NAME %>% 
  unique()
StormEvents$DAMAGE_PROPERTY %>% 
  unique()
StormEvents$DAMAGE_CROPS %>% 
  unique() 
SE_corr <- StormEvents[, c("MONTH_NAME", 
                           "INJURIES_DIRECT", "INJURIES_INDIRECT",
                           "DEATHS_DIRECT", "DEATHS_INDIRECT",
                           "DAMAGE_PROPERTY", "DAMAGE_CROPS")] %>% 
  filter(!is.na(DAMAGE_PROPERTY) & !is.na(DAMAGE_CROPS)) %>% 
  mutate(magnitude_PROPERTY = case_when(
    stringr::str_detect(DAMAGE_PROPERTY, "K") ~ 1000,
    stringr::str_detect(DAMAGE_PROPERTY, "M") ~ 1000000,
    stringr::str_detect(DAMAGE_PROPERTY, "B") ~ 1000000000
  )) %>% 
  mutate(magnitude_CROPS = case_when(
    stringr::str_detect(DAMAGE_CROPS, "K") ~ 1000,
    stringr::str_detect(DAMAGE_CROPS, "M") ~ 1000000,
    stringr::str_detect(DAMAGE_CROPS, "B") ~ 1000000000
  )) %>% 
  mutate(DAMAGE_PROPERTY = stringr::str_replace_all(DAMAGE_PROPERTY, "[KMB]", "") %>% 
           as.numeric(),
         DAMAGE_CROPS = stringr::str_replace_all(DAMAGE_CROPS, "[KMB]", "") %>% 
           as.numeric()) %>% 
  mutate(DAMAGE_PROPERTY = DAMAGE_PROPERTY * magnitude_PROPERTY,
         DAMAGE_CROPS = DAMAGE_CROPS * magnitude_CROPS) %>% 
  mutate(MONTH = case_when(
    MONTH_NAME == "January" ~ 1,
    MONTH_NAME == "February" ~ 2,
    MONTH_NAME == "March" ~ 3,
    MONTH_NAME == "April" ~ 4,
    MONTH_NAME == "May" ~ 5,
    MONTH_NAME == "June" ~ 6,
    MONTH_NAME == "July" ~ 7
  )) %>% 
  select(MONTH, 
         INJURIES_DIRECT, INJURIES_INDIRECT,
         DEATHS_DIRECT, DEATHS_INDIRECT,
         DAMAGE_PROPERTY, DAMAGE_CROPS)
glimpse(SE_corr)
cor(SE_corr)
col <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                          "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                          "#4393C3", "#2166AC", "#053061"))
corrplot::corrplot(cor(SE_corr), 
                   type = "lower", 
                   tl.col = "black", 
                   tl.srt = 50,
                   tl.cex = 0.8,
                   col = rev(col(200)))

#Regression
SE_reg <- StormEvents %>% 
  filter(EVENT_TYPE %in% "Tornado") %>% 
  mutate(Flood_time = (as.POSIXct(END_DATE_TIME, format = "%m/%d/%Y %H:%M") - 
                         as.POSIXct(BEGIN_DATE_TIME, format = "%m/%d/%Y %H:%M")) %>% 
           as.numeric()) %>% 
  select(EVENT_TYPE, Flood_time, INJURIES_DIRECT)
glimpse(SE_reg)

SE_reg$INJURIES_DIRECT %>% 
  unique()

SE_reg$Flood_time %>% 
  unique() %>% 
  head()

mod <- lm(INJURIES_DIRECT ~ Flood_time, data = SE_reg)
summary(mod)

SE_reg %>% 
  ggplot() +
  geom_point(aes(x = Flood_time, y = INJURIES_DIRECT),
             color = "red") +
  geom_abline(slope = mod[["coefficients"]]['Flood_time'],
              intercept = mod[["coefficients"]]['(Intercept)'],
              color = 'blue',
              size = 1.0) + 
  geom_segment(aes(x = Flood_time, y = INJURIES_DIRECT,
                   xend = Flood_time, yend = mod[['fitted.values']]),
               color = "gray") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(color = "black", fill = NA))

#Hypothesis test
OHIO <- StormEvents %>% 
  filter(STATE %in% 'OHIO') %>% 
  filter(EVENT_TYPE %in% c("Flood", "Thunderstorm Wind"))

head(OHIO[, c("STATE", "MONTH_NAME", "EVENT_TYPE")])

table(OHIO$MONTH_NAME, OHIO$EVENT_TYPE)

OHIO <- OHIO %>% 
  mutate(MONTH_NAME = factor(MONTH_NAME),
         EVENT_TYPE = factor(EVENT_TYPE))
chisq.test(y = OHIO$EVENT_TYPE, x = OHIO$MONTH_NAME)[['p.value']]











