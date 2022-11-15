library(tidyverse)
library(summarytools)
library(naniar)
library(ggthemes)
library(kableExtra)
library(gridExtra)

dfraw = read_csv("Data.csv") %>%
  select(-c("Trash", "Dummy"))
temp2 = dfraw %>%
  select(Rank) %>%
  separate(Rank, c("A", "B", "C", "D", "E", "G")) %>%
  mutate(across(.cols = everything(), .fns = as.numeric))
temp2[is.na(temp2)] = 0
temp2 = temp2 %>%
  mutate(MaxRank = pmax(A, B, C, D, E, G))


# dfSummary(dfraw) %>% view()

df = dfraw %>% 
  mutate(Rank = temp2$MaxRank) %>%
  filter(MbbsYr > 1956 & MbbsYr < 2022) %>%
  mutate(DegCol = str_to_upper(DegCol)) %>%
  mutate(Name = str_to_upper(Name)) %>%
  filter(Rank > 0 & Rank < 100000) %>%
  mutate(Nature = if_else(Nature == "Government", 
                          true = 1, 
                          false = 0)) %>%
  mutate(Designation = ifelse(Designation=="MO",
                              "MEDICAL OFFICER",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="SMO",
                              "SENIOR MEDICAL OFFICER",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="ASMO",
                              "ADDITIONAL SENIOR MEDICAL OFFICER",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="DEMONSTRATOR",
                              "HOUSE SURGEON",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="POST GRADUATE STUDENT",
                              "JUNIOR RESIDENT",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="DEPUTY CIVIL SURGEON",
                              "SENIOR MEDICAL OFFICER",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="JR",
                              "JUNIOR RESIDENT",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="SR",
                              "SENIOR RESIDENT",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="PG STUDENT",
                              "JUNIOR RESIDENT",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="SENIOR PROFESSOR",
                              "PROFESSOR",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="CONSULTANT",
                              "ASSISTANT PROFESSOR",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="POST GRADUATE",
                              "SENIOR RESIDENT",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="PG RESIDENT",
                              "SENIOR RESIDENT",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="M.O",
                              "MEDICAL OFFICER",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="DEPUTY DIRECTOR",
                              "ADMINISTRATIVE OFFICIALS",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="DIRECTOR MER",
                              "ADMINISTRATIVE OFFICIALS",
                              Designation)) %>%
  mutate(Designation = ifelse(Designation=="HOUSE SURGEON",
                              "JUNIOR RESIDENT",
                              Designation))
dfSummary(df) %>% view()

df %>%
  filter(MbbsYr<2016 & MbbsYr!=1977) %>%
  group_by(MbbsYr) %>%
  summarise(AvgRank = mean(Rank)) %>%
  ggplot(aes(x = MbbsYr, y = AvgRank)) +
  geom_smooth(se = FALSE, lwd = 2, col = "lightgrey", alpha=0.2) +
  geom_line(lwd = 1) +
  geom_point(size = 2) +
  theme_clean() + 
  labs(x = "Year of MBBS Admission",
       y = "Average (Mean) National Rank of Doctors",
       title = "Average Rank across years",
       subtitle = "Mean of the ranks of all doctors who joined MBBS in that particular year",
       caption = "Data from Google Forms by DMER. (sample size = 3453)
       Ranks obtained by current faculty in Medical Colleges during their MBBS entrance examination
       Data Analysis in R using tidyverse by Dr.AD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))


df %>%
  filter(MbbsYr<2016 & MbbsYr!=1977) %>%
  group_by(MbbsYr) %>%
  summarise(AvgRank = median(Rank)) %>%
  ggplot(aes(x = MbbsYr, y = AvgRank)) +
  geom_smooth(se = FALSE, lwd = 2, col = "lightgrey", alpha=0.2) +
  geom_line(lwd = 1) +
  geom_point(size = 2) +
  theme_clean() + 
  labs(x = "Year of MBBS Admission",
       y = "Median of National Rank of Doctors",
       title = "Median Rank across years",
       subtitle = "Median of the ranks of all doctors who joined MBBS in that particular year",
       caption = "Data from Google Forms by DMER. (sample size = 3453)
       Ranks obtained by current faculty in Medical Colleges during their MBBS entrance examination
       Data Analysis in R using tidyverse by Dr.AD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

mean(df$Nature)

df %>%
  group_by(MbbsYr) %>%
  summarise(Govt = mean(Nature)) %>%
  filter(MbbsYr<2016) %>%
  mutate(Govt = Govt*100) %>%
  ggplot(aes(x = MbbsYr, y = Govt)) +
  geom_smooth(se = FALSE, lwd = 2, col = "lightgrey", alpha=0.1) +
  geom_line(lwd = 1) +
  geom_point(size = 2) +
  theme_clean() + 
  labs(x = "Year of MBBS Admission",
       y = "Percentage of doctors from Government Colleges",
       title = "Share of Govt. College Graduates in jobs",
       subtitle = "Percentage of all working doctors in Haryana Govt. who graduated from a Govt. Medical College",
       caption = "Data from Google Forms by DMER. (sample size = 3453)
       Ranks obtained by current faculty in Medical Colleges during their MBBS entrance examination
       Data Analysis in R using tidyverse by Dr.AD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,100))

df1 = df %>%
  group_by(Designation) %>%
  summarize(Numbers = n(),
            `Median Rank` = median(Rank),
            `Median Year of MBBS Admission` = median(MbbsYr),
            `Percentage from Govt. Colleges` = mean(Nature)*100) %>%
  filter(Numbers >= 10) %>%
  arrange(desc(Numbers)) %>%
  mutate(Designation = str_to_title(Designation))
df1 %>% kbl(digits = 0) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


df1 %>%
  ggplot(aes(x = `Percentage from Govt. Colleges`,
             y = reorder(Designation, `Percentage from Govt. Colleges`))) +
  geom_bar(stat = "identity",
           width = 0.8, col = "black", fill = "lightgrey") +
  theme_clean() + 
  labs(y = "Current Designation",
       x = "Percentage of doctors from Government Medical Colleges",
       title = "Share of Govt. College Graduates in different positions",
       subtitle = "Percentage of doctors in different posts in Haryana Govt. who graduated from a Govt. Medical College",
       caption = "Data from Google Forms by DMER. (sample size = 3453)
       Ranks obtained by current faculty in Medical Colleges during their MBBS entrance examination
       Data Analysis in R using tidyverse by Dr.AD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))


df1 %>%
  ggplot(aes(x = `Median Rank`,
             y = reorder(Designation, -`Median Rank`))) +
  geom_point(size = 5) +
  theme_clean() + 
  labs(y = "Current Designation",
       x = "Median Rank in MBBS Entrance Examination",
       title = "Median All-India Rank for various posts",
       subtitle = "Median rank in MBBS entrance examination for doctors working on different posts",
       caption = "Data from Google Forms by DMER. (sample size = 3453)
       Ranks obtained by current faculty in Medical Colleges during their MBBS entrance examination
       Data Analysis in R using tidyverse by Dr.AD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

df1
