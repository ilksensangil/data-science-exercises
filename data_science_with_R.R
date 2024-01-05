library(tidyverse)

nba_1_ <- read_csv("C:/Users/pc ssd/Downloads/nba (1).csv")

t(nba_1_[1,])

nba_1_

select(nba_1_,Player)

select(nba_1_,Player,Age)

select(nba_1_, Player, Age, Tm:MP)

select(nba_1_, Player, Age, Tm:MP, -GS)

select(nba_1_, Player, Age, Pos, everything())

select(nba_1_, starts_with("FG"))

select(nba_1_, ends_with("%"))

select(nba_1_, contains("RB"))

nba_1_ <- rename(nba_1_, index = ...1)

nba_1_

filter(nba_1_, Age > 25)

filter(nba_1_, Age > 25, Year > 2010)

filter(nba_1_, Age > 25 | Year > 2010)

filter(nba_1_, Age >= 23, Age <= 30)

filter(nba_1_, between(Age, 23,30))

filter(nba_1_, Pos == "G")

filter(nba_1_, grepl("G", Pos))

filter(nba_1_, grepl("^G", Pos))

filter(nba_1_, grepl("G$", Pos))

filter(nba_1_, grepl("F.G", Pos))

filter(nba_1_, grepl("F..G", Pos))

filter(nba_1_, grepl("F.*G", Pos))

filter(nba_1_, grepl("F.{2}G", Pos))

filter(nba_1_, grepl("F.{2,}G", Pos))

filter(nba_1_, grepl("F.{,2}G", Pos))

filter(nba_1_, grepl("F.{2,4}G", Pos))

filter(nba_1_, grepl("PG|SG", Pos))

filter(nba_1_, grepl("[PS]G", Pos))

filter(nba_1_, !is.na(GS))

mutate(nba_1_, new_col = 1)

mutate(nba_1_, new_col = 1, PPG = PTS / G)

mutate(nba_1_, GS = replace_na(GS, mean(GS, na.rm = TRUE)))

transmute(nba_1_, new_col = 1, PPG = PTS / G)

arrange(nba_1_, Age)

arrange(nba_1_, desc(Age))

arrange(nba_1_, Age, G)

arrange(nba_1_, Age, desc(G))

summarise(nba_1_, meanPTS = mean(PTS, na.rm = TRUE))

df <- group_by(nba_1_, Age, Pos)

summarise(df, meanPTS = mean(PTS, na.rm = TRUE), count = n())

df1 <- select(nba_1_, Player, Age, PTS, G)

df2 <- mutate(df1, PPG = PTS / G)

df3 <- filter(df2, !is.na(PPG))

df4 <- group_by(df3, Age)

summarise(df4, meanPPG = mean(PPG), count = n())

summarise(group_by(filter(mutate(select(nba_1_,
                                        
                                        Player, Age, PTS, G), 
                                 
                                 PPG = PTS / G), 
                          
                          !is.na(PPG)), 
                   
                   Age), 
          
          meanPPG = mean(PPG), count = n())
nba_1_ %>% 
  
  select(Player, Age, PTS, G) %>% 
  
  mutate(PPG = PTS / G) %>% 
  
  filter(!is.na(PPG)) %>% 
  
  group_by(Age) %>% 
  
  summarise(meanPPG = mean(PPG), count = n())