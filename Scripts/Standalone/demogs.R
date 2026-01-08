########################################################
###### Collider new exp June 2025 demographics #########
########################################################

dem <- read.csv(here('Data', 'demognew.csv'))

dem <- dem %>% filter(Status == 'APPROVED') # 240

dem$Age <- as.numeric(dem$Age)
print(mean(dem$Age)) #38.2
min(dem$Age) #18
max(dem$Age) #80
sd(dem$Age) # 13.0


sum(dem$Sex == 'Female') # 125
sum(dem$Sex == 'Male') # 114 so other ==1

mean(dem$Time.taken) / 60 # 31.6
sd(dem$Time.taken) / 60 # 13.8
