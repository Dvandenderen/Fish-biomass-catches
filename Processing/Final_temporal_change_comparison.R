setwd("C:/Users/danie/Documents/Online for git/Fish-biomass-catches/Outputs-by-region/")

year_start <- 1970
year_end <- 2020

library(tidyverse)

#TB temporal change
file_list <- list.files(pattern = "\\.csv$", full.names = TRUE)

df_long <- map_dfr(file_list, ~ {
  read_csv(.x) %>%
    pivot_longer(
      cols = c("Tbio_SUR_MTkm2"),
      names_to = "measure",
      values_to = "value"
    )
})

df_filtered <- df_long %>%
  filter(Year >= year_start, Year <= year_end)

reg <- unique(df_filtered$Region)
TB_slope <- data.frame(Region=reg, TB = NA)

for(j in 1:length(reg)){
  sub_reg <- subset(df_filtered,df_filtered$Region == reg[j])
  mod1 <- (lm(sub_reg$value~sub_reg$Year))
  TB_slope$TB[j] <- (as.numeric(mod1$coefficients[1] + mod1$coefficients[2]*2010) - 
                           as.numeric(mod1$coefficients[1] + mod1$coefficients[2]*2001))
}


# ER temporal change
# Step 1: Load all region CSV files
file_list <- list.files(pattern = "\\.csv$", full.names = TRUE)

# Step 2: Combine and reshape to long format
df_long <- map_dfr(file_list, ~ {
  read_csv(.x) %>%
    pivot_longer(
      cols = c("ER_SAU_SUR", "ER_WAT_SUR", "ER_STO"),
      names_to = "measure",
      values_to = "value"
    )
})

df_filtered <- df_long %>%
  filter(Year >= year_start, Year <= year_end)

reg <- unique(df_filtered$Region)
ER_slope <- data.frame(Region=reg, ER_WAT = NA, ER_SAU=NA,ER_STO=NA)

for(j in 1:length(reg)){
  sub_reg <- subset(df_filtered,df_filtered$Region == reg[j])
  mod1 <- (lm(sub_reg$value[sub_reg$measure == "ER_WAT_SUR"]~
                sub_reg$Year[sub_reg$measure == "ER_WAT_SUR"]))
  ER_slope$ER_WAT[j] <- (as.numeric(mod1$coefficients[1] + mod1$coefficients[2]*10) - 
                           as.numeric(mod1$coefficients[1] + mod1$coefficients[2]*1))
  
  mod2 <-  (lm(sub_reg$value[sub_reg$measure == "ER_SAU_SUR"]~
                 sub_reg$Year[sub_reg$measure == "ER_SAU_SUR"]))
  
  ER_slope$ER_SAU[j] <- (as.numeric(mod2$coefficients[1] + mod2$coefficients[2]*10) - 
                           as.numeric(mod2$coefficients[1] + mod2$coefficients[2]*1))
  if(!(is.na(sub_reg$value[sub_reg$measure == "ER_STO"][1]))){
    mod3 <-  (lm(sub_reg$value[sub_reg$measure == "ER_STO"]~
                   sub_reg$Year[sub_reg$measure == "ER_STO"]))
    ER_slope$ER_STO[j] <- 
      (as.numeric(mod3$coefficients[1] + mod3$coefficients[2]*10) - 
         as.numeric(mod3$coefficients[1] + mod3$coefficients[2]*1))
  }
}

TB_slope$TB <- round(TB_slope$TB,digits = 2)
ER_slope[,2:4]<- round(ER_slope[,2:4],digits = 2)

tot <- cbind(TB_slope,ER_slope[,2:4])
write.csv(tot,"../Outputs/temporal_change.csv",row.names = F)
