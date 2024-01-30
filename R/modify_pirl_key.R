
library(here)
library(tidyverse)

path <- here::here("data", "PIRL_key.RDA")
load(path)
df <-  as_tibble(PIRL_key)

df <- df |>
  mutate(PIRL_Name = ifelse(Variable_Name == "STATE", "state", PIRL_Name),
         R_Variable = ifelse(Variable_Name == "STATE", "state", R_Variable),
         `PIRL#` = ifelse(`PIRL#` == 3000, 9999, `PIRL#`),
         Data_Element_Name = ifelse(Variable_Name == "STATE", "Reporting State", Data_Element_Name))


saveRDS(df, path)
