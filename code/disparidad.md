---
title: "disparidad"
author: "L"
date: "2023-09-27"
github_document: foo.md
---


```{r }
#Paquetes

library(tidyverse)
library(rio)
library(zoo)
library(optimx)

data <- import("C:/Users/luisl/iCloudDrive/r/data_disparidad/data/data.xlsx") 


```


```{r }
#elimnar columnas

data_c <- data %>%
  select_if(~ !all(is.na(.)))

glimpse(data_c)

```


```{r }
#filtrar data
data_c <- data_c %>% slice(2:25)

```

```{r }

# setear nombres de columnas
colnames(data_c) <- c("departmento", "pop_2003", "pop_2012", "nacidovivo_2003", "nacidovivo_2012", "ingresopc_2003", "ingresopc_2012", "mortinf_2003", "mortinf_2012",                     "mortniñez_2003", "mortniñez_2012", "incidtb_2003", "incidtb_2012",
                    "medxhb_2003","medxhb_2012", "esperanzavida_2003", "esperanzavida2012")

glimpse(data_c)

```


```{r }
# formato numerico...
data_c <- data_c %>% mutate(across(-departmento, as.numeric))
glimpse(data_c)
```
## 

```{r }
# Convertir datos a formato largo
data_long <- data_c %>%  
  pivot_longer(
    cols = -departmento, 
    names_to = "variable", 
    values_to = "valor"
  ) %>% 
  separate(variable, into = c('tipo', 'anio'), sep = '_') %>% 
  pivot_wider(names_from = tipo, values_from = valor)

glimpse(data_long)

```


```{r }
#explorar datos
data <- import("C:/Users/luisl/iCloudDrive/r/data_disparidad/data/data.xlsx")
glimpse(data_c)
```