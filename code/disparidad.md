---
title: "disparidad"
author: "L"
date: "2023-09-27"
output: html_document
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




```{r }
inequality_clean_final <- inequality_clean %>% 
  select(year = anio,
         hexp = ingpc, #ingpc, espvid
         ir_tb = mortnin, #inctb, mortinf, mortnin, razmed
         population = nacviv, #pob, nacviv
         country = departamento) %>% 
  mutate(num_tb_cases = ir_tb*population/100000)
```



```{r }

```



```{r }

```


```{r , echo=FALSE}
levels(as.factor(data$year))
levels(data$)

data2 <- data %>% 
  filter(year == 1994 & year == 2012,
         !country %in% c('Suriname', 'Cuba', 'Guyana')) %>% 
  select(year = year,
         country = country,
         population = poblacion_total,
         gdp = GDP_per_capita_PPP_constant_2005_international, 
         gni = GNI_percapita_PPP_constant_2005_international,
         lexp = esperanza_vida_nacer_anos,
         hexp = Health_expenditure_percapita_PPP_constant_2005_international,
         ir_tb = tb_incidence,
         num_tb_cases = num_incid_tb_cases
         ) %>% 
  mutate(id = 1:n())



```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
