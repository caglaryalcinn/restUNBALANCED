str(data)
vis_miss(data)
gg_miss_var(data)
gg_miss_var(data, facet = attrition_flag)

gg_miss_upset(data)

#Due to its serial nature, we filled the pokedex number of our missing values ​​with a loop, and we filled it with the fill function as of the prayer of generation. We deleted the missing values ​​in the types, name and response variable. We filled the remaining missing values ​​with the rf function of the mice function. We kept our iteration number to a minimum.
```{r}
for(i in 2:length(data$pokedex_number)){
  if(is.na(data$pokedex_number[i]) == TRUE){
    data$pokedex_number[i]=data$pokedex_number[i-1]+1}
}
data_omit <- data %>% drop_na(type.1)
data_omit <- data_omit %>% drop_na(type.2)
data_omit <- data_omit %>% drop_na(legendary)
data_omit <- fill(data_omit, generation)
vis_miss(data_omit)
imputing <- mice(data_omit,m=1,maxit=1,meth = "rf",seed=500)

#When we look at the graphs, we achieved a good result despite our low maxir and m parameters. This success of random forrest minus data filling gave us an idea for our strategy while training our data set.

densityplot(imputing)
xyplot(imputing ,defense ~ total+hp+attack+speed_atk+speed_def,pch=18,cex=1)
data <- complete(imputing,1)

colnames(test)<-str_to_sentence(colnames(test))
colnames(test) <- str_to_lower(colnames(test))
colnames(test)[c(1,2,10,11)] <- c("id","pokedex_number","speed_atk","speed_def")
colnames(test)
test %>% count(type.1)
test %>% count(type.2)
test %>% count(generation)
test <- test %>% mutate(name=str_to_lower(name),
                        type.1=str_to_lower(type.1),
                        type.2=str_to_lower(type.2))

test <- test %>% mutate(name=str_trim(name,side="left"),
                        type.1=str_trim(type.1,side="left"),
                        type.2=str_trim(type.2,side="left"))
test <- test %>% mutate(name=as.factor(name),
                        type.1=as.factor(type.1),
                        type.2=as.factor(type.2),)
test$generation <- as.factor(test$generation)
imputing2 <- mice(test,m=1,maxit=1, meth = "rf" ,seed=500)
summary(imputing2)
densityplot(imputing2)
test<- complete(imputing2,1)
vis_miss(test)