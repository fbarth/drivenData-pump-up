# aquisicao

trainValues <- read.csv("data/trainingSetValues.csv")
trainLabels <- read.csv("data/trainingSetLabels.csv")
train <- merge(trainValues, trainLabels, by="id")

# pre-processamento

dataset <- train
dataset$id <- NULL
dataset$longitude <- NULL
dataset$latitude <- NULL
dataset$date_recorded <- NULL
dataset$funder <- NULL
dataset$installer <- NULL
dataset$wpt_name <- NULL
dataset$num_private <- NULL
dataset$subvillage <- NULL
dataset$region_code <- NULL
dataset$district_code <- NULL
dataset$lga <- NULL
dataset$ward <- NULL

dataset$scheme_management <- as.factor(
  ifelse(dataset$scheme_management == '',
         NA,
         paste(dataset$scheme_management)
         )
)

dataset$scheme_management <- as.factor(
  ifelse(dataset$scheme_management == 'None',
         NA,
         paste(dataset$scheme_management)
  )
)

levels(dataset$scheme_name)

dataset$scheme_name <- NULL

dataset$permit <- as.factor(
  ifelse(dataset$permit == '',
         NA,
         paste(dataset$permit))
)

dataset$construction_year <-
  ifelse(dataset$construction_year == 0,
         NA,
         dataset$construction_year)
  
dataset$construction_year <- NULL
dataset$extraction_type_group <- NULL
dataset$management <- NULL
dataset$management_group <- NULL
dataset$payment <- NULL
dataset$payment_type <- NULL

dataset$water_quality <- as.factor(
  ifelse(dataset$water_quality == 'unknown',
         NA,
         paste(dataset$water_quality))
)

dataset$quality_group <- as.factor(
  ifelse(dataset$quality_group == 'unknown',
         NA,
         paste(dataset$quality_group))
)

dataset$quantity <- as.factor(
  ifelse(dataset$quantity == 'unknown',
         NA,
         paste(dataset$quantity))
)
dataset$quantity_group <- NULL

dataset$source <- as.factor(
  ifelse(dataset$source == 'unknown',
         NA,
         paste(dataset$source))
)

dataset$source_class <- as.factor(
  ifelse(dataset$source_class == 'unknown',
         NA,
         paste(dataset$source_class))
)

temp <- dataset[complete.cases(dataset), ]

# modelagem
  
library(randomForest)
model <- randomForest(status_group ~ ., 
                      data =  temp,
                      do.trace=100,
                      importance=TRUE)

formula <- status_group ~ quantity + region + scheme_management + amount_tsh + gps_height + permit + population

model2 <- randomForest(formula , 
                      ntree = 200,
                      data =  temp,
                      do.trace=100,
                      importance=TRUE)

# aplicando a funcao
test <- read.csv("data/testSetValues.csv")

dataset <- test
#dataset$id <- NULL
dataset$longitude <- NULL
dataset$latitude <- NULL
dataset$date_recorded <- NULL
dataset$funder <- NULL
dataset$installer <- NULL
dataset$wpt_name <- NULL
dataset$num_private <- NULL
dataset$subvillage <- NULL
dataset$region_code <- NULL
dataset$district_code <- NULL
dataset$lga <- NULL
dataset$ward <- NULL

dataset$scheme_management <- as.factor(
  ifelse(dataset$scheme_management == '',
         NA,
         paste(dataset$scheme_management)
  )
)

dataset$scheme_management <- as.factor(
  ifelse(dataset$scheme_management == 'None',
         NA,
         paste(dataset$scheme_management)
  )
)

levels(dataset$scheme_name)

dataset$scheme_name <- NULL

dataset$permit <- as.factor(
  ifelse(dataset$permit == '',
         NA,
         paste(dataset$permit))
)

dataset$construction_year <-
  ifelse(dataset$construction_year == 0,
         NA,
         dataset$construction_year)

dataset$construction_year <- NULL
dataset$extraction_type_group <- NULL
dataset$management <- NULL
dataset$management_group <- NULL
dataset$payment <- NULL
dataset$payment_type <- NULL

dataset$water_quality <- as.factor(
  ifelse(dataset$water_quality == 'unknown',
         NA,
         paste(dataset$water_quality))
)

dataset$quality_group <- as.factor(
  ifelse(dataset$quality_group == 'unknown',
         NA,
         paste(dataset$quality_group))
)

dataset$quantity <- as.factor(
  ifelse(dataset$quantity == 'unknown',
         NA,
         paste(dataset$quantity))
)
dataset$quantity_group <- NULL

dataset$source <- as.factor(
  ifelse(dataset$source == 'unknown',
         NA,
         paste(dataset$source))
)

dataset$source_class <- as.factor(
  ifelse(dataset$source_class == 'unknown',
         NA,
         paste(dataset$source_class))
)

dataset$status_predicted <- predict(
  model2, dataset
)

dataset$status_predicted <- as.factor(
  ifelse(is.na(dataset$status_predicted),
         "functional",
         paste(dataset$status_predicted))
)

submeter <- dataset[, c('id','status_predicted')]
View(submeter)
names(submeter) <- c('id','status_group')
write.csv(submeter, 
          "results/results_1.csv", 
          row.names = FALSE)

