#cars
data = read.csv("./../data/vehicles.csv", sep=",")
View(data)
names(data)
hist(data$price)
ncol(data)#[1] 25
nrow(data)#[1] 509577

# Check for missing values
rowSums(is.na(data))
colSums(is.na(data))
sum(which((rowSums(is.na(data))/ncol(data))>0.5)) # 0
(colSums(is.na(data))/nrow(data))>0.5 # 1
sum(which((colSums(is.na(data))/nrow(data))>0.5)) # 22 of county

#Remove useless data
pairs(data[1:400,c("price","id","url","region_url","image_url","lat","long", "vin")])
data$id = NULL
data$url = NULL
data$region_url = NULL
data$image_url = NULL
data$county = NULL #empty
data$description = NULL
data$lat = NULL
data$long = NULL
data$vin = NULL

View(data)

str(data)
for(i in seq(1, ncol(data))){
  print(i)
  if(is.factor(data[,i])){
    print(levels(data[,i]))
  }
}
#Update "" factor level with NA
levels(data$manufacturer)[1] = NA
levels(data$fuel)[1] = NA
levels(data$model)[1] = NA
levels(data$drive)[1] = NA
levels(data$title_status)[1] = NA
levels(data$transmission)[1] = NA
levels(data$condition)[1] = NA
levels(data$cylinders)[1] = NA
levels(data$size)[1] = NA
levels(data$type)[1] = NA
levels(data$paint_color)[1] = NA
View(data)

data = na.omit(data)
data = data[data$price!=0, ]


write.csv(data, "cleaned-cars.csv", row.names=F)
#write.table(data, 'cleaned-cars.csv', row.names=F, sep=",")

# 
# 
# # ncol(data)#[1] 16
# # nrow(data)#[1] 509577
# # write.table(data, 'cleaned-cars.csv', row.names=F, sep=",")
# # data = read.csv("cleaned-cars.csv", sep=",")
# # ncol(data)#[1] 16
# # nrow(data)#[1] 269297
# 
# 
# #Remove missing values
# #Remove columns with more than 1/2 rows missing
# colSums(is.na(data)) # Number of missing per column/variable
# range(colSums(is.na(data))) # [1]     19 179012
# which((colSums(is.na(data))/nrow(data))>0.5)#[1] size 13
# 
# names(data)
# # [1] "region"       "price"        "year"        
# # [4] "manufacturer" "model"        "condition"   
# # [7] "cylinders"    "fuel"         "odometer"    
# # [10] "title_status" "transmission" "drive"       
# # [13] "size"         "type"         "paint_color" 
# # [16] "state"  
# data = data[ , -which((colSums(is.na(data))/nrow(data))>0.5)] #removed size
# names(data)
# # [1] "region"       "price"        "year"        
# # [4] "manufacturer" "model"        "condition"   
# # [7] "cylinders"    "fuel"         "odometer"    
# # [10] "title_status" "transmission" "drive"       
# # [13] "type"         "paint_color"  "state" 
# 
# #Remove rows with more than 1/2 columns missing
# rowSums(is.na(data)) # Number of missing per row
# range(rowSums(is.na(data))) # 0 to 12
# which((rowSums(is.na(data))/ncol(data))>0.5)
# nrow(data) #[1] 509577
# data = data[ -which((rowSums(is.na(data))/ncol(data))>0.5), ]
# nrow(data)#[1] 509403
# 
# ncol(data)#[1] 15
# View(data)
# 
# range(rowSums(is.na(data))) # 0 to 7
# range((rowSums(is.na(data))/ncol(data)))#[1] 0.0000000 0.4666667
# data = data[ -which((rowSums(is.na(data)))>6), ]
# range((rowSums(is.na(data))/ncol(data))) #[1] 0.0000000 0.4
# 
# nrow(data)#[1] 503078
# ncol(data)#[1] 15
# View(data)
# 
# # write.table(data, 'cleaned-cars2.csv', row.names=F, sep=",")
# # data = read.csv("cleaned-cars2.csv", sep=",")
# 
# 
# data = data[ -which((rowSums(is.na(data)))>5), ]
# range((rowSums(is.na(data))/ncol(data))) #[1] 0.0000000 0.3333333
# 
# nrow(data)#[1] 462985
# ncol(data)#[1] 15
# View(data)
# 
# write.table(data, 'cleaned-cars3.csv', row.names=F, sep=",")
# data = read.csv("cleaned-cars3.csv", sep=",")
