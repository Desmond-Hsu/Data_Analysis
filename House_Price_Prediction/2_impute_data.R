install.packages('DMwR')
require(DMwR)
install.packages('bnstruct')
library(bnstruct)
install.packages('mice')
library(mice)
install.packages('miceadds')
library(miceadds)
train_avm = read.csv('D:/Desmond/Match/E.Sun/HousePrice_prediction/datasets/train_avm.csv')
test_avm = read.csv('D:/Desmond/Match/E.Sun/HousePrice_prediction/datasets/test_avm.csv')

#### Impute village_income_median ####
df1 = train_avm[,c('lat','lon','village','village_income_median')]
df2 = test_avm[,c('lat','lon','village','village_income_median')]
df_vim = rbind(df1,df2)
imputeData <- knnImputation(as.matrix(df_vim),k=10)
imputeData_df = data.frame(imputeData)
#data_clean <- knn.impute(as.matrix(df))
#data_clean_df = data.frame(data_clean)

train_avm$vim_impute = imputeData_df$village_income_median[1:37687]
test_avm$vim_impute = imputeData_df$village_income_median[37688:43988]


#### Impute txn_floor ####
df3 = train_avm[,c('txn_floor','total_floor','building_complete_dt','building_material','building_type','parking_way')]
df4 = test_avm[,c('txn_floor','total_floor','building_complete_dt','building_material','building_type','parking_way')]
df_txnfloor = rbind(df3,df4)
miceMethod <- mice(data=df_txnfloor,method='pmm',m=3,maxit=5)
data1 = complete(miceMethod,1) # 只用前6個變數
data2 = complete(miceMethod,2) # 新增 'highschool_rate','village_income_median','building_area

train_avm$txnfloor_impute1 = data1$txn_floor[1:37687]
test_avm$txnfloor_impute1 = data1$txn_floor[37688:43988]


#train_avm = train_avm[,1:137]
#colnames(train_avm)
#test_avm = test_avm[,1:136]
#colnames(test_avm)


sum(is.na(train_avm$vim_impute))
write.csv(train_avm,file='D:/Desmond/Match/E.Sun/HousePrice_prediction/datasets/train_avm_impute.csv',row.names = FALSE)
write.csv(test_avm,file='D:/Desmond/Match/E.Sun/HousePrice_prediction/datasets/test_avm_impute.csv',row.names = FALSE)
