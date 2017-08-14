#從sqlite3將資料拉到R裡面
#connet to sqlite3
install.packages("RSQLite")
install.packages("sqldf")
install.packages("gsubfn")
install.packages("proto")
library("RSQLite")
library("DBI")
library("sqldf")
# connect to the sqlite file
conn = dbConnect(RSQLite::SQLite(), dbname="yahoo_new_car0708.sqlite3")
# get a list of all tables
alltables = dbListTables(conn)
# get the populationtable as a data.frame
car = dbGetQuery( conn,'select * from yahooNewCars_clean2' )
#Clear the results of the last query
#dbClearResult(car)

#open thread
#import library
#install.packages(RevoUtilsMath)
library(RevoUtilsMath)
#set multiThread = 4
setMKLthreads(4)
#config multThread = 4
getMKLthreads()
memory.size(max=32710)


#資料型態轉換
car$years = as.numeric(car$years)
car$tag_price = as.numeric(car$tag_price)
car$cc = as.numeric(car$cc)
car$compression = as.numeric(car$compression)
car$doors = as.numeric(car$doors)
car$passengers = as.numeric(car$passengers)
car$length = as.numeric(car$length)
car$height = as.numeric(car$height)
car$weight = as.numeric(car$weight)
car$width  = as.numeric(car$width)
car$wheelbase = as.numeric(car$wheelbase)
car$trunk_capacity = as.numeric(car$trunk_capacity)
car$trunk_fullcapacity = as.numeric(car$trunk_fullcapacity)
car$tank_capacity = as.numeric(car$tank_capacity)
car$fuel_city = as.numeric(car$fuel_city)
car$fuel_freeway = as.numeric(car$fuel_freeway)
car$fuel_consum = as.numeric(car$fuel_consum)
car$license_tax = as.numeric(car$license_tax)
car$fuel_expense = as.numeric(car$fuel_expense)

#str(car)

car.brand = dbGetQuery( conn,'select distinct brand from yahooNewCars_clean2' )
car.brand <- as.character(c('VOLKSWAGEN','TOYOTA','NISSAN','MITSUBISHI','MAZDA','LEXUS','HONDA','FORD','BMW','BENZ'))
car.Nbrand<- c(1,2,3,4,5,6,7,8,9,10)
map.brand <- as.data.frame(cbind(car.brand,car.Nbrand))

map.brand$car.brand <- factor(map.brand$car.brand,
              levels = c('VOLKSWAGEN','TOYOTA','NISSAN','MITSUBISHI','MAZDA','LEXUS','HONDA','FORD','BMW','BENZ'),
              labels = c(1,2,3,4,5,6,7,8,9,10))


