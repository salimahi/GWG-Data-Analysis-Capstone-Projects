cyclistic <- rbind(c202207, c202208, c202209, c202210, c202211, c202212, c202301, c202302, c202303, c202304, c202305, c202306, c202307)
write.csv(cyclistic, "C:\\Users\\salim\\Dropbox\\Ismail\\GWG-Data-Analysis-Capstone-Projects\\Cyclistic Trip Data\\combined_data.csv", row.names=FALSE)
View(cyclistic)
unique(cyclistic$rideable_type)
