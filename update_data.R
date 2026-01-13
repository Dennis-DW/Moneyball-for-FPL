print("🔄 connecting to FPL API...")

# Step 1: Download latest data
source("R/01_fetch_data.R")
update_local_data() 

# Step 2: Clean and recalculate points
source("R/02_clean_data.R")
clean_fpl_data()

print("✅ Data Updated Successfully!")
print("👉 Refresh your web browser to see the new stats.")

#head()	Shows the first 6 rows (Quick peek)	head(my_rds$data)
#str()	Shows the structure (Data types, columns)	str(my_rds$data)
#colnames()	Lists all column names	colnames(my_rds$data)
#dim()	Shows dimensions (Rows x Columns)	dim(my_rds$data)
#unique()	Shows unique values in a specific column	unique(my_rds$data$manager)