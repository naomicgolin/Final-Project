USE stats_140_project; # Select database

# Column Names and Data Types
SELECT COLUMN_NAME, DATA_TYPE
FROM INFORMATION_SCHEMA.COLUMNS
where TABLE_NAME = 'ufo_data' ;

# Number of Columns
SELECT COUNT(*)
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_NAME = 'ufo_data' ;

# Number of Rows
SELECT COUNT(*)
FROM ufo_data;

# Add primary key
ALTER TABLE ufo_data 
ADD id int NOT NULL AUTO_INCREMENT primary key FIRST;

# Changing datatype of columns
ALTER TABLE `ufo_data`
MODIFY COLUMN state varchar(2000),
MODIFY COLUMN shape varchar(2000);

# Create Year Column
ALTER TABLE ufo_data
ADD COLUMN Year varchar(4);

UPDATE ufo_data
SET Year = SUBSTRING(date_time, 1, 4);
