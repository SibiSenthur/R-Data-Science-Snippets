--UIN: 655532606
--Name: Sibi Senthur Muthusamy

-- Creating Schema for Table 1
-- Question #1
create schema hiveHomeWork;
use hiveHomeWork;

create table hiveHomeWork.table1
(userId String,
photoId String,
createdTime TIMESTAMP,
filter String,
likes INT,
COMMENTS INT)
ROW FORMAT DELIMITED FIELDS TERMINATED BY ','
STORED AS TEXTFILE;

LOAD DATA LOCAL INPATH "/home/cloudera/Hive/instagram-micro.csv" OVERWRITE INTO TABLE hiveHomeWork.table1;
-- Question #2
set hive.exec.dynamic.partition=true;
set hive.exec.dynamic.partition.mode=nonstrict;

create table hiveHomeWork.table2
(userId String,
photoId String,
createdTime TIMESTAMP,
likes INT,
COMMENTS INT)
PARTITIONED BY (filter String)
ROW FORMAT DELIMITED FIELDS TERMINATED BY ','
STORED AS TEXTFILE;

INSERT OVERWRITE TABLE hiveHomeWork.table2 PARTITION(filter) SELECT userId, photoId, createdTime, likes, comments, filter FROM hiveHomeWork.table1;
-- Question #3
set hive.enforce.bucketing=true;

create table hiveHomeWork.table3
(userId String,
photoId String,
createdTime TIMESTAMP,
filter String,
likes INT,
COMMENTS INT)
CLUSTERED BY (userId) INTO 5 BUCKETS
ROW FORMAT DELIMITED FIELDS TERMINATED BY ','
STORED AS TEXTFILE;

INSERT OVERWRITE TABLE hiveHomeWork.table3 SELECT userId, photoId, createdTime, filter, likes, comments FROM hiveHomeWork.table1;
-- Question #4
set hive.exec.dynamic.partition=true;
set hive.exec.dynamic.partition.mode=nonstrict;
set hive.enforce.bucketing=true;

create table hiveHomeWork.table4
(userId String,
photoId String,
createdTime TIMESTAMP,
likes INT,
COMMENTS INT)
PARTITIONED BY (filter String)
CLUSTERED BY (userId) INTO 5 BUCKETS
ROW FORMAT DELIMITED FIELDS TERMINATED BY ','
STORED AS TEXTFILE;

INSERT OVERWRITE TABLE hiveHomeWork.table4 PARTITION(filter) SELECT userId, photoId, createdTime, likes, comments, filter FROM hiveHomeWork.table1;


-- Question #5 
SELECT filter, count(filter) as Count FROM hiveHomeWork.table1 WHERE filter IN ('Clarendon','Gingham','Reyes') GROUP BY filter ORDER BY Count DESC; 

-- Question #6
SELECT filter, count(filter) as Count FROM hiveHomeWork.table4 WHERE filter IN ('Clarendon','Gingham','Reyes') GROUP BY filter ORDER BY Count DESC;
