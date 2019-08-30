brew install postgresql
brew services start postgresql
echo "create user coodle with password 'coodle3110'" | psql postgres
echo "create database coodle_data owner coodle" | psql postgres
echo "create table account (aid TEXT PRIMARY KEY NOT NULL,pwd TEXT NOT NULL)" | psql coodle_data
echo "create table event(eid TEXT primary key not null,description text,status text,start_time double precision,final_day int,final_time int,anonymous boolean)" | psql coodle_data
echo "create table availability(aid text,day int, time int, eid TEXT)" | psql coodle_data
echo "create table admin(aid text,eid TEXT,admin_right text)" | psql coodle_data
echo "GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO coodle" | psql coodle_data
