DROP DATABASE IF EXISTS scalajack;
CREATE DATABASE scalajack;
USE scalajack;

CREATE TABLE people(
	name VARCHAR(20) NOT NULL,
	age INTEGER NOT NULL,
	crazy DOUBLE NOT NULL,
	PRIMARY KEY (name) 
);

CREATE TABLE stuff(
	str VARCHAR(20) NOT NULL,
	i INTEGER NOT NULL,
	b BOOLEAN NOT NULL,
	maybe VARCHAR(10),
	PRIMARY KEY (str)
);

INSERT INTO people VALUES("Tom", 35, 0.14);
INSERT INTO people VALUES("Mary", 24, 0.39);
INSERT INTO stuff VALUES("wow",5,true,null);
INSERT INTO stuff VALUES("yikes",7,true,"ok");