DROP DATABASE IF EXISTS scalajack;
CREATE DATABASE scalajack;
USE scalajack;

CREATE TABLE people(
	name VARCHAR(20),
	age INTEGER,
	crazy DOUBLE,
	PRIMARY KEY (name) 
);

INSERT INTO people VALUES("Tom", 35, 0.14);
INSERT INTO people VALUES("Mary", 24, 0.39);