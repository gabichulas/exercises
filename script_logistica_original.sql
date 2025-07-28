CREATE EXTENSION postgis;



CREATE TABLE Person (
    id SERIAL PRIMARY KEY,
    first_name TEXT,
    last_name TEXT,
    email TEXT
);

CREATE TABLE User (
    person_id INT PRIMARY KEY REFERENCES Person(id),
    password_hash TEXT
);

CREATE TABLE Admin_ (
    person_id INT PRIMARY KEY REFERENCES User(person_id),
    role TEXT
);

CREATE TABLE Driver (
    person_id INT PRIMARY KEY REFERENCES User(person_id),
    available BOOLEAN,
    working_hours TEXT
);

CREATE TABLE Client (
    person_id INT PRIMARY KEY REFERENCES Person(id)
);

CREATE TABLE Point_ (
    id SERIAL PRIMARY KEY,
    numeracion VARCHAR(10),
    point_name VARCHAR(100),
    tipo VARCHAR(50),
    point_location GEOGRAPHY(POINT, 4326), -- WGS 84
    owner_id INT REFERENCES Client(person_id)
);

CREATE TABLE Street (
    id SERIAL PRIMARY KEY,
    from_point_id INT REFERENCES Point_(id),
    to_point_id INT REFERENCES Point_(id),
    street_name VARCHAR(100),
    distance FLOAT,
    street_length INT,
    maxspeed INT,
    street_weight FLOAT
);

CREATE TABLE Point_Management (
    admin_id INT PRIMARY KEY REFERENCES Admin_(id),
    point_id INT PRIMARY KEY REFERENCES Point_(id),
    change_date TIMESTAMPTZ,
    change_type TEXT
);

CREATE TABLE Vehicle (
    id SERIAL PRIMARY KEY,
    capacity_kg FLOAT,
    volume_m3 FLOAT,
    v_type TEXT,
    available BOOLEAN
);

CREATE TABLE Trip (
    id SERIAL PRIMARY KEY,
    trip_date DATE,
    started_at TIMESTAMPTZ,
    ended_at TIMESTAMPTZ,
    total_distance FLOAT,
    total_time FLOAT,
    vehicle_id INT REFERENCES Vehicle(id)
);

CREATE TABLE Driver_Trip (
    driver_id INT PRIMARY KEY REFERENCES Driver(id),
    trip_id INT PRIMARY KEY REFERENCES Trip(id)
);

CREATE TABLE Shipment (
    id SERIAL PRIMARY KEY,
    ship_date DATE,
    delivered BOOLEAN,
    package_info VARCHAR(100),
    package_priority INT,
    trip_id INT REFERENCES Trip(id)
);

CREATE TABLE Ships (
    shipment_id INT PRIMARY KEY REFERENCES Shipment(id),
    point_id INT PRIMARY KEY REFERENCES Point_(id),
    distance FLOAT
);

