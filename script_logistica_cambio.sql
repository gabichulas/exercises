-- Habilitar PostGIS (si está instalado)
CREATE EXTENSION IF NOT EXISTS postgis;

-- Personas y Usuarios
CREATE TABLE Person (
    id SERIAL PRIMARY KEY,
    first_name TEXT,
    last_name TEXT,
    email TEXT
);

CREATE TABLE Users (
    person_id INT PRIMARY KEY REFERENCES Person(id),
    password_hash TEXT
);

CREATE TABLE Admin_ (
    person_id INT PRIMARY KEY REFERENCES Users(person_id),
    role TEXT
);

CREATE TABLE Driver (
    person_id INT PRIMARY KEY REFERENCES Users(person_id),
    available BOOLEAN,
    working_hours TEXT
);

CREATE TABLE Client (
    person_id INT PRIMARY KEY REFERENCES Person(id)
);

-- Puntos geográficos
CREATE TABLE Point_ (
    id SERIAL PRIMARY KEY,
    numeracion VARCHAR(10),
    point_name VARCHAR(100),
    tipo VARCHAR(50),
    point_location GEOGRAPHY(POINT, 4326), -- WGS 84
    owner_id INT REFERENCES Client(person_id)
);

-- Calles entre puntos
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

-- Gestión de puntos (clave compuesta)
CREATE TABLE Point_Management (
    admin_id INT REFERENCES Admin_(person_id),
    point_id INT REFERENCES Point_(id),
    change_date TIMESTAMPTZ,
    change_type TEXT,
    PRIMARY KEY (admin_id, point_id)
);

-- Vehículos
CREATE TABLE Vehicle (
    id SERIAL PRIMARY KEY,
    capacity_kg FLOAT,
    volume_m3 FLOAT,
    v_type TEXT,
    available BOOLEAN
);

-- Viajes
CREATE TABLE Trip (
    id SERIAL PRIMARY KEY,
    trip_date DATE,
    started_at TIMESTAMPTZ,
    ended_at TIMESTAMPTZ,
    total_distance FLOAT,
    total_time FLOAT,
    vehicle_id INT REFERENCES Vehicle(id)
);

-- Relación entre conductores y viajes (clave compuesta)
CREATE TABLE Driver_Trip (
    driver_id INT REFERENCES Driver(person_id),
    trip_id INT REFERENCES Trip(id),
    PRIMARY KEY (driver_id, trip_id)
);

CREATE TABLE Package (
    id SERIAL PRIMARY KEY,
    weight_kg FLOAT,
    volume_m3 FLOAT,
    description VARCHAR(100),
    priority INT
);

CREATE TABLE Shipment (
    id SERIAL PRIMARY KEY,
    ship_date DATE,
    delivered BOOLEAN,
    trip_id INT REFERENCES Trip(id)
);

CREATE TABLE Shipment_Package (
    shipment_id INT REFERENCES Shipment(id),
    package_id INT REFERENCES Package(id),
    PRIMARY KEY (shipment_id, package_id)
);

-- Puntos de entrega de cada envío (clave compuesta)
CREATE TABLE Ships (
    shipment_id INT REFERENCES Shipment(id),
    point_id INT REFERENCES Point_(id),
    distance FLOAT,
    PRIMARY KEY (shipment_id, point_id)
);
