CREATE TABLE Componentes (
 ID INTEGER NOT NULL,
 NAMEEN CHAR(100),
 NAMEPT CHAR(100),
 HEIGHT INTEGER,
 WIDTH  INTEGER,
 PINS   INTEGER,
 DRAWINGCODE VARCHAR,
 PRIMARY KEY (ID));
INSERT INTO Components VALUES (
 1,
 'Resistor',
 'Resistor',
 3,
 3,
 2,
 ''
);
.exit
