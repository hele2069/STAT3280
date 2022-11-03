CREATE TABLE departments ( 
	department_id INT,
	department_name VARCHAR(50),
	CONSTRAINT departments_pk PRIMARY KEY (department_id) 
);

CREATE TABLE employees (
	employee_id INT,
    employee_name VARCHAR(50),
    department_id INT,
    employee_salary INT,
    CONSTRAINT employees_pk PRIMARY KEY (employee_id),
    CONSTRAINT fk_department_id FOREIGN KEY (department_id) REFERENCES departments (department_id)
);

DROP TABLE suppliers;
CREATE TABLE suppliers (
	supplier_id CHAR(3),
    supplier_name VARCHAR(50),
    city VARCHAR(40),
    state VARCHAR(30)
);
INSERT INTO suppliers VALUES('100','Microsoft','Redmond','Washington');
INSERT INTO suppliers VALUES('200','Google','Mountain View','California');
INSERT INTO suppliers VALUES('300','Oracle','Redwood City','California');
INSERT INTO suppliers VALUES('400','Kimberly-Clark','Irving','Texas');
INSERT INTO suppliers VALUES('500','Tyson Foods','Springdale','Arkansas');

UPDATE suppliers
	SET city = 'Boise',state='Idaho'
    WHERE supplier_name = 'Microsoft';

DELETE FROM suppliers 
	WHERE state = 'California';

SELECT * FROM suppliers



