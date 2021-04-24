# Setup and Run a Local Sync'd Copy of Production Database for Development

```bash
sudo apt-get install ruby-dev libpq-dev build-essential

sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
sudo apt-get update
sudo apt-get install postgresql

sudo gem install pgsync

mkdir powwater_localdb
cd powwater_localdb

pgsync --init
# edit yml file

# run db in docker
docker run --name postgres -p 5432:5432 -e POSTGRES_PASSWORD=p -d postgres

# get docker image ID from listing:
sudo docker ps -a 

# exec into container and create tables:
docker exec -it <ID> /bin/sh

# in docker containers cmd:
psql -d postgres -h localhost -p 5432 -U postgres -W
<enter password>

# create extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

# create types:
CREATE TYPE order_type AS ENUM ('New', 'Swap', 'Refill');
CREATE TYPE order_status AS ENUM ('Completed', 'Pending', 'In Progress', 'Cancelled', 'Rejected');
CREATE TYPE order_delivery_status AS ENUM ('Pending', 'Awaiting Delivery', 'Out for Delivery', 'Delivered', 'Cancelled', 'Rejected');
CREATE TYPE vehicle_type AS ENUM ('Boda', 'Tuk Tuk');
CREATE TYPE payment_type AS ENUM ('Credit Card', 'MPesa', 'Mobile Money');

# create tables:
CREATE TABLE customers (
  uid                      UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  customer_first_name      TEXT,
  customer_last_name       TEXT,
  customer_phone_number    TEXT,
  customer_phone_region    TEXT,
  customer_email           TEXT,
  created_at               TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  created_by               TEXT DEFAULT NULL,
  modified_at              TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  modified_by              TEXT DEFAULT NULL
);

CREATE TABLE vendors (
  uid UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  vendor_name TEXT,
  website TEXT,
  vendor_location TEXT,
  vendor_location_url TEXT,
  phone_number TEXT,
  operation_description TEXT,
  sealing BOOLEAN,
  refill BOOLEAN,
  bottle_deposit_model BOOLEAN,
  customer_locations TEXT,
  status TEXT,
  rating REAL CHECK (rating >= 0 AND rating <= 5),
  approval_status TEXT,
  commission REAL,
  discount REAL,
  discount_minimum REAL,
  online_status TEXT,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  created_by TEXT,
  modified_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  modified_by TEXT
);

CREATE TABLE riders (
  uid                      UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  rider_id                 SERIAL,
  rider_name               TEXT,
  phone_number             TEXT,
  vehicle_type             TEXT,
  rating                   REAL,
  carrying_capacity        REAL,
  insurance                TEXT,
  drivers_license          TEXT,
  driver_status            TEXT,
  certificate_standing     TEXT,
  approval_status          TEXT,
  wallet_balance           REAL,
  cancellation_rate        REAL,
  acceptance_rate          REAL,
  created_at               TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  created_by               TEXT DEFAULT NULL,
  modified_at              TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  modified_by              TEXT DEFAULT NULL
);

CREATE TABLE orders (
  uid UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  customer_uid UUID REFERENCES customers(uid),
  vendor_uid UUID REFERENCES vendors(uid),
  rider_uid UUID REFERENCES riders(uid),
  order_number SERIAL NOT NULL,
  order_type order_type,
  order_status order_status,
  order_delivery_status order_delivery_status,
  vendor_response TEXT,
  vendor_response_text TEXT,
  order_date DATE NOT NULL,
  order_time TIME NOT NULL,
  order_datetime TIMESTAMP NOT NULL,
  timezone TEXT NOT NULL,
  timezone_code TEXT NOT NULL,
  payment_type payment_type,
  currency TEXT,
  price_of_water REAL,
  delivery_fee REAL,
  delivery_commission REAL,
  vendor_commission REAL,
  discount_applied TEXT,
  discount_amount REAL,
  total_payment_price REAL,
  time_vendor_prep REAL,
  time_rider_to_vendor REAL,
  time_rider_to_customer REAL,
  total_delivery_time REAL,
  vendor_response_time TIMESTAMP DEFAULT NULL,
  cancellation_time TIMESTAMP DEFAULT NULL,
  rejected_time TIMESTAMP DEFAULT NULL,
  vendor_rating REAL CHECK (
    vendor_rating >= 0
    AND vendor_rating <= 5
  ),
  rider_rating REAL CHECK (
    rider_rating >= 0
    AND rider_rating <= 5
  ),
  order_rating REAL CHECK (
    order_rating >= 0
    AND order_rating <= 5
  ),
  is_vendor_rating_malicious BOOLEAN NOT NULL DEFAULT FALSE,
  is_rider_rating_malicious BOOLEAN NOT NULL DEFAULT FALSE,
  is_order_rating_malicious BOOLEAN NOT NULL DEFAULT FALSE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  created_by TEXT,
  modified_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  modified_by TEXT
);

CREATE TABLE customer_locations (
  uid                        UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  customer_uid               UUID REFERENCES customers(uid),
  customer_location_name     TEXT,
  customer_location_url      TEXT,
  customer_location_lat      REAL,
  customer_location_lon      REAL,
  customer_location_address  TEXT,
  customer_location_vicinity TEXT,
  customer_location_place_id TEXT,
  created_at                 TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  created_by                 TEXT DEFAULT NULL,
  modified_at                TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  modified_by                TEXT DEFAULT NULL
);

CREATE TABLE vendor_locations (
  uid                        UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  vendor_uid                 UUID REFERENCES vendors(uid),
  vendor_location_name       TEXT,
  vendor_location_url        TEXT,
  vendor_location_lat        REAL,
  vendor_location_lon        REAL,
  vendor_location_address    TEXT,
  vendor_location_vicinity   TEXT,
  vendor_location_place_id   TEXT,
  vendor_region_name         TEXT,
  vendor_region_place_id     TEXT,
  vendor_region_url          TEXT,
  created_at                 TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  created_by                 TEXT DEFAULT NULL,
  modified_at                TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  modified_by                TEXT DEFAULT NULL
);

CREATE TABLE vendor_offerings (
  uid                      UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  vendor_uid               UUID REFERENCES vendors(uid),
  capacity                 REAL,
  offer_type               TEXT,
  price_per_unit           REAL CHECK (price_per_unit > 0),
  created_at               TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  created_by               TEXT,
  modified_at              TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  modified_by              TEXT
);

CREATE TABLE vendor_inventory (
  uid                      UUID NOT NULL DEFAULT uuid_generate_v4(),
  vendor_uid               UUID,
  capacity                 REAL,
  offer_type               TEXT,
  price_per_unit           REAL CHECK (price_per_unit > 0),
  quantity                 INTEGER,
  created_at               TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  created_by               TEXT,
  modified_at              TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  modified_by              TEXT
);

CREATE TABLE vendor_riders (
  vendor_uid UUID,
  rider_uid UUID,
  PRIMARY KEY(vendor_uid, rider_uid)
);

CREATE TABLE vendor_tests (
  uid                      UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  vendor_uid               UUID REFERENCES vendors(uid),
  test_date                DATE,
  ph_value                 REAL CHECK (ph_value > 0),
  tds                      REAL CHECK (tds > 0),
  tbc                      BOOLEAN DEFAULT NULL,
  created_at               TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  created_by               TEXT,
  modified_at              TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  modified_by              TEXT
);

CREATE TABLE vendor_working_hours (
  uid                        UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  vendor_uid                 UUID REFERENCES vendors(uid),
  day_of_week                INTEGER,
  working_hours_start        TIME,
  working_hours_stop         TIME,
  created_at                 TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  created_by                 TEXT DEFAULT NULL,
  modified_at                TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  modified_by                TEXT DEFAULT NULL
);

CREATE TABLE order_items (
  uid                      UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  order_uid                UUID,
  volume                   REAL,
  quantity                 REAL,
  order_type               ORDER_TYPE, -- changed from `type` due to confusion with SQL type command
  created_at               TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  created_by               TEXT,
  modified_at              TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  modified_by              TEXT
);

CREATE TABLE order_routes (
  uid UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  order_uid UUID REFERENCES orders(uid),
  vendor_location_uid UUID REFERENCES vendor_locations(uid),
  customer_location_uid UUID REFERENCES customer_locations(uid),
  origin_id TEXT,
  destination_id TEXT,
  estimated_distance REAL,
  estimated_duration REAL,
  estimated_polyline TEXT,
  actual_distance REAL,
  actual_duration REAL,
  actual_polyline TEXT,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  created_by TEXT,
  modified_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  modified_by TEXT
);

CREATE TABLE delivery_pricing (
  uid                        UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  tier_distance_min          INTEGER NOT NULL CHECK (tier_distance_min >= 0),
  tier_distance_max          INTEGER NOT NULL CHECK (tier_distance_max > tier_distance_min),
  tier_new_delivery_fee      REAL NOT NULL CHECK (tier_new_delivery_fee >= 0),
  tier_refill_delivery_fee   REAL NOT NULL CHECK (tier_refill_delivery_fee >= 0),
  tier_swap_delivery_fee     REAL NOT NULL CHECK (tier_swap_delivery_fee >= 0),
  created_at                 TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  created_by                 TEXT DEFAULT NULL,
  modified_at                TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  modified_by                TEXT DEFAULT NULL
);

# exit
exit

# commit
sudo docker commit <ID> powwater_localdb

# sync
pgsync customers,vendors,riders,orders,order_items,customer_locations,vendor_locations,vendor_riders,order_routes,vendor_offerings,vendor_tests,vendor_working_hours,vendor_inventory --defer-constraints-v2 --disable-integrity
```

## Output Results

```bash
pgsync customers,vendors,riders,orders,order_items,customer_locations,vendor_locations,vendor_riders,order_routes,vendor_offerings,vendor_tests,vendor_working_hours,vendor_inventory --defer-constraints-v2 --disable-integrity

From: powwater_dev on 34.93.37.61:5432
To: postgres on localhost:5432
vendors: Extra columns: online_status
vendors: Different column types: bottle_deposit_model (text -> boolean), commission (numeric -> real), discount (numeric -> real), discount_minimum (numeric -> real), rating (numeric -> real), refill (text -> boolean), sealing (text -> boolean)
riders: Extra sequences: public.riders_rider_id_seq1
riders: Missing sequences: public.riders_rider_id_seq
orders: Extra sequences: public.orders_order_number_seq1
orders: Missing sequences: public.orders_order_number_seq
orders: Different column types: cancellation_time (timestamp with time zone -> timestamp without time zone), order_datetime (timestamp with time zone -> timestamp without time zone), order_time (time with time zone -> time without time zone), rejected_time (timestamp with time zone -> timestamp without time zone), vendor_response_time (timestamp with time zone -> timestamp without time zone)
order_items: Extra columns: order_type
order_items: Missing columns: order_number, type
order_items: Different column types: quantity (numeric -> real), volume (numeric -> real)
vendor_tests: Missing columns: test_date_tbc

✔ customers - 0.3s
✔ vendors - 0.3s
✔ riders - 0.3s
✔ orders - 0.3s
✔ order_items - 0.3s
✔ customer_locations - 0.3s
✔ vendor_locations - 0.3s
✔ vendor_riders - 0.3s
✔ order_routes - 0.3s
✔ vendor_offerings - 0.3s
✔ vendor_tests - 0.3s
✔ vendor_working_hours - 0.3s
✔ vendor_inventory - 0.3s
```



Now can run shiny app with `docker_db <- TRUE` on line 77 to connect to locally sync'd database instead of production database.
