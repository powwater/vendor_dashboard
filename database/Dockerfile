FROM postgres:latest

COPY SQL/powwater_dev_public_schema_dump.sql /docker-entrypoint-initdb.d/powwater_db_dump.sql

EXPOSE 5432