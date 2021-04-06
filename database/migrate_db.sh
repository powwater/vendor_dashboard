# backup public schema to a local file:
pg_dump --host "34.93.37.61" --port "5432" --username "postgres" --dbname "powwater_dev" --schema "public" -f "public.pgsql"

# change public to new public_mobile schema
psql --host "34.93.37.61" --port "5432" --username "postgres" --dbname "powwater_dev" -c "ALTER SCHEMA public RENAME TO public_mobile"

# use backup file to restore public schema
pg_restore --host "34.93.37.61" --port "5432" --username "postgres" --verbose --schema "public" -f "public.pgsql"
