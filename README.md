Vandyland
===============

### Installation Instructions

  * [Download Stack](https://docs.haskellstack.org/en/stable/README/)
  * [Download Postgresql](https://www.postgresql.org/download/)
  * From the command prompt, run `psql --username=postgres` and then `CREATE DATABASE vandyland WITH ENCODING='UTF8' CONNECTION LIMIT=-1;` to initialize Postgres
  * Configure your username for Postgresql in the file `.db_username` (default: postgres) and your password in the file `.db_password` (default: \<empty string>)
  * Run the server with `stack build && stack exec vandyland`

### TO BE CONTINUED
