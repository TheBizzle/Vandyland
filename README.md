Vandyland
===============

### Installation Instructions

  * [Download Stack](https://docs.haskellstack.org/en/stable/README/)
  * [Download Postgresql](https://www.postgresql.org/download/)
  * From the command prompt, run `psql --username=postgres` and then `CREATE DATABASE vandyland WITH ENCODING='UTF8' CONNECTION LIMIT=-1;` to initialize Postgres
  * Configure your login credentials for Postgresql in the file `DBCredentials.hs` (if default, username is "postgres" and password is "")
  * Run the server with `stack build && stack exec vandyland`

### TO BE CONTINUED
