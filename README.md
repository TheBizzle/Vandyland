Vandyland
===============

### Installation Instructions

  * [Download Stack](https://docs.haskellstack.org/en/stable/README/)
  * [Download Postgresql](https://www.postgresql.org/download/)
  * From the command prompt, run `psql postgres` and then `createdb vandyland -U postgres` to initialize Postgres
  * Configure your login credentials for Postgresql in the file `DBCredentials.hs`
  * Run the server with `stack build && stack exec vandyland`

### TO BE CONTINUED
