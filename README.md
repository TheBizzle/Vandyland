Vandyland
===============

### Installation Instructions

  * [Download Stack](https://docs.haskellstack.org/en/stable/README/)
  * [Download Postgresql](https://www.postgresql.org/download/)
  * From the command prompt, run `psql --username=postgres` and then `CREATE DATABASE vandyland WITH ENCODING='UTF8' CONNECTION LIMIT=-1;` to initialize Postgres
  * At the root of the repository, add the files `.db_username` and `.db_password`.  The former should contain your Postgres username (default: postgres), and the latter your Postgres password (default: \<empty string>).
  * Run the command `stack install happy` ensure that the Happy build tool is available for the dependencies
  * Run the server with `stack build && stack exec vandyland`
  * If you want the test page to run swimmingly from non-localhost URL, update [the first line of `testpage.js`](https://github.com/TheBizzle/Vandyland/blob/master/html/testpage.js#L1) to reflect the URL of the server.

### TO BE CONTINUED
