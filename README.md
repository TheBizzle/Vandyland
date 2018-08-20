Vandyland
===============

### Installation Instructions

  * If your machine doesn't already have it, you might need to [install libgmp](http://www.mathemagix.org/www/mmdoc/doc/html/external/gmp.en.html)
  * [Download Stack](https://docs.haskellstack.org/en/stable/README/)
  * [Download Postgresql](https://www.postgresql.org/download/) (this tends to be the most troublesome part)
    * Vandyland hasn't been tested with any Postgres version but Postgres 9, so use other versions at your own risk
    * After you think you've gotten Postgres set up, ensure that `psql --version` and `pg_config --version` print version numbers; if not, your installation is probably broken
    * Ensure that that the Postgres server is running by performing `ps aux | grep postgres` and looking for a `postgres` process; if not, you need to launch the server, which can be done by running `pt_ctl start` (or [these instructions](https://www.postgresql.org/docs/9.1/static/server-start.html))
    * If you get an error in the coming steps that says "Missing C library: pq", you need to get Postgres stuff onto your PATH; on Red Hat, this can be accomplished by installing `postgresql*-devel` through `yum`
  * Next, you'll need to initialize the Postgres database tables.  From the command prompt, run `psql --username=postgres` and then `CREATE DATABASE vandyland WITH ENCODING='UTF8' CONNECTION LIMIT=-1;` and then `CREATE DATABASE badgerstate WITH ENCODING='UTF8' CONNECTION LIMIT=-1;`.
  * At the root of the repository, add the files `.db_username` and `.db_password`.  The former should contain your Postgres username (default: postgres), and the latter your Postgres password (default: \<empty string>).
  * Run the command `stack install happy` to ensure that the Happy build tool is available for the dependencies

### Running

  * To run the server without HTTP, run `stack build && stack exec vandyland`

  * For HTTPS support, ensure that your SSL cert is accessible and run this command: `stack build && sudo /PATH/TO/STACK/stack exec --allow-different-user vandyland -- --port=80 --ssl-port=443 --ssl-cert=/PATH/TO/CERT/cert.pem --ssl-key=/PATH/TO/KEY/privkey.pem --ssl-address=0.0.0.0 --no-ssl-chain-cert` (your filenames for the key and cert may differ)

  * If you want the test page to run swimmingly from non-localhost URL, update [the first line of `testpage.js`](https://github.com/TheBizzle/Vandyland/blob/master/html/basic/testpage.js#L1) to reflect the URL of the server, and then hit the `/html/basic` URL with your browser.

### API Docs

Web API docs can be found [here](https://github.com/TheBizzle/Vandyland/wiki/Web-API).
