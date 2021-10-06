# esqueleto-pgcrypto



## Tests

To run tests you will need a `pgctest` named postgresql database listening on port 5432.

The connection details are

````haskell
"host=localhost port=5432 user=pgctest password=pgctest dbname=pgctest"
````

You can crete this instace as follows:

````bash
$ sudo -u postgres createuser --superuser pgctest 
$ sudo -u postgres createdb pgctest
$ sudo -u postgres psql
# set the password of pgctest to pgctest
postgres=# \password pgctest 
````
