# esqueleto-pgcrypto

Esqueleto support for the pgcrypto PostgreSQL module        

````haskell
share
    [mkPersist sqlSettings]
    [persistLowerCase|
    UserAccount json
        name T.Text
        UniqueName name
        passwordHash T.Text
        deriving Show Read Eq

-- insert a `PersistField` using pgcrypto's `crypt` function for password hashing
insertUserAccount = insertSelect $ 
    pure $
        UserAccount
            <# val "username"
            <&> toCrypt (BF Nothing) "1234password"

login name pwd = select $ do
    user <- from $ Table UserAccount
    where_ $ user ^. UserAccountName ==. val name
        &&. fromCrypt (user ^. UserAccountPasswordHash) pwd
    pure user

````

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
