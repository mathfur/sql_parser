sql_parser
==========
Now, sql_parser is only able to input SELECT statement and output the table names.

Install
-------

 1. You need cabal and cabal-dev.
 2. git clone https://github.com/mathfur/sql_parser.git
 3. cd sql_parser
 4. cabal-dev install --only-dependencies
 5. cabal-dev configure --enable-tests
 6. cabal-dev build
 7. cabal-dev test

Usage
-----
```
% echo "SELECT * FROM users LEFT JOIN companies ON companies.id = users.company_id" | dist/build/sql_parser/sql_parser
users, companies
```

License
-------
Copyright &copy; 2012 mathfur
Licensed under the [Apache License,  Version 2.0][Apache]
Distributed under the [MIT License][mit].
Dual licensed under the [MIT license][MIT] and [GPL license][GPL].
[Apache]: http://www.apache.org/licenses/LICENSE-2.0
[MIT]: http://www.opensource.org/licenses/mit-license.php
[GPL]: http://www.gnu.org/licenses/gpl.html
