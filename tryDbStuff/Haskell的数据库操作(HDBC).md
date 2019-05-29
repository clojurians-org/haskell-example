# Haskell的数据库操作(HDBC)

在Haskell上操作数据需要两个东西，HDBC包和对应数据库的Driver。但是有一点要特别注意，与Java上的JDBC不同，大部分的HDBC仍需要底层C库的支持。（以MySQL为例，依赖需要HDBC、HDBC-mysql、系统级别的mysql-client和系统级别的ssl)

package.yaml:

```
...
dependencies:
- base >=4.7 && <5
- HDBC
- HDBC-mysql
...
```
全局的stack配置文件 ~/.stack/config.yaml:

```
...
extra-include-dirs:
- /usr/local/opt/openssl/include

extra-lib-dirs:
- /usr/local/opt/openssl/lib
...
```

stack.yaml:

```
...
extra-include-dirs:
- /usr/local/Cellar/mysql-client/5.7.23/include
extra-lib-dirs:
- /usr/local/Cellar/mysql-client/5.7.23/lib
...
```

## HDBC执行SQL语句

* 获取数据库连接

```
conn <- connectXXX xxx
-- MySQL示例
conn <- connectMySQL defaultMySQLConnectInfo {
	mysqlHost = "root",
	mysqlPort = 3306,
	mysqlDatabase = "mytest".
	mysqlUser = "root",
	mysqlPassword = "root"
}
```

* 执行语句并提交

```
-- 执行(已经会有结果),返回的结果是被改动的行数
run conn "insert into User(id,name) values(?,?)" [toSql 0,toSql "name"]
-- 提交 (与事务有管)
commit conn
```

* 错误处理

```
handleSql (\err -> ...) (run conn "sql" [] )

handleSql handleErr runSql
	where
		handleErr :: SqlError -> IO a
		handleErr = ... -- 处理错误的逻辑，比如回滚等
		runSql :: IO a
		runSql = run conn "sql" [sql参数] --执行sql的逻辑
```

* 查询语句

```
quickQuery' conn "SELECT * from test where id < 2" []
-- 返回的是Sql数据结构的列表
-- [[SqlString "0",SqlNull],[SqlString "0",SqlString "zero"],[SqlString "1",SqlString "one"]]
```

* 预处理语句

```
-- 创建预处理语句
stmt <- prepare conn "SELECT * from test where id < 2"
-- 执行预处理语句
execute stmt []
-- 从stmt获取结果
results <- fetchAllRowsAL stmt 
```
`fetchAllRowsAL` 和`fetchAllRowsAL'`都是获取所有行数据，不同的是前者是惰性获取的，而后者是严格求值，当数据量特别大的时候，严格求值可能会消耗更多资源，而惰性求值是按块获取。但是使用惰性时需要当把上一次的数据都处理完才能进行下一次获取，否则上一次未处理的数据会丢失。