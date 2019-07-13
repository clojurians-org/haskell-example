# Haskell的数据库操作(persistent)

与HDBC一样的是persistent也是一样抽象层的东西，这里拿mysql为例，需要三个依赖 `persistent`、 `persistent-mysql`和`persistent-template`，其中template依赖包是用于自动生成代码的，即不用写重复度非常相同的模版代码。另外仍需要一个依赖，是关于log的，将自动记录。

```
dependencies:
...
- monad-logger
- persistent
- persistent-mysql
- persistent-template
...
```

## 生成数据库映射对象

```
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
	Id Int
	name String
	password String
	deriving Show
|] 
```

由persistLowerCase生成的就是一个Data的结构，包括了`UserKey`主键 和其他。需要注意的是，当下面使用查询类接口拿到实例的时候，其实拿到的并不是`User` 而是`Entity User`，要拿到到最基本的结构就要使用函数 `entityVal`获取。

详细可参考 <https://github.com/yesodweb/persistent/blob/master/docs/Persistent-entity-syntax.md>


另外可以进行映射定制化 ：

* 自动生成json转换(Aeson依赖包功能)

```
User json
	Id Int
	...
```

* 表名称(字段名称)与结构(结构字段)名称映射

```
User sql=sys_user
	Id Int sql=user_id
	...
```

* 指定数据库字段类型

```
User
	name String sqltype=varchar(3)
```

## 创建数据库连接池
persistent是自带了连接池的，一般的执行都是通过`runSqlPersistMPool`来执行。另外由于persistent创建连接池的函数需要是Logger类型的，所有需要`runStdoutLoggingT`来创建。下面是以MySQL为例。

```
-- 连接信息
connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo {
  connectPort = 3306,
  connectHost = "127.0.0.1",
  connectDatabase = "mytest",
  connectUser = "root",
  connectPassword = "root"
}

-- 创建连接池
createDBPool :: IO (Pool SqlBackend)
createDBPool = do
	pool <- runStdoutLoggingT $ createMySqlPool connectionInfo 10
	return pool
```


## 执行数据库操作

### 查询
一般查询是通过函数 `selectXXX [] []`来执行的，参数的第一个列表是过滤规则，当过滤列表为空时，需要指定是什么表的过滤情况，`selectList ([] :: [Filter User]) []` 第二个列表是其他选择，如下：

```
people <- selectList
    (       [PersonAge >. 25, PersonAge <=. 30]
        ||. [PersonFirstName /<-. ["Adam", "Bonny"]]
        &&. ([PersonAge ==. 50] ||. [PersonAge ==. 60])
    )
    []
```
其他选项一般是指 `order by`等选项，如下：

```
resultsForPage pageNumber = do
    let resultsPerPage = 10
    selectList
        [ PersonAge >=. 18]
        [ Desc PersonAge
        , Asc PersonLastName
        , Asc PersonFirstName
        , LimitTo resultsPerPage
        , OffsetBy $ (pageNumber - 1) * resultsPerPage
        ]
```
简单的小例子:

```
action1 :: IO ()
action1 = do
  pool <- runStdoutLoggingT (createMySQLPool connectionInfo 10)
  flip runSqlPersistMPool pool $ do
    doMigrations -- runMigration migrateAll ,结构变更时使用，可有可无
    users <- selectList ([UserId ==. UserKey 1] :: [Filter User]) []
    liftIO $ print $ map (\u -> entityKey u) users

action3 :: Pool SqlBackend -> IO [User]
action3 pool = do
  flip runSqlPersistMPool pool $ do
    users <- selectList ([] :: [Filter User]) []
    return $ map entityVal users

```

`flip`函数是把一个函数的第一个参数和第二个参数位置变更，这里因为第一个参数是一个复杂的逻辑，所以放到后面去实现会更美观。

### 插入
插入很简单，基本常用的就是几个函数`insert` `insertMany` `insertKey`

* `insert` : 插入，直接入参一个记录结构，返回插入成功后一个主键
* `insertMany`: 插入多个，与插入一样，返回多个的主键
* `insertKey`: 同时带有主键的插入，生成主键利用自动生成的函数生成 `XXXKey`， 这里的User的函数是`UserKey`

一个小例子:

```
action2 :: IO ()
action2 = do
  pool <- createDBPool
  flip runSqlPersistMPool pool $ do
    uid <- insertKey (UserKey 7) (User "test" "test")
    liftIO $ print uid
```

### 更新
更新也是很简单，基本常用的就是三个函数`update` `udateWhere` `replace`,不过不同的是。

* `update`: 两个参数，第一个参数是主键，第二参数是设值逻辑。

```
personId <- insert $ Person "Michael" "Snoyman" 26
update personId [PersonAge =. 27]
```

* `updateWhere`: 两个参数，第一个参数是过滤列表，和select的一样，第二参数是设值逻辑.

```
updateWhere [PersonFirstName ==. "Michael"] [PersonAge *=. 2]
```

* `replace`:直接根据主键替换，第一个参数是主键，第二个参数是记录结构。

```
personId <- insert $ Person "Michael" "Snoyman" 26
replace personId $ Person "John" "Doe" 20
```

一些小例子：

```
-- 更新的例子
action4 :: Pool SqlBackend -> IO ()
action4 pool = actionBone pool $ do
  update (UserKey 1) [UserName =. "test" , UserPassword =. "test"]
  liftIO $ print "test"

-- 更新替换
action5 ::Pool SqlBackend -> IO()
action5 pool = actionBone pool $ do
  replace (UserKey 1) $ User "admin" "admin"
```

### 删除
删除常用的也是有三个函数 `delete` `deleteBy` `deleteWhere`

就不详细说明了，很简单

```
personId <- insert $ Person "Michael" "Snoyman" 26
delete personId
deleteBy $ PersonName "Michael" "Snoyman"
deleteWhere [PersonFirstName ==. "Michael"]
```