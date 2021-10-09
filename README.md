# EMQX 设备公私钥交换认证

## 简介
EMQX 设备公私钥交换认证插件，比较简单实用，适合快速认证和接入设备。

## 文档
EMQX 设备公私钥交换认证插件是一个通过特殊的密钥验证机制来实现设备接入能力的基础插件。

### 构建
- git clone emqx `v4.3.8` 的源码;
- 把插件代码复制到 emqx 源码目录下的 `apps` 目录下;
- 执行`make`构建。
```shell
git clone https://github.com/emqx/emqx.git
cd emqx
git fetch origin v4.3.8:v4.3.8
git checkout v4.3.8
cd apps
git clone https://github.com/wwhai/emqx_auth_kpair.git
```
然后记得注册插件进去，打开 emqx 源码目录下的 `rebar.config.erl` 在 `246` 行的列表内加入插件名称即可.
最后执行
```sh
make clean
make
```
## 原理
设备创建成功后数据库生成记录：

| 字段     | 类型                                |     |
| -------- | ----------------------------------- | --- |
| clientid | 是个字符串ID，标识唯一设备          |     |
| pubk     | 一个hash字符串，标识设备的公钥      |     |
| privk    | 一个hash字符串，标识设备的私钥      |     |
| token    | token = hash(pubk, privk, clientid) |     |

上面几个字符串构成了认证的核心字段，我们先拿MQTT协议来接入：

首先 Mqtt客户端用`clientid`当 `clientid`, `pubk` 当 `username`，`token` 当 `password`,向服务端发送请求，服务端会从数据库里面查询拥有这个 `clientid` 的记录, 用它的私钥来计算一个串：
$$
S= hash(privk, pubk, clientId)
$$
最后判断 `S` 是否等于 `token` 即可, 伪代码描述：

```vb
Auth(clientid, pubk, token){
    privk = query("select privk from tb_device where clientid=#{clientid}")
    s = hash(privk, pubk, clientId)
    if s == token {
    	access
    }
    else{
    	deny
    }
}
```



## 社区
- QQ群：475512169
- 博客：https://wwhai.github.io