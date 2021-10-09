use emqx_auth;
--
-- Table structure for table `acl`
--
DROP TABLE IF EXISTS `acl`;
CREATE TABLE `acl` (
  `id` int unsigned NOT NULL AUTO_INCREMENT,
  `allow` int DEFAULT NULL COMMENT '0: deny, 1: allow',
  `ip` varchar(60) DEFAULT NULL COMMENT 'Ip',
  `username` varchar(100) DEFAULT NULL COMMENT 'Username',
  `client_id` varchar(100) DEFAULT NULL COMMENT 'ClientId',
  `access` int NOT NULL COMMENT '1: subscribe, 2: publish, 3: pubsub',
  `topic` varchar(100) NOT NULL DEFAULT '' COMMENT 'Topic Filter',
  PRIMARY KEY (`id`)
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 COLLATE = utf8mb4_0900_ai_ci;
--
-- Table structure for table `device`
--
DROP TABLE IF EXISTS `device`;
CREATE TABLE `device` (
  `id` bigint NOT NULL AUTO_INCREMENT COMMENT 'PK: id',
  `client_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT 'MQTT client_id',
  `public_key` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT 'public key: MQTT username',
  `private_key` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT 'private key',
  `token` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT 'token: MQTT password',
  `ip` varchar(12) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT 'ip blacklist， * or an IPV4',
  PRIMARY KEY (`id`)
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 COLLATE = utf8mb4_general_ci;