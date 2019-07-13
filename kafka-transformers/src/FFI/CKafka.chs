{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module FFI.CKafka where

#include "librdkafka/rdkafka.h"

{#enum rd_kafka_type_t as ^ {underscoreToCase} deriving (Show, Eq) #}
{#enum rd_kafka_conf_res_t as ^ {underscoreToCase} deriving (Show, Eq) #}
{#enum rd_kafka_resp_err_t as ^ {underscoreToCase} deriving (Show, Eq) #}
{#enum rd_kafka_timestamp_type_t as ^ {underscoreToCase} deriving (Show, Eq) #}

{#pointer *rd_kafka_timestamp_type_t as RdKafkaTimestampTypeTPtr foreign -> RdKafkaTimestampTypeT #}

{#fun pure unsafe rd_kafka_version_str as ^
    {} -> `String' #}
