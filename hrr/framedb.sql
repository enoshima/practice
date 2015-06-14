CREATE DATABASE IF NOT EXISTS TEST DEFAULT CHARACTER SET UTF8;

DROP TABLE IF EXISTS KAGRA.framedb;
CREATE TABLE KAGRA.framedb (
      frame_id      INT UNSIGNED auto_increment PRIMARY KEY
    , fname         VARCHAR(100) NOT NULL
    , gps_start     INT NOT NULL DEFAULT 0
    , gps_stop      INT NOT NULL DEFAULT 0
    , chname        VARCHAR(100) NOT NULL
    , sampling_rate INT NOT NULL DEFAULT 0
    , dq_flag       INT(1) NOT NULL DEFAULT 0
);

INSERT INTO KAGRA.framedb
    (frame_id, fname, gps_start, gps_stop, chname, sampling_rate, dq_flag)
    VALUES
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209036-32.gwf',1113209036,1113209036+32,'PEM-EX_MIC_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209036-32.gwf',1113209036,1113209036+32,'PEM-EX_MAG_Z_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209036-32.gwf',1113209036,1113209036+32,'PEM-EX_MAG_Y_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209036-32.gwf',1113209036,1113209036+32,'PEM-EX_MAG_X_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209036-32.gwf',1113209036,1113209036+32,'PEM-EX_ACC_NO2_Z_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209036-32.gwf',1113209036,1113209036+32,'PEM-EX_ACC_NO2_Y_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209036-32.gwf',1113209036,1113209036+32,'PEM-EX_ACC_NO2_X_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209067-32.gwf',1113209067,1113209067+32,'PEM-EX_MIC_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209067-32.gwf',1113209067,1113209067+32,'PEM-EX_MAG_Z_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209067-32.gwf',1113209067,1113209067+32,'PEM-EX_MAG_Y_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209067-32.gwf',1113209067,1113209067+32,'PEM-EX_MAG_X_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209067-32.gwf',1113209067,1113209067+32,'PEM-EX_ACC_NO2_Z_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209067-32.gwf',1113209067,1113209067+32,'PEM-EX_ACC_NO2_Y_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209067-32.gwf',1113209067,1113209067+32,'PEM-EX_ACC_NO2_X_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209099-32.gwf',1113209099,1113209099+32,'PEM-EX_MIC_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209099-32.gwf',1113209099,1113209099+32,'PEM-EX_MAG_Z_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209099-32.gwf',1113209099,1113209099+32,'PEM-EX_MAG_Y_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209099-32.gwf',1113209099,1113209099+32,'PEM-EX_MAG_X_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209099-32.gwf',1113209099,1113209099+32,'PEM-EX_ACC_NO2_Z_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209099-32.gwf',1113209099,1113209099+32,'PEM-EX_ACC_NO2_Y_FLOOR-RAW',2048,4),
        (null,'/data/kagra/xend/test/R0201/K-K1_R-1113209099-32.gwf',1113209099,1113209099+32,'PEM-EX_ACC_NO2_X_FLOOR-RAW',2048,4)
        ;

GRANT ALL PRIVILEGES ON KAGRA.framedb TO 'haskal01'@'localhost';



