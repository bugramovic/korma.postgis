(ns korma.postgis-repl-tests
  (:use [korma.postgis])
  (:use [korma.db])
  (:use [korma.core])
  (:use [clojure.stacktrace])
  (:require [clojure.java.jdbc :as jdbc])
  (:import [com.vividsolutions.jts.geom.Geometry])
  (:import [com.vividsolutions.jts.geom.Polygon])
  (:import [com.vividsolutions.jts.io.WKTWriter] ) )

(defdb playground {:classname "org.postgresql.Driver"
                  :subprotocol "postgresql"
                  :subname "osm"
                  :user "osm"
                  :password "osm"})

(def conn (get-connection playground))

(jdbc/with-connection conn
   (jdbc/with-query-results rs ["drop table korma_spatial"])
   (jdbc/with-query-results rs ["create table korma_spatial (id serial, name varchar(200) )"])
)

(def drop-table []
  (jdbc/with-connection conn
     (jdbc/do-commands "drop table korma_postgis_point")
  )
)

(defn create-table []
  (jdbc/with-connection conn
     (jdbc/do-commands "create table korma_postgis_point (id serial, name varchar(200) )")
  )
)

(defn make-spatial []
  (.executeQuery (.createStatement (.getConnection (:datasource conn)) ) "select AddGeometryColumn('korma_postgis_point', 'geom', 4326, 'POINT', 2)")
)
;(defn make-spatial []
;  (jdbc/with-connection conn
;     (jdbc/do-commands "select AddGeometryColumn('korma_postgis_point', 'geom', 4326, 'POINT', 2)" )
;  )
;)

(register-types playground)

(exec-raw "INSERT INTO \"planet_osm_point\" (\"osm_id\") VALUES (99999999)")

(exec-raw "CREATE TABLE korma_spatial(
            id serial CONSTRAINT unittest_pk PRIMARY KEY,
            name varchar(200) NOT NULL
            )")

(def poly (from-wkt "POLYGON((0 0, 10000000 0, 10000000 10000000, 0 10000000, 0 0))"))

(def point (from-wkt "POINT(700377.953080346 6308207.09608293)"))

(def wkt (to-wkt poly))

(defentity planet_osm_polygon)

(defentity planet_osm_point
  (fields :osm_id :way)
  (transform transform-postgis)
  (prepare prepare-postgis)
  )


(sql-only
(insert planet_osm_point
  (values [{:osm_id 99999999 :way [point 900913] }] )
  )
)

(def point-res
(select planet_osm_point
     (fields :osm_id (st-x :way) (st-y :way) :way)
    (where (st-within :way (st-buffer [point 900913] 1000.0) ) )
    (limit 10)
  )
)


(sql-only
  (select planet_osm_polygon
    (fields :osm_id)
    (where (st-within :way [poly 900913]) )
    (limit 10))
)


(println
(sql-only
  (select planet_osm_point
     (fields :osm_id (st-x :way) (st-y :way) :way)
    (where (st-within :way (st-buffer [point 900913] 1000.0) ) )
    (limit 10)
  )
)
)


(macroexpand
  (st-buffer [point 900913] 100.0)
;(st-within (st-buffer [point 900913] 100.0)  :way )
)

;(st-within :way (st-buffer [point 900913] 100.0))
