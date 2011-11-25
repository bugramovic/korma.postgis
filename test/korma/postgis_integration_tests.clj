(ns korma.postgis-integration-tests
  (:use [korma.postgis])
  (:use [korma.db])
  (:use [korma.core])
  (:use [clojure.test])
  (:use [clojure.stacktrace])
  (:require [clojure.java.jdbc :as jdbc])
  (:import [com.vividsolutions.jts.geom.Geometry])
  (:import [com.vividsolutions.jts.geom.Polygon])
  (:import [com.vividsolutions.jts.io.WKTWriter] ) )

;tests that actually use a db
; -> requires a running postgis-installation

;TODO move connection-parameters into a separate file
(defdb osm {:classname "org.postgresql.Driver"
                  :subprotocol "postgresql"
                  :subname "osm"
                  :user "osm"
                  :password "osm"})

(def pool (get-connection osm))

(defn create-table [table type]
  (.executeUpdate (.createStatement (.getConnection (:datasource pool)) )
     (str "create table " table "(id serial, name varchar(200) )")
  )
  (.executeQuery (.createStatement (.getConnection (:datasource pool)) )
    (str "select AddGeometryColumn('" table "', 'geom', 4326, '" type "', 2)" ))
)

(defn drop-table [table]
  (.executeUpdate (.createStatement (.getConnection (:datasource pool)) )
     (str "drop table " table)
  ))

(defentity korma_postgis_point
  (fields :id :name :geom)
  (prepare prepare-postgis)
  (transform transform-postgis)
)

(defentity korma_postgis_poly
  (fields :id :name :geom)
  (prepare prepare-postgis)
  (transform transform-postgis)
)

(register-types osm)

(defn create-test-tables []
  (create-table "korma_postgis_point" "POINT")
  (create-table "korma_postgis_poly" "POLYGON"))

(defn drop-test-tables []
  (drop-table "korma_postgis_point")
  (drop-table "korma_postgis_poly"))

(defn fill-test-tables []
  (let [point (from-wkt "POINT(6.6 7.7)")
        poly  (from-wkt "POLYGON((5.5 5.5, 5.5 8.8, 8.8 8.8, 8.8 5.5, 5.5 5.5))")]
    (insert korma_postgis_point
      (values {:geom [point 4326] :name "jts-point"}))
    ;(insert korma_postgis_point
    ;  (values {:geom ["POINT(8.7 8.7)" 4326] :name "wkt-point"}))
    (insert korma_postgis_poly
      (values {:geom [poly 4326] :name "jts-polygon"}))
  ))

(defn test-fixture [f]
  (try
    (create-test-tables)
    (fill-test-tables)
    (f)
    (catch Exception e (println e))
    (finally
      (drop-test-tables) )))

(deftest insert-ok
  (is (= 1 (count (select korma_postgis_point))))
  (is (= 1 (count (select korma_postgis_poly))))
)

(deftest st-buffer-intersects
  (is (= 1 (count (select korma_postgis_point
                   (where (st-intersects (st-buffer [ (from-wkt "POINT(7.0 7.0)") 4326] 1.0) :geom) )))))
  (is (= 0 (count (select korma_postgis_point
                   (where (st-intersects (st-buffer [ (from-wkt "POINT(7.0 7.0)") 4326] 0.1) :geom) )))))
)

(deftest join-inside
  (is (= 1 (count (select korma_postgis_point
                   (from :korma_postgis_poly)
                   (where (st-within :geom :korma_postgis_poly.geom) )) )) )
  )

(use-fixtures :once test-fixture)

(run-tests)