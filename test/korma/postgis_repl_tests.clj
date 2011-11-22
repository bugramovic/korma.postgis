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

(register-types playground)