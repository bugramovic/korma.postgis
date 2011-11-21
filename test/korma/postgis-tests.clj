(ns korma.postgis-tests
  (:use [korma.postgis])
  (:use [korma.db])
  (:use [korma.core])
  (:use [clojure.test])
  )

(defentity geom-ent
  (fields :id :geom)
  )

(def point (from-wkt "POINT(700377.953080346 6308207.09608293)"))

(deftest intersect-buffer
(is (.equalsIgnoreCase "SELECT \"geom-ent\".* FROM \"geom-ent\" WHERE ST_INTERSECTS(\"geom-ent\".\"geom\", ST_BUFFER(ST_GEOMFROMTEXT(?, ?), ?))"
      (sql-only
        (select geom-ent
          (where (st-intersects :geom (st-buffer [point 4326] 10 ) ) )
      ))
    )) )

(deftest buffer-intersect
(is (.equalsIgnoreCase "SELECT \"geom-ent\".\"id\", ST_X(\"geom-ent\".\"geom\") FROM \"geom-ent\" WHERE ST_INTERSECTS(ST_BUFFER(ST_GEOMFROMTEXT(?, ?), ?), \"geom-ent\".\"geom\")"
      (sql-only
        (select geom-ent
          (fields :id (st-x :geom) )
          (where (st-intersects (st-buffer ["POINT(6.6 7.7)" 4326] 10 ) :geom ) )
      ))
    )))

(run-tests)