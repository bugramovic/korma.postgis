# Introduction #
this project augments the excellent korma sql library for clojure
with some conviniance for working with postgis databases.
This includes conversions from and to JavaTopologySuite geometries to keep you sane
and aliases for the postgis functions, so you can type less and your editor might even
provide auto-complete

# Warning #
korma.postgis is in no way production ready and has currently only been run with postgres 9.1/postgis 1.5
other versions might work, but they also might blow up in your face
and even if you use postgis 1.5: you might want to keep your distance

## Examples ##
(use 'korma.postgis)
;... define db ...
(defentity planet_osm_polygon
    (prepare   prepare-jts) ; not sure if this would be such a good idea if prepare is applied to selects and things get messy
    (transform tranform-to-jts) ;this is optional and simply transforms and PGGeometry to their JTS equivalent
)

(def point (new Point 7.7 8.8))

(select planet_osm_road
    (where (st-within :way (st-buffer [point 4326] 100))
    (limit 100) )
;the spatial functions either take a keyword, a seq with geometry+srid or a number as their parameter(s)

;generated sql: TODO

TODO: show insert/update examples! -> well use prepare !
(insert into planet_osm_road ....

# Features #

## todos ##
* currently the input geometries are converted to WKT which is obviously a stupid idea from an performance viewpoint .. so change to WKB
* make sure everything works well with more complex queries
* test all those generated sql-function macros/aliases
* come up with a sane way to define tests, that might also run on other machines (test data...)

## extension points ##
there is currently one multimethod that can be used to customize korma.postgis to your needs:
(defmulti to-wkt class)
converts the input-parameter into WKT to interface with the database (this WILL change to to-wkb)
usefull if you don't use JTS as your geometry library (although you should)

## Plans? ##
transforming korma.postgis into korma.spatial that handles the different spatial sql databases (ms sql server, oracle spatial, arcsde/st_geometry)
is currently no short term goal
but it shouldn't be that hard, as all implement the same standard (well, in theory at least)



## License ##

Distributed under the Eclipse Public License, the same as Clojure.
