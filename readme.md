# Introduction #
this project augments the tasty [korma](https://github.com/ibdknox/korma) sql library for [clojure](http://clojure.org/) with some convenience for working with [postgis](http://postgis.refractions.net/) databases.
This includes conversions from and to JavaTopologySuite geometries to keep you sane
and "aliases" for the postgis functions, so you can type less and your editor might even
provide auto-complete.  

# Warning #
korma.postgis is in no way production ready
and has currently only been run with postgres 9.0/postgis 1.5.
other versions might work, but they also might blow up in your face and even if you use postgis 1.5: you might want to keep your distance.  
not everything has been tested and most of the code was generated.  

# Example/excuse for an tutorial #
```clojure
    ;....
    (use 'korma.postgis)
    ;... define postgres-db ... please note, that the postgres jdbc driver is not a korma.postgis dependency
    ;                           the postgis extension however is included

    (register-types db) ;register the postgis-types in the db-pool ... only needed for the transform-postgis function

    (defentity geom-ent
        (prepare   prepare-postgis) ;this allows you to use jts-geometries in your insert/update statements
        (transform tranform-postgis) ;this converts PGGeometry to JTS-Geometries -> "SELECT geom FROM geom_table" gets you JTS-Geometries), if you called register-types
    )

    (def point (new Point 7.7 8.8)) ;define some JTS point

    ;simple example
    (select geom-ent
        (where (st-within :geom (st-buffer [point 4326] 100))
        (limit 100) )

    ;the spatial-functions take either a keyword (-> column) or a seq with [geometry, srid] for the geometry parameter
    ;geometry can either be a JTS geometry or a WKT string


    ;generated sql examples

    (sql-only
        (select geom-ent
          (fields :id (st-x :geom) )
          (where (st-intersects (st-buffer ["POINT(6.6 7.7)" 4326] 10 ) :geom )))
    )
    ;generates: SELECT "geom-ent"."id", ST_X("geom-ent"."geom") FROM "geom-ent" WHERE ST_INTERSECTS(ST_BUFFER(ST_GEOMFROMTEXT(?, ?), ?), "geom-ent"."geom")

    ;for a 'spatial join' you have to add the extra table using (from :table)
    (sql-only
         (select korma_postgis_point
                   (from :korma_postgis_poly)
                   (where (st-within :geom :korma_postgis_poly.geom) ))
    )
    ;generates: SELECT "korma_postgis_point".* FROM "korma_postgis_point", "korma_postgis_poly" WHERE ST_WITHIN("korma_postgis_point"."geom", "korma_postgis_poly"."geom")

```

# Todos #
* currently the input geometries are converted to WKT which is obviously a stupid idea from an performance viewpoint .. so change to WKB
* test all those generated sql-function macros/aliases
* maybe attach srid metadata to an entity, so there is no need to provide it every time ? (not sure about this .. having the spatial refrence explicit prevents errors)

# Features #
* allow input of JTS geometries or WKT strings
* convert PGgeometry to JTS
* generated macros for most of postgis' "ST_" functions
* macros include basic documentation, so you can use e.g. (doc st-buffer) to look at postgis documentation - which was scraped from postgis' function reference page


# Extension points #
there is currently one multimethod that can be used to customize korma.postgis to your needs:
```clojure
(defmulti to-wkt class)
```
converts the input-parameter into WKT to interface with the database (this WILL change to to-wkb)
usefull if you don't use JTS as your geometry library (although you should)

# I don't use postgis, i use oracle|mssql|arcsde ! #
transforming korma.postgis into korma.spatial that handles the different spatial sql databases (ms sql server, oracle spatial/locator, arcsde/st_geometry).  
is currently no short term goal.  
generated queries need to be a bit different depending on the database.   Esri's ST_GEOMETRY in oracle for example returns 0|1 instead of a boolean,
so we would have to generate
    [...] WHERE ST_INTERSECTS(table1.shape, table2.shape) = 1
    instead of
    [...] WHERE ST_INTERSECTS(table1.shape, table2.shape)


this might happen, after the api is finalized


## License ##

Distributed under the Eclipse Public License, the same as Clojure.