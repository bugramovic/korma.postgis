(ns korma.postgis
  (:use [korma.core])
  (:use [korma.db])
  (:import [org.postgis.PGgeometry])
  (:import [com.vividsolutions.jts.geom.Geometry])
  (:import [com.vividsolutions.jts.io.WKBReader])
  (:import [com.vividsolutions.jts.io.WKTWriter] ) )

(defn register-types
  "Registers the postgis-types for the connections in the pool via a c3p0 ConnectionCustomizer"
  [db]
  (.setConnectionCustomizerClassName (:datasource (get-connection db) )
    "korma.PostgisConnCustomizer") )


; allow user to register their own converters
; default is a stupid workaround till the dispatch is fixed
(defmulti to-wkt class
   :default :jts)

; make sure that intermediate query-structures are simply returned
(defmethod to-wkt clojure.lang.MapEntry [m] m)

(defmethod to-wkt :jts [geom]
  (let [writer (new com.vividsolutions.jts.io.WKTWriter)]
    (. writer write geom)))

; we get a string -> assume we get WKT and pass through
(defmethod to-wkt String [wkt] wkt)

(defn from-wkt
  "reads wkt and outputs a jts-geometry"
  [wkt]
  (let [reader (new com.vividsolutions.jts.io.WKTReader)]
    (. reader read wkt)))

(defmacro convert-to-sql
  "expands to (sqlfn st_geomfromtext ..) to convert a given geometry to a postgis-geometry
   uses the multimethod to-wkt to generate wkt, so a [..].jts.geom.Geometry or a WKT-String are valid as default
   example: (convert-to-sql (new Point 10 10) 4326)"
  [geom srid]
   ;can use "int ~srid", need to use Integer/valueOf ... see http://dev.clojure.org/jira/browse/CLJ-820
  `(sqlfn st_geomfromtext (to-wkt ~geom) (Integer/valueOf ~srid))
  )

(defn conv-param
  "converts a parameter to be used in an postgis - function
   if a seq is passed: first must be something to-wkt can handle (e.g. a jts-geometry) and the second one must be a integer (srid) "
   [p]
  (cond
    (keyword? p) p
    (map? p) p
    (coll? p) (convert-to-sql (first p) (last p))
    (integer? p) (Integer/valueOf p)
    (float? p) (Double/valueOf p)
    :else (throw (IllegalArgumentException. "Needs to be a keyword, map, int, float or a seq with geometry and srid e.g. [my-jts-point 4326]"))
    ))


; convert the PostGIS JDBC type to JTS
(defmulti pg-to-jts class :default :no-op)
(defmethod pg-to-jts :no-op [x] x)
(defmethod pg-to-jts org.postgis.PGgeometry [pg-geom]
  (let [buffer (new java.lang.StringBuffer)
        geom (.getGeometry pg-geom)
        reader (new com.vividsolutions.jts.io.WKTReader)]

    (.outerWKT geom buffer)
    (.read reader (.toString buffer))))

(defn transform-postgis
  "tranform-function to be used in entities
   tranforms the types in the result by the db-to-jts multimethod"
  [results]
  (let [apply-on-map (fn [m] (into {} (for [[k v] m] [k (pg-to-jts v)])) ) ]
    (cond (nil? results) nil
          (map? results) (apply-on-map results)
          ;(coll? results) (map apply-on-map results) ;this should be useless, i guess
          :else results ) ))

(defn prepare-geom-vec [v]
  (if (and (vector? v)
           (= 2 (count v))
           (or (instance? com.vividsolutions.jts.geom.Geometry (first v))
               (string? (first v))))
    (conv-param v) v ))

;convert a jts geometry, so we can do an insert/update
;(defmulti jts-to-pg class :default :no-op)
;(defmethod jts-to-pg :no-op [x] x)
;(defmethod jts-to-pg com.vividsolutions.jts.geom.Geometry [jts-geom]
;  (convert-to)
;  (let [wkt (.write (new com.vividsolutions.jts.io.WKTWriter) jts-geom) ]
;    (st-geo)
;    (org.postgis.PGgeometry/geomFromString wkt)
;  ))

;**** doesn't work, postgres driver can't infer the type for some reason
;     not sure why, as the type should be registered
;(defmethod jts-to-pg com.vividsolutions.jts.geom.Geometry [jts-geom]
;  (let [wkt (.write (new com.vividsolutions.jts.io.WKTWriter) jts-geom) ]
;    (org.postgis.PGgeometry/geomFromString wkt)
;  ))

(defn prepare-postgis [ent]
  (let [apply-on-map (fn [m] (into {} (for [[k v] m] [k (prepare-geom-vec v)])) ) ]
    (cond (nil? ent) nil
      (map? ent) (apply-on-map ent)
      (coll? ent) (map apply-on-map ent) ) ;TODO: this condition is probably useless
    )
  )

; ***********
; auto generated macros for the postgis functions
; half automaticly created by test/korma/reference_scraper.clj
; ***********

(defmacro geometry
  [arg0]
  `(sqlfn Geometry (conv-param ~arg0)))

; *******************
; Geometry accessors
; ******************

(defmacro geometrytype
   "- Returns the type of the geometry as a string. Eg:
			'LINESTRING', 'POLYGON', 'MULTIPOINT', etc.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/GeometryType.html"
  [arg0]
  `(sqlfn GeometryType (conv-param ~arg0)))

 (defmacro st-boundary
   "- Returns the closure of the combinatorial boundary of this
			Geometry.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Boundary.html"
  [arg0]
  `(sqlfn ST_Boundary (conv-param ~arg0)))

 (defmacro st-coorddim
   "- Return the coordinate dimension of the ST_Geometry value.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_CoordDim.html"
  [arg0]
  `(sqlfn ST_CoordDim (conv-param ~arg0)))

 (defmacro st-dimension
   "- The inherent dimension of this Geometry object, which must
			be less than or equal to the coordinate dimension.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Dimension.html"
  [arg0]
  `(sqlfn ST_Dimension (conv-param ~arg0)))

 (defmacro st-endpoint
   "- Returns the last point of a LINESTRING
		geometry as a POINT.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_EndPoint.html"
  [arg0]
  `(sqlfn ST_EndPoint (conv-param ~arg0)))

 (defmacro st-envelope
   "- Returns a geometry representing the double precision (float8) bounding box of the
			supplied geometry.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Envelope.html"
  [arg0]
  `(sqlfn ST_Envelope (conv-param ~arg0)))

 (defmacro st-exteriorring
   "- Returns a line string representing the exterior ring of the POLYGON geometry. Return
				NULL if the geometry is not a polygon.  Will not work with MULTIPOLYGON
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_ExteriorRing.html"
  [arg0]
  `(sqlfn ST_ExteriorRing (conv-param ~arg0)))

 (defmacro st-geometryn
   "- Return the 1-based Nth geometry if the geometry is a
			GEOMETRYCOLLECTION, MULTIPOINT, MULTILINESTRING, MULTICURVE or MULTIPOLYGON.
			Otherwise, return NULL.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_GeometryN.html"
  [arg0]
  `(sqlfn ST_GeometryN (conv-param ~arg0)))

 (defmacro st-geometrytype
   "- Return the geometry type of the ST_Geometry value.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_GeometryType.html"
  [arg0]
  `(sqlfn ST_GeometryType (conv-param ~arg0)))

 (defmacro st-interiorringn
   "- Return the Nth interior linestring ring of the polygon geometry.
			Return NULL if the geometry is not a polygon or the given N is out
			of range.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_InteriorRingN.html"
  [arg0]
  `(sqlfn ST_InteriorRingN (conv-param ~arg0)))

 (defmacro st-isclosed
   "- Returns TRUE if the
		LINESTRING's start and end points are coincident.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_IsClosed.html"
  [arg0]
  `(sqlfn ST_IsClosed (conv-param ~arg0)))

 (defmacro st-isempty
   "- Returns true if this Geometry is an empty geometry . If
				true, then this Geometry represents the empty point set - i.e.
				GEOMETRYCOLLECTION(EMPTY).
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_IsEmpty.html"
  [arg0]
  `(sqlfn ST_IsEmpty (conv-param ~arg0)))

 (defmacro st-isring
   "- Returns TRUE if this
	  LINESTRING is both closed and simple.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_IsRing.html"
  [arg0]
  `(sqlfn ST_IsRing (conv-param ~arg0)))

 (defmacro st-issimple
   "- Returns (TRUE) if this Geometry has no anomalous geometric
				points, such as self intersection or self tangency.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_IsSimple.html"
  [arg0]
  `(sqlfn ST_IsSimple (conv-param ~arg0)))

 (defmacro st-isvalid
   "- Returns true if the
		ST_Geometry is well formed.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_IsValid.html"
  [arg0]
  `(sqlfn ST_IsValid (conv-param ~arg0)))

 (defmacro st-isvalidreason
   "- Returns text stating if a geometry is valid or not and if not valid, a reason why.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_IsValidReason.html"
  [arg0]
  `(sqlfn ST_IsValidReason (conv-param ~arg0)))

 (defmacro st-m
   "- Return the M coordinate of the point, or NULL if not
			available. Input must be a point.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_M.html"
  [arg0]
  `(sqlfn ST_M (conv-param ~arg0)))

 (defmacro st-ndims
   "- Returns coordinate dimension of the geometry as a small int.
			Values are: 2,3 or 4.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_NDims.html"
  [arg0]
  `(sqlfn ST_NDims (conv-param ~arg0)))

 (defmacro st-npoints
   "- Return the number of points (vertexes) in a geometry.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_NPoints.html"
  [arg0]
  `(sqlfn ST_NPoints (conv-param ~arg0)))

 (defmacro st-nrings
   "- If the geometry is a polygon or multi-polygon returns the number of rings.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_NRings.html"
  [arg0]
  `(sqlfn ST_NRings (conv-param ~arg0)))

 (defmacro st-numgeometries
   "- If geometry is a GEOMETRYCOLLECTION (or MULTI*) return the
			number of geometries, otherwise return NULL.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_NumGeometries.html"
  [arg0]
  `(sqlfn ST_NumGeometries (conv-param ~arg0)))

 (defmacro st-numinteriorrings
   "- Return the number of interior rings of the first polygon in
			the geometry. This will work with both POLYGON and MULTIPOLYGON types but only looks at the first polygon.
			Return NULL if there is no polygon in the
			geometry.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_NumInteriorRings.html"
  [arg0]
  `(sqlfn ST_NumInteriorRings (conv-param ~arg0)))

 (defmacro st-numinteriorring
   "- Return the number of interior rings of the first polygon in
			the geometry. Synonym to ST_NumInteriorRings.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_NumInteriorRing.html"
  [arg0]
  `(sqlfn ST_NumInteriorRing (conv-param ~arg0)))

 (defmacro st-numpoints
   "- Return the number of points in an ST_LineString or
		  ST_CircularString value.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_NumPoints.html"
  [arg0]
  `(sqlfn ST_NumPoints (conv-param ~arg0)))

 (defmacro st-srid
   "- Returns the spatial reference identifier for the ST_Geometry as defined in spatial_ref_sys table.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_SRID.html"
  [arg0]
  `(sqlfn ST_SRID (conv-param ~arg0)))

 (defmacro st-startpoint
   "- Returns the first point of a LINESTRING
	  geometry as a POINT.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_StartPoint.html"
  [arg0]
  `(sqlfn ST_StartPoint (conv-param ~arg0)))

 (defmacro st-summary
   "- Returns a text summary of the contents of the
		ST_Geometry.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Summary.html"
  [arg0]
  `(sqlfn ST_Summary (conv-param ~arg0)))

 (defmacro st-x
   "- Return the X coordinate of the point, or NULL if not
			available. Input must be a point.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_X.html"
  [arg0]
  `(sqlfn ST_X (conv-param ~arg0)))

 (defmacro st-y
   "- Return the Y coordinate of the point, or NULL if not
			available. Input must be a point.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Y.html"
  [arg0]
  `(sqlfn ST_Y (conv-param ~arg0)))

 (defmacro st-z
   "- Return the Z coordinate of the point, or NULL if not
			available. Input must be a point.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Z.html"
  [arg0]
  `(sqlfn ST_Z (conv-param ~arg0)))

;***************
;Geometry output
;***************

(defmacro st-asbinary
   "- Return the Well-Known Binary (WKB) representation of the geometry/geography without SRID meta data.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_AsBinary.html"
  [arg0]
  `(sqlfn ST_AsBinary (conv-param ~arg0)))

 (defmacro st-asewkb
   "- Return the Well-Known Binary (WKB) representation of the geometry with SRID meta data.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_AsEWKB.html"
  [arg0]
  `(sqlfn ST_AsEWKB (conv-param ~arg0)))

 (defmacro st-asewkt
   "- Return the Well-Known Text (WKT) representation of the geometry with SRID meta data.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_AsEWKT.html"
  [arg0]
  `(sqlfn ST_AsEWKT (conv-param ~arg0)))

 (defmacro st-asgeojson
   "- Return the geometry as a GeoJSON element.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_AsGeoJSON.html"
  [arg0]
  `(sqlfn ST_AsGeoJSON (conv-param ~arg0)))

 (defmacro st-asgml
   "- Return the geometry as a GML version 2 or 3 element.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_AsGML.html"
  [arg0]
  `(sqlfn ST_AsGML (conv-param ~arg0)))

 (defmacro st-ashexewkb
   "- Returns a Geometry in HEXEWKB format (as text) using either
			little-endian (NDR) or big-endian (XDR) encoding.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_AsHEXEWKB.html"
   [arg0]
  `(sqlfn ST_AsHEXEWKB (conv-param ~arg0)))

 (defmacro st-askml
   "- Return the geometry as a KML element. Several variants. Default version=2, default precision=15
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_AsKML.html"
  [arg0]
  `(sqlfn ST_AsKML (conv-param ~arg0)))

 (defmacro st-assvg
   "- Returns a Geometry in SVG path data given a geometry or geography object.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_AsSVG.html"
  [arg0]
  `(sqlfn ST_AsSVG (conv-param ~arg0)))

 (defmacro st-geohash
   "- Return a GeoHash representation (geohash.org) of the geometry.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_GeoHash.html"
  [arg0]
  `(sqlfn ST_GeoHash (conv-param ~arg0)))

 (defmacro st-astext
   "- Return the Well-Known Text (WKT) representation of the geometry/geography without SRID metadata.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_AsText.html"
  [arg0]
  `(sqlfn ST_AsText (conv-param ~arg0)))


;***************************************
; spatial relationships and measurement
; **************************************

; 1 parameter

(defmacro st-area
   "- Returns the area of the surface if it is a polygon or
				multi-polygon. For \"geometry\" type area is in SRID units.  For \"geography\" area is in square meters.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Area.html"
  [arg0]
  `(sqlfn ST_Area (conv-param ~arg0)))

 (defmacro st-centroid
   "- Returns the geometric center of a geometry.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Centroid.html"
  [arg0]
  `(sqlfn ST_Centroid (conv-param ~arg0)))

 (defmacro st-hasarc
   "- Returns true if a geometry or geometry collection contains a circular string
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_HasArc.html"
  [arg0]
  `(sqlfn ST_HasArc (conv-param ~arg0)))

 (defmacro st-length
   "- Returns the 2d length of the geometry if it is a linestring or multilinestring. geometry are in units of spatial reference and geography are in meters (default spheroid)
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Length.html"
  [arg0]
  `(sqlfn ST_Length (conv-param ~arg0)))

 (defmacro st-length3d
   "- Returns the 3-dimensional or 2-dimensional length of the geometry if it is a
			linestring or multi-linestring.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Length3D.html"
  [arg0]
  `(sqlfn ST_Length3D (conv-param ~arg0)))

; 2 parameters

(defmacro st-azimuth
   "- Returns the angle in radians from the horizontal of the vector defined by pointA and pointB
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Azimuth.html"
  [arg0 arg1]
  `(sqlfn ST_Azimuth (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-closestpoint
   "- Returns the 2-dimensional point on g1 that is closest to g2.  This is the first point of
			the shortest line.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_ClosestPoint.html"
  [arg0 arg1]
  `(sqlfn ST_ClosestPoint (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-contains
   "- Returns true if and only if no points of B lie in the exterior of A, and at least one point of the interior of B lies in the interior of A.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Contains.html"
  [arg0 arg1]
  `(sqlfn ST_Contains (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-containsproperly
   "- Returns true if B intersects the interior of A but not the boundary (or exterior). A does not contain properly itself, but does contain itself.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_ContainsProperly.html"
  [arg0 arg1]
  `(sqlfn ST_ContainsProperly (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-covers
   "- Returns 1 (TRUE) if no point in Geometry B is outside
			Geometry A. For geography: if geography point B is not outside Polygon Geography A
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Covers.html"
  [arg0 arg1]
  `(sqlfn ST_Covers (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-coveredby
   "- Returns 1 (TRUE) if no point in Geometry/Geography A is outside
			Geometry/Geography B
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_CoveredBy.html"
  [arg0 arg1]
  `(sqlfn ST_CoveredBy (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-crosses
   "- Returns TRUE if the supplied geometries have some, but not all,
	  interior points in common.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Crosses.html"
  [arg0 arg1]
  `(sqlfn ST_Crosses (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-linecrossingdirection
   "- Given 2 linestrings, returns a number between -3 and 3 denoting what kind of crossing behavior. 0 is no crossing.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_LineCrossingDirection.html"
  [arg0 arg1]
  `(sqlfn ST_LineCrossingDirection (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-disjoint
   "- Returns TRUE if the Geometries do not \"spatially intersect\" - if they do not share any space together.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Disjoint.html"
  [arg0 arg1]
  `(sqlfn ST_Disjoint (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-distance
   "- For geometry type Returns the 2-dimensional cartesian minimum distance (based on spatial ref) between two geometries in
		projected units.  For geography type defaults to return spheroidal minimum distance between two geographies in meters.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Distance.html"
  [arg0 arg1]
  `(sqlfn ST_Distance (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-hausdorffdistance
   "- Returns the Hausdorff distance between two geometries.  Basically a measure of how similar or dissimilar 2 geometries are. Units are in the units of the spatial
		reference system of the geometries.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_HausdorffDistance.html"
  [arg0 arg1]
  `(sqlfn ST_HausdorffDistance (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-maxdistance
   "- Returns the 2-dimensional largest distance between two geometries in
		projected units.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_MaxDistance.html"
  [arg0 arg1]
  `(sqlfn ST_MaxDistance (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-distance-sphere
   "- Returns minimum distance in meters between two lon/lat
				geometries. Uses a spherical earth and radius of 6370986 meters.
				Faster than ST_Distance_Spheroid, but less
				accurate. PostGIS versions prior to 1.5 only implemented for points.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Distance_Sphere.html"
  [arg0 arg1]
  `(sqlfn ST_Distance_Sphere (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-distance-spheroid
   "- Returns the minimum distance between two lon/lat geometries given a
			particular spheroid.
			PostGIS  versions prior to 1.5 only support points.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Distance_Spheroid.html"
  [arg0 arg1]
  `(sqlfn ST_Distance_Spheroid (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-dfullywithin
   "- Returns true if all of the geometries are within the specified
		distance of one another
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_DFullyWithin.html"
  [arg0 arg1]
  `(sqlfn ST_DFullyWithin (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-dwithin
   "- Returns true if the geometries are within the specified
		distance of one another. For geometry units are in those of spatial reference and For geography units are in meters and measurement is
		defaulted to use_spheroid=true (measure around spheroid), for faster check, use_spheroid=false to measure along sphere.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_DWithin.html"
  [arg0 arg1]
  `(sqlfn ST_DWithin (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-equals
   "- Returns true if the given geometries represent the same geometry. Directionality
			is ignored.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Equals.html"
  [arg0 arg1]
  `(sqlfn ST_Equals (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-intersects
   "- Returns TRUE if the Geometries/Geography \"spatially intersect\"
    - (share any portion of space) and FALSE if they don't (they are Disjoint).
			For geography -- tolerance is 0.00001 meters (so any points that close are considered to intersect)
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Intersects.html"
  [arg0 arg1]
  `(sqlfn ST_Intersects (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-length2d
   "- Returns the 2-dimensional length of the geometry if it is a
				linestring or multi-linestring. This is an alias for ST_Length
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Length2D.html"
  [arg0 arg1]
  `(sqlfn ST_Length2D (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-length-spheroid
   "- Calculates the 2D or 3D length of a linestring/multilinestring on an ellipsoid. This
			is useful if the coordinates of the geometry are in
			longitude/latitude and a length is desired without reprojection.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Length_Spheroid.html"
  [arg0 arg1]
  `(sqlfn ST_Length_Spheroid (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-length2d-spheroid
   "- Calculates the 2D length of a linestring/multilinestring on an ellipsoid. This
			is useful if the coordinates of the geometry are in
			longitude/latitude and a length is desired without reprojection.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Length2D_Spheroid.html"
  [arg0 arg1]
  `(sqlfn ST_Length2D_Spheroid (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-length3d-spheroid
   "- Calculates the length of a geometry on an ellipsoid,
			taking the elevation into account. This is just an alias for ST_Length_Spheroid.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Length3D_Spheroid.html"
  [arg0 arg1]
  `(sqlfn ST_Length3D_Spheroid (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-longestline
   "- Returns the 2-dimensional longest line points of two geometries.
		The function will only return the first longest line if more than one, that the function finds.
		The line returned will always start in g1 and end in g2.
		The length of the line this function returns will always be the same as st_maxdistance returns for g1 and g2.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_LongestLine.html"
  [arg0 arg1]
  `(sqlfn ST_LongestLine (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-orderingequals
   "- Returns true if the given geometries represent the same geometry
		and points are in the same directional order.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_OrderingEquals.html"
  [arg0 arg1]
  `(sqlfn ST_OrderingEquals (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-overlaps
   "- Returns TRUE if the Geometries share space, are of the same dimension, but are not completely contained by each other.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Overlaps.html"
  [arg0 arg1]
  `(sqlfn ST_Overlaps (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-perimeter
   "- Return the length measurement of the boundary of an ST_Surface
		  or ST_MultiSurface value. (Polygon, Multipolygon)
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Perimeter.html"
  [arg0 arg1]
  `(sqlfn ST_Perimeter (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-perimeter2d
   "- Returns the 2-dimensional perimeter of the geometry, if it
			is a polygon or multi-polygon.  This is currently an alias for ST_Perimeter.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Perimeter2D.html"
  [arg0 arg1]
  `(sqlfn ST_Perimeter2D (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-perimeter3d
   "- Returns the 3-dimensional perimeter of the geometry, if it
			is a polygon or multi-polygon.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Perimeter3D.html"
  [arg0 arg1]
  `(sqlfn ST_Perimeter3D (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-pointonsurface
   "- Returns a POINT guaranteed to lie on the surface.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_PointOnSurface.html"
  [arg0 arg1]
  `(sqlfn ST_PointOnSurface (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-relate
   "- Returns true if this Geometry is spatially related to
				anotherGeometry, by testing for intersections between the
				Interior, Boundary and Exterior of the two geometries as specified
				by the values in the intersectionMatrixPattern.  If no intersectionMatrixPattern
				is passed in, then returns the maximum intersectionMatrixPattern that relates the 2 geometries.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Relate.html"
  [arg0 arg1]
  `(sqlfn ST_Relate (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-shortestline
   "- Returns the 2-dimensional shortest line between two geometries
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_ShortestLine.html"
  [arg0 arg1]
  `(sqlfn ST_ShortestLine (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-touches
   "- Returns TRUE if the geometries have at least one point in common,
		but their interiors do not intersect.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Touches.html"
  [arg0 arg1]
  `(sqlfn ST_Touches (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-within
   "- Returns true if the geometry A is completely inside geometry B
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Within.html"
  [arg0 arg1]
  `(sqlfn ST_Within (conv-param ~arg0) (conv-param ~arg1)))

;******************************
; geometry processing functions
; *****************************

(defmacro st-convexhull
   "- The convex hull of a geometry represents the minimum convex
		geometry that encloses all geometries within the set.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_ConvexHull.html"
  [arg0]
  `(sqlfn ST_ConvexHull (conv-param ~arg0)))

(defmacro st-buffer
   "- (T) For geometry: Returns a geometry that represents all points whose distance
			from this Geometry is less than or equal to distance. Calculations
			are in the Spatial Reference System of this Geometry. For geography: Uses a planar transform wrapper.  Introduced in 1.5 support for
			different end cap and mitre settings to control shape. buffer_style options: quad_segs=#,endcap=round|flat|square,join=round|mitre|bevel,mitre_limit=#.#
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Buffer.html"
  [arg0 arg1]
  `(sqlfn ST_Buffer (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-intersection
   "- (T) Returns a geometry that represents the shared portion of geomA and geomB.  The geography implementation
					does a transform to geometry to do the intersection and then transform back to WGS84.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Intersection.html"
  [arg0 arg1]
  `(sqlfn ST_Intersection (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-simplify
   "- Returns a \"simplified\" version of the given geometry using
				the Douglas-Peucker algorithm.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Simplify.html"
  [arg0 arg1]
  `(sqlfn ST_Simplify (conv-param ~arg0) (conv-param ~arg1)))

 (defmacro st-union
   "- Returns a geometry that represents the point set union of	the Geometries.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Union.html"
  ([arg0]
  `(sqlfn ST_Union (conv-param ~arg0)))
  ([arg0 arg1]
  `(sqlfn ST_Union (conv-param ~arg0) (conv-param ~arg1))))

;*****
; MISC
;*****

(defmacro st-xmax
   "- Returns X maxima of a bounding box 2d or 3d or a geometry.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_XMax.html"
  [arg0]
  `(sqlfn ST_XMax (conv-param ~arg0)))

 (defmacro st-xmin
   "- Returns X minima of a bounding box 2d or 3d or a geometry.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_XMin.html"
  [arg0]
  `(sqlfn ST_XMin (conv-param ~arg0)))

 (defmacro st-ymax
   "- Returns Y maxima of a bounding box 2d or 3d or a geometry.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_YMax.html"
  [arg0]
  `(sqlfn ST_YMax (conv-param ~arg0)))

 (defmacro st-ymin
   "- Returns Y minima of a bounding box 2d or 3d or a geometry.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_YMin.html"
  [arg0]
  `(sqlfn ST_YMin (conv-param ~arg0)))

 (defmacro st-zmax
   "- Returns Z minima of a bounding box 2d or 3d or a geometry.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_ZMax.html"
  [arg0]
  `(sqlfn ST_ZMax (conv-param ~arg0)))

 (defmacro st-zmin
   "- Returns Z minima of a bounding box 2d or 3d or a geometry.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_ZMin.html"
  [arg0]
  `(sqlfn ST_ZMin (conv-param ~arg0)))

(defmacro st-extent
   "- an aggregate function that returns the bounding box that bounds rows of geometries.
  full doc: http://postgis.refractions.net/documentation/manual-1.5/ST_Extent.html"
  [arg0]
  `(sqlfn ST_Extent (conv-param ~arg0)))
