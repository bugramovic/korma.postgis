(ns korma_postgis.test.reference-scraper
  (:use [clojure.string :only (join)])
  (:require [net.cgrand.enlive-html :as html]))

; not really a test but the scraper for the postgis-reference-documentation
; to create the macros that act as function-aliases

(def base-url "http://postgis.refractions.net/documentation/manual-1.5/")
(def url (java.net.URL. (str base-url "reference.html")) )

(def page (html/html-resource url) )

(defstruct pgfn :name :short-doc :link )

(defn macro-str [m param-count]
  "takes a pgfn and the number of parameters and returns the string for the macro
   output is for example:
   (defmacro st-within [arg0 arg1]
    `(sqlfn st_within (conv-param ~arg0) (conv-param ~arg1)))
  "
  (let [param-idx (take param-count (iterate inc 0))
        param-vec-str (str "[" (join (interpose " " (map #(str "arg" %) param-idx)  ) ) "]")
        param-body (str (join (interpose " " (map #(str "(conv-param ~arg" % ")") param-idx) ) ) )
        macro-name (fn [s] (. (. s toLowerCase) replace  "_" "-") )
       ]
    (str "(defmacro " (macro-name (:name m)) " " param-vec-str "\n"
      "   \"" (. (:short-doc m) trim) "\n" "  full doc: " base-url (:link m) "\"\n"
      "  `(sqlfn " (:name m) " " param-body "))"
      )
  ) )

;test-run
; (println (macro-str {:name "ST_Transform" :link "http://shit" :short-doc "fuck this"} 2) )

(def sect1 (html/select page [:div.sect1]))

(defn sect-by-title [title sect1-seq]
  "returns a seq of sect1-nodes that have the given title (title as in the :name attribute within the :a)"
  (filter #(= title (:name (:attrs (first (html/select % [:h2 :a])) ))) sect1-seq) )

(defn- link [node]
      (:href (:attrs (first node))))
(defn- name [node]
      (first (:content (first node))))

(defn- doc-str [node]
  (first (:content (first node))))

(defn pgfn-from-dt-node [dt-node]
  "creates a pgfn from an dt-node within a struct1-node"
  (let [link-node (html/select dt-node [:a])
        doc-node (html/select dt-node [:span.refpurpose])
        ]
    pgfn {:link (link link-node) :name (name link-node) :short-doc (doc-str doc-node) } ))

(defn to-pgfn [sect]
  "converts all elemets within the given sect1 into pgfn-structs"
  (let [dt-nodes (html/select sect [:div.toc :dt])]
    (map pgfn-from-dt-node dt-nodes)))

(defn to-str [lst] (map #(. (str %) replace ":" "") lst ))

(defn filter-whitelist [pgfn-seq whitelst] (
  filter #(contains? (set whitelst) (:name %)) pgfn-seq))

(defn filter-blacklist [pgfn-seq blacklist] (
  filter #(not (contains? (set blacklist) (:name %))) pgfn-seq))

(defn print-macros [pgfn-seq param-count] (println (interpose "\n\n" (map #(macro-str % param-count) pgfn-seq))) )

; manually run some of these to create the macro code
(def geom-accessor-2-params (to-str #{:ST_PointN :ST_Zmflag})) ;all others: 1 parameter
;(def pgfns (to-pgfn (sect-by-title "Geometry_Accessors" sect1 )))
;(def pgfns (to-pgfn (sect-by-title "Geometry_Outputs" sect1 ))) ;all 1 parameter
;(def pgfns (to-pgfn (sect-by-title "Spatial_Relationships_Measurements" sect1)))
(def rel-meas-1-param (to-str #{:ST_Area :ST_Centroid :ST_HasArc :ST_Length :ST_Length3D}))
(def misc-1-param (to-str #{:ST_XMax :ST_YMax :ST_ZMax :ST_XMin :ST_YMin :ST_ZMin  }))
(def pgfns-misc (to-pgfn (sect-by-title "Miscellaneous_Functions" sect1)))
;(print-macros (filter-whitelist pgfns geom-rel-meas-1-param ) 1)
;(print-macros (filter-blacklist pgfns rel-meas-1-param ) 2)
;(print-macros (filter-whitelist pgfns-misc misc-1-param ) 1)
;(print-macros (filter-whitelist pgfns-misc ["ST_Extent"] ) 1)

(def pgfns-proc (to-pgfn (sect-by-title "Geometry_Processing" sect1 )))
(print-macros (filter-whitelist pgfns-proc [ "ST_ConvexHull" ]) 1)
(print-macros (filter-whitelist pgfns-proc ["ST_Intersection" "ST_Buffer" "ST_Simplify" "ST_Union" "ST_Intersection"]) 2)



;(filter-whitelist pgfns geom-rel-meas-1-param)
;(filter-blacklist pgfns geom-rel-meas-1-param)
;all others have 2 parameters











;(def geom-rel-meas-2-param ["ST_Azimuth" "ST_ClosestPoint" "ST_Contains" "ST_ContainsProperly" "ST_Covers" "ST_CoveredBy" "ST_Crosses"
;                             "ST_LineCrossingDirection" "ST_Disjoint" "ST_Distance" "ST_HausdorffDistance" "ST_MaxDistance"])



;(to-pgfn (filter-sect-by-title "Geometry_Accessors" sect1 ) )

