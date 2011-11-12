(defproject korma_postgis "1.0.0-SNAPSHOT"
  :description "some conviance for workting with korma and postgis databases"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [korma "0.2.2-SNAPSHOT"]]
   
  :autodoc {:name "Korma Postigs" :page-title "Korma Postgis Docs"}  
  :dev-dependencies [[org.clojars.rayne/autodoc "0.8.0-SNAPSHOT"]
		     [enlive "1.0.0"]
                     [postgresql/postgresql "9.0-801.jdbc4"]]
                     
)
