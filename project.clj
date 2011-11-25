(defproject korma_postgis "0.0.1"
  :description "some conviance for workting with korma and postgis databases"
  :java-source-path "src"
  :dependencies [[org.clojure/clojure "1.3.0"]
                ;[korma "0.3.0-alpha3-ab"]
                 [korma "0.3.0-alpha4"]
                 [com.vividsolutions/jts "1.12"]
                 [org.postgis/postgis-jdbc "1.3.3"
                    :exclusions [postgresql/postgresql] ]]
   
  :autodoc {:name "Korma Postigs" :page-title "Korma Postgis Docs"}  
  :dev-dependencies [[org.clojars.rayne/autodoc "0.8.0-SNAPSHOT"]
		                 [enlive "1.0.0"] ;for the documentation scraper
                     [postgresql/postgresql "9.0-801.jdbc4"]]
)
