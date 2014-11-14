(defproject korma_postgis "0.0.1"
  :description "some conviance for workting with korma and postgis databases"
  :java-source-path "src"
  :dependencies [[korma "0.4.0"]
                 [com.vividsolutions/jts "1.13"]
                 [org.postgis/postgis-jdbc "1.3.3"
                    :exclusions [postgresql/postgresql] ]]

  :autodoc {:name "Korma Postigs" :page-title "Korma Postgis Docs"}
  :dev-dependencies [[org.clojars.rayne/autodoc "0.8.0-SNAPSHOT"]
		                 [enlive "1.0.0"] ; for the documentation scraper
                     [org.postgresql/postgresql "9.3-1100-jdbc4"]]
)
