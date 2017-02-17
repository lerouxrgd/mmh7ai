# mmh7ai

Work in progress.

## Usage

At the REPL:

```clj
(require '[mmh7ai.db :as db])

;; scrap data if needed, load db from disk
(db/init-db!)

;; querying db with datalog
(db/query
 '[:find ?name ?tier
   :where
   [?e :faction :dungeon]
   [?e :upgraded true]
   [?e :tier ?tier]
   [?e :name ?name]])
```

