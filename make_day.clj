(require '[babashka.deps :as deps])

(require '[selmer.parser :refer [render]])

(require '[org.httpkit.client :as http])

(deps/add-deps '{:deps {org.clojars.tanelso2/clj-toolbox {:mvn/version "0.2.2"}
                        enlive/enlive {:mvn/version "1.1.6"}}})

(require '[clj-toolbox.prelude :refer :all])
(require '[clojure.repl :refer [doc]])

(require '[clj-toolbox.files :as files])

(require '[net.cgrand.enlive-html :as html])

(defn render-file [filename args]
  (render (slurp filename) args))

(def cookie-file ".cookie-cache")

(defn get-cookie
  []
  (if (files/file-exists? cookie-file)
      (slurp cookie-file)
      nil))

(defn prompt-cookie
  []
  (println "Please paste another cookie: ")
  (let [ret (str/trim (read-line))]
    (spit cookie-file ret)
    ret))

(defn get-or-prompt-cookie
  []
  (let [ret (get-cookie)]
    (if (nil? ret)
      (prompt-cookie)
      ret)))

(def test-description-link "https://adventofcode.com/2021/day/1")
(def test-input-link "https://adventofcode.com/2021/day/1/input")

(defn make-aoc-request [url]
  (let [cookie (get-or-prompt-cookie)
        resp @(http/get url {:headers {"Cookie" (str "session=" cookie)}})]
    (println (str "Url is " url))
    (if (not= 200 (:status resp))
      (throw (Exception. (str "Non-200 error raised: " (:status resp))))
      (:body resp))))

(defn get-input-for-day [n]
  (let [url (if (= n 0)
              test-input-link
              (str "https://adventofcode.com/2022/day/" n "/input"))]
    (make-aoc-request url)))

(defn get-description-page-for-day [n]
  (let [url (if (= n 0)
              test-description-link
              (str "https://adventofcode.com/2022/day/" n))]
    (make-aoc-request url)))

(defn get-description-for-day [n]
  (let [page (get-description-page-for-day n)]
    (-> page
        (html/html-resource))))

(defn write-input [n]
  (let [filename (str "inputs/day" n ".txt")]
    (spit filename (get-input-for-day n))))

(defn make-test-file [n]
  (let [template "templates/test_dayx.ml"
        rendered (render-file template {:day n})
        filename (str "test/test_day" n ".ml")]
    (spit filename rendered)))

(defn make-lib-file [n]
  (let [template "templates/dayx.ml"
        rendered (render-file template {:day n})
        filename (str "lib/day" n ".ml")]
    (spit filename rendered)))

(defn make-day [n]
  (write-input n)
  (make-test-file n)
  (make-lib-file n))

(defn -main [& args]
  (let [num (-> (first args)
                (parse-int))]
    (if (some? num)
      (do
        (println (str "Making day " num))
        (make-day num))
      (println "Usage: $FILE <num>"))))
