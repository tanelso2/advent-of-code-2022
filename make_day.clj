(require '[babashka.deps :as deps])

(require '[selmer.parser :refer [render]])

(require '[org.httpkit.client :as http])

(deps/add-deps '{:deps {org.clojars.tanelso2/clj-toolbox {:mvn/version "0.2.2"}
                        enlive/enlive {:mvn/version "1.1.6"}}})

(require '[clojure.repl :refer [doc]])

(require '[clj-toolbox.files :as files])

(require '[clojure.java.shell :refer [sh]])

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

(def test-input-link "https://adventofcode.com/2021/day/1/input")

(defn get-input-for-day [n]
  (let [
        url (if (= n "0")
              test-input-link
              (str "https://adventofcode.com/2022/day/" n "/input"))
        cookie (get-or-prompt-cookie)
        resp @(http/get url {:headers {"Cookie" (str "session=" cookie)}})]
    (println (str "Url is " url))
    (if (not= 200 (:status resp))
      (throw (Exception. "Non-200 error raised"))
      (:body resp))))

(defn write-input [n]
  (let [filename (str "inputs/day" n ".txt")]
    (println (str "Making: " filename))
    (spit filename (get-input-for-day n))
    filename))

(defn make-test-file [n]
  (let [template "templates/test_dayx.ml"
        rendered (render-file template {:day n})
        filename (str "test/test_day" n ".ml")]
    (println (str "Making: " filename))
    (if (files/file-exists? filename)
      (throw (Exception. (str "file " filename " already exists")))
      (spit filename rendered)
      filename)))

(defn make-lib-file [n]
  (let [template "templates/dayx.ml"
        rendered (render-file template {:day n})
        filename (str "lib/day" n ".ml")]
    (println (str "Making: " filename))
    (if (files/file-exists? filename)
      (throw (Exception. (str "file " filename " already exists")))
      (spit filename rendered)
      filename)))

(defn git-add [filename]
  (println (str "Git adding: " filename))
  (sh "git" "add" filename))


(defn make-day [n]
  (for [f [write-input make-test-file make-lib-file]]
    (let [filename (f n)]
      (git-add filename)))

(defn -main [& args]
  (let [num (first args)]
    (if (some? num)
      (make-day num)
      (println "Usage: bb make_day.clj <num>"))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
