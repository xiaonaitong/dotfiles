;;when clojure version >=  1.3.0 
(when (some (complement neg?)
            (map - (map *clojure-version* [:major :minor :incremental])
                 [1, 3, 0]))
  (do
    (use 'clojure.repl)
    (use 'clojure.pprint)
    (use 'clojure.java.javadoc)
    (require 'clojure.reflect)
    (require '[clojure.string :as s])
    (defn show
      ([o] (show o nil))
      ([o re & {full :full prev :prev basic :basic}]
         (let [match? (if re #(re-find re (str %))
                          identity)
               prev (or prev [:public :private :protected :static])
               basic (or basic false)
               symbol-simple-name (fn [sym]
                                    (if sym
                                      (-> sym name (.split "\\.") last)
                                      ""))
               print-name (if full #(if % (name %) "") symbol-simple-name )
               method-decl (fn [{:keys [return-type parameter-types flags]
                                mname :name}] 
                             (let [flags (set flags)]
                              (when (seq (filter flags prev))
                                (format "%15s %10s %s(%s)"
                                        (apply str (interpose " " (map name (filter flags prev))))
                                        (print-name return-type)
                                        (print-name mname)
                                        (apply str (interpose ","
                                                              (map print-name parameter-types)))))))]
           (let [clazz (if (class? o) o (type o))
                 clazzes (if basic [clazz]
                             (conj (supers clazz) clazz))
                 methods (mapcat  (comp :members (ns-resolve 'clojure.reflect 'reflect)) clazzes)
                 methods (filter (comp match? :name) methods)]
                        (dorun
                         (map println
                              (->> methods (map method-decl) set)))))))
    (defn add-pom-deps [deps]
      (require 'cemerick.pomegranate)
      ((ns-resolve 'cemerick.pomegranate  'add-dependencies) :coordinates deps
       :repositories (merge @(ns-resolve 'cemerick.pomegranate.aether 'maven-central)
                            {"clojars" "http://clojars.org/repo"})))))
