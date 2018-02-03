(ns leiningen.modules
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [lein-modules
             [inheritance :refer [inherit]]
             [common :refer [parent with-profiles read-project]]
             [compression :refer [compressed-profiles]]]
            [cuddlefish.core :refer [changed-files]]
            [leiningen.core
             [project :as prj]
             [main :as main]
             [eval :as eval]
             [utils :as utils]]))

(defn child?
  "Return true if child is an immediate descendant of project"
  [project child]
  (= (:root project) (:root (parent child))))

(defn file-seq-sans-symlinks
  "A tree seq on java.io.Files that aren't symlinks"
  [dir]
  (tree-seq
   (fn [^java.io.File f]
     (and (.isDirectory f)
          (not (utils/symlink? f))))
   (fn [^java.io.File d]
     (seq (.listFiles d)))
   dir))

(defn children
  "Return the child maps for a project according to its active profiles"
  [project]
  (if-let [dirs (-> project :modules :dirs)]
    (remove nil?
            (map (comp #(try (read-project %)
                             (catch Exception e
                               (println (.getMessage e))))
                       (memfn getCanonicalPath)
                       #(io/file (:root project) % "project.clj"))
                 dirs))
    (->> (file-seq-sans-symlinks (io/file (:root project)))
         (filter #(= "project.clj" (.getName %)))
         (remove #(= (:root project) (.getParent %)))
         (keep (comp #(try (read-project %)
                           (catch Exception e
                             (println (.getMessage e))))
                     str))
         (filter #(child? project
                          (with-profiles % (compressed-profiles project)))))))

(defn id
  "Returns fully-qualified symbol identifier for project"
  [project]
  (if project
    (symbol (:group project) (:name project))))

(defn progeny
  "Recursively return the project's children in a map keyed by id"
  ([project]
   (progeny project (compressed-profiles project)))
  ([project profiles]
   (let [kids (children (with-profiles project profiles))]
     (apply merge
            (into {} (map (juxt id identity) kids))
            (->> kids
                 (remove #(= (:root project) (:root %))) ; in case "." in :dirs
                 (map #(progeny % profiles)))))))

(defn interdependence
  "Turn a progeny map (symbols to projects) into a mapping of projects
  to their dependent projects"
  [pm]
  (let [deps (fn [p] (->> (:dependencies p)
                          (map first)
                          (map pm)
                          (remove nil?)))]
    (reduce (fn [acc [_ p]] (assoc acc p (deps p))) {} pm)))

(defn topological-sort
  "A topological sort of a mapping of graph nodes to their edges"
  {:authors ["Jon Harrop"]}
  [deps]
  (loop [deps     deps
         resolved #{}
         result   []]
    (if (empty? deps)
      result
      (if-let [dep (some (fn [[k v]]
                           (when (empty? (remove resolved v))
                             k))
                         deps)]
        (recur (dissoc deps dep)
               (conj resolved dep)
               (conj result dep))
        (throw (Exception. (apply str "Cyclic dependency: "
                                  (interpose ", " (map :name (keys deps))))))))))

(def ordered-builds
  "Sort a representation of interdependent projects topologically"
  (comp topological-sort interdependence progeny))

(defn roots
  "Transforms a \"progeny\" map of symbols to project maps to a map of
  directories (Files) to a sets of project symbols. This enables us to
  localize file changes to impacted projects."
  [pm]
  nil)

(defn impacted
  "Transforms a \"roots\" map of Files to sets of project symbols and a
  \"changed\" map of root-relative file paths to git change
  information into a single set of project symbols which have had
  DIRECT source change."
  [roots changed]
  nil)

(defn invalidated-builds
  "Check status information, computing and returning a topsort of the "
  [project since to]
  (let [pm (progeny project)
        roots (roots pm)
        changed (changed {:git "git"} since to)
        impacted (impacted roots changed)

        ;; Get a map from projects to their deps
        interdependence (interdependence pm)

        ;; Invert that to a map from projects to their dependees
        dependees (reduce (fn [acc [p deps]]
                            (reduce #(update %1 %2 (fnil conj #{}) p)
                                    acc deps))
                          {} interdependence)

        ;; Compute the closure of impact over dependent projects using
        ;; the dependees map.
        impacted (loop [impacted impacted]
                   (let [impacted* (set (mapcat dependees impacted))]
                     (if-not (= impacted impacted*)
                       (recur impacted*)
                       impacted*)))]

    ;; Select and topsort the impacted targets.
    (->> impacted
         (select-keys interdependence)
         (topological-sort))))

(defn create-checkouts
  "Create checkout symlinks for interdependent projects"
  [projects]
  (doseq [[project deps] projects]
    (when-not (empty? deps)
      (let [dir (io/file (:root project) "checkouts")]
        (when-not (.exists dir)
          (.mkdir dir))
        (println "Checkouts for" (:name project))
        (binding [eval/*dir* dir]
          (doseq [dep deps]
            (eval/sh "rm" "-f" (:name dep))
            (eval/sh "ln" "-sv" (:root dep) (:name dep))))))))

(def checkout-dependencies
  "Setup checkouts/ for a project and its interdependent children"
  (comp create-checkouts interdependence progeny))

(defn cli-with-profiles
  "Set the profiles in the args unless some are already there"
  [profiles args]
  (if (some #{"with-profile" "with-profiles"} args)
    args
    (with-meta (concat
                ["with-profile" (->> profiles
                                     (map name)
                                     (interpose ",")
                                     (apply str))]
                args)
      {:profiles-added true})))

(defn dump-profiles
  [args]
  (if (-> args meta :profiles-added)
    (str "(" (second args) ")")
    ""))

(defn print-modules
  "If running in 'quiet' mode, only prints the located modules.

  Otherwise prints a more human-formatted modules list."

  [{:keys [quiet?]} modules]
  (if (empty? modules)
    (if-not quiet?
      (println "No modules found"))

    ;; There are modules
    (do (if-not quiet?
          (println " Module build order:"))
        (doseq [p modules]
          (if-not quiet?
            (println "  " (:name p))
            (println (:name p))))

        ;; For the test suite, return all children.
        (map id modules))))

(defn modules
  "Run a task for all related projects in dependency order.

  Any task (along with any arguments) will be run in this project and
  then each of this project's child modules. For example:

  $ lein modules install
  $ lein modules deps :tree
  $ lein modules do clean, test
  $ lein modules analias

  You can create 'checkout dependencies' for all interdependent modules
  by including the :checkouts flag:

  $ lein modules :checkouts

  You can limit which modules run the task with the :dirs option:

  $ lein modules :dirs core,web install

  Delimited by either comma or colon, this list of relative paths
  will override the [:modules :dirs] config in project.clj

  You can list the modules by not providing a command, or providing
  the :list flag. When listing modules, the :quiet flag limits output
  to a newline delimited list of namespaces.

  $ lein modules :quiet :list

  Accepts '-q', '--quiet' and ':quiet' to suppress non-subprocess output.
  
  You can introspect your git history to run the selected command only
  on changed modules and their transitive dependees.
  
  $ lein modules :changed origin/master HEAD test

  will figure out what modules have hand changes, and run the tests on
  any module which has changed, or which depends on a changed
  module. If the root project.clj has changed, all tests will run."
  [project & args]
  (let [[quiet? args] ((juxt some remove) #{"-q" "--quiet" ":quiet"} args)
        quiet? (or quiet? (-> project :modules :quiet))
        {:keys [quiet?] :as opts} {:quiet? (boolean quiet?)}]
    (condp = (first args)
    ":checkouts" (do
                   (checkout-dependencies project)
                   (apply modules project (remove #{":checkouts"} args)))
    ":dirs" (let [dirs (s/split (second args) #"[:,]")]
              (apply modules
                (-> project
                    (assoc-in [:modules :dirs] dirs)
                    (assoc-in [:modules :quiet] quiet?)
                  (vary-meta assoc-in [:without-profiles :modules :dirs] dirs))
                (drop 2 args)))
    ":changed" (let [[_changed since to & args'] args]
                 ;; FIXME (reid.mckenzie 2018-02-02):
                 ;;   this needs to do at least the whole ordered builds
                 ;;   dance, generate dirs sectors and recur.
                 nil)
    ":list" (print-modules opts (ordered-builds project))
    nil     (print-modules opts (ordered-builds project))
    (let [modules (ordered-builds project)
          profiles (compressed-profiles project)
          args (cli-with-profiles profiles args)
          subprocess (get-in project [:modules :subprocess]
                       (or (System/getenv "LEIN_CMD")
                         (if (= :windows (utils/get-os)) "lein.bat" "lein")))]
      (when-not quiet?
        (print-modules opts modules))
      (doseq [project modules]
        (when-not quiet?
          (print-modules opts modules))
        (doseq [project modules]
          (when-not quiet?
            (println "------------------------------------------------------------------------")
            (println " Building" (:name project) (:version project) (dump-profiles args))
            (println "------------------------------------------------------------------------"))
          (if-let [cmd (get-in project [:modules :subprocess] subprocess)]
            (binding [eval/*dir* (:root project)]
              (let [exit-code (apply eval/sh (cons cmd args))]
                (when (pos? exit-code)
                  (throw (ex-info "Subprocess failed" {:exit-code exit-code})))))
            (let [project (prj/init-project project)
                  task (main/lookup-alias (first args) project)]
              (main/apply-task task project (rest args)))))))))
