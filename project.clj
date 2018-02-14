(defproject fundingcircle/lein-modules "_"
  :description "Similar to Maven multi-module projects, but less sucky"
  :url "https://github.com/FundingCircle/lein-modules"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :eval-in-leiningen true

  :plugins [[me.arrdem/lein-git-version "2.0.5"]]

  :deploy-repositories {"releases" :clojars}

  :release-tasks [["vcs" "assert-committed"]
                  ["vcs" "tag"]
                  ["deploy"]
                  ["vcs" "push"]]

  :git-version
  {:status-to-version
   (fn [{:keys [tag version branch ahead ahead? dirty?] :as git}]
     (if (and tag (not ahead?) (not dirty?))
       (do (assert (re-find #"\d+\.\d+\.\d+" tag)
                   "Tag is assumed to be a raw SemVer version")
           tag)
       (if (and tag (or ahead? dirty?))
         (let [[_ prefix patch] (re-find #"(\d+\.\d+)\.(\d+)" tag)
               patch            (Long/parseLong patch)
               patch+           (inc patch)]
           (format "%s.%d%s-SNAPSHOT" prefix patch+
                   (if (not= "master" branch)
                     (str \- branch) "")))
         "0.1.0-SNAPSHOT")))})
