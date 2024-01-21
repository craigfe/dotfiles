#!/usr/bin/env janet
(use sh)
(import cmd)

# --- Helpers ---

(defn- bold-red [x]
  (string "\x1b[1;31m" x "\x1b[0m"))

(defn- bold-cyan [x]
  (string "\x1b[1;36m" x "\x1b[0m"))

(defn- bold-green [x]
  (string "\x1b[1;32m" x "\x1b[0m"))

(defn- grey [x]
  (string "\x1b[38;5;8m" x "\x1b[0m"))

(defn- string/quote [x]
  (string "\xe2\x80\x9c" x "\xe2\x80\x9d"))

(defn- error-and-exit [msg & args]
  (eprintf (string (bold-red "error:") " " msg) ;args)
  (os/exit 1))

(defn spinner [s]
  (def spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])
  (var i 0)
  (var stopped false)
  (while (not stopped)
    (def frame (spinner-frames (% i (length spinner-frames))))
    (prinf "\r%s  %s " (bold-green frame) s)
    (flush)
    (try
      (ev/sleep 0.1)
      ([error try-catch-fiber]
       (match error
        :success (do
           (set stopped true)
           (printf "\r⌛ %s  %s" s (bold-green "✔ done")))
         _ (propagate error try-catch-fiber))))
    (++ i)))

(defn time-ago [a b]
  (def diff (- a b))
  (def seconds (math/floor diff))
  (def minutes (math/floor (/ seconds 60)))
  (def hours (math/floor (/ minutes 60)))
  (def days (math/floor (/ hours 24)))
  (def weeks (math/floor (/ days 7)))
  (def months (math/floor (/ days 30)))
  (def years (math/floor (/ days 365)))
  (if (> years 0)
    (string (math/floor years) "y")
    (if (> months 0)
      (string (math/floor months) "m")
      (if (> weeks 0)
        (string (math/floor weeks) "w")
        (if (> days 0)
          (string (math/floor days) "d")
          (if (> hours 0)
            (string (math/floor hours) "h")
            (if (> minutes 0)
              (string (math/floor minutes) "m")
              (string (math/floor seconds) "s"))))))))

# --- Commands ---

(defn- start 
  "Run a `monzo/wearedev` service locally" [service]
  (if (= service nil)
    (do 
      (eprintf "%s: service empty" (bold-red "error"))
      (os/exit 1)))
  (def directory "/Users/craigferguson/src/github.com/monzo/wearedev")
  (def subcommand (string/format "go run ./%s/" service))
  (printf "Running `go run %s`" service)
  ($ tmux new-session -c ,directory -s ,service -d ,subcommand)
  ($ tmux choose-tree -t ,service))

(defn fetch-if-necessary []
  # Fetch again if the most-recent fetch was more than 5 minutes ago
  (def last-fetch ((os/stat "/Users/craigferguson/src/github.com/monzo/wearedev/.git/FETCH_HEAD") :modified))
  (def now (os/time))
  (def stale-time-seconds (* 5 60)) # 5 minutes
  (def last-fetch-string (grey (string/format "(last fetch: %s ago)" (time-ago now last-fetch))))
  (if (> (- now last-fetch) stale-time-seconds)
    (do 
      (printf "📥 fetching latest changes from origin  %s" last-fetch-string)
      ($ git fetch --quiet))
    (printf "📦 using cached changes from origin  %s" last-fetch-string)))

(defn- feature [name]
  (def branch-name (string/format "craigfe@%s" name))
  # Check that this feature branch has not already been completed
  (def void (buffer/new 0))
  (if ($? git rev-parse --verify ,branch-name > [stdout void] > [stderr void])
    (error-and-exit "branch already exists: %s" (string/quote branch-name)))
  (fetch-if-necessary)
  ($ git checkout -b ,branch-name --quiet)
  (print (bold-cyan "switched to feature branch: ") (bold-green branch-name)))

(defn- pr [name]
  (print "TODO"))

(defn- refresh-asap
  "Takes a Chrome window ID containing an OpenSearch Dashboard, waits for it to load, then
   clicks the refresh button as soon as possible. This is a work-around for the fact that
   OpenSearch doesn't actually run the linked query on page load."
  [window-id]
  (def function "(function() {
    var button = document.querySelector(\"button[data-test-subj='querySubmitButton'\");
    if (!button) {
      return \"not found\";
    }
    button.click();
    return \"done\"
  })();")
  (while true
    (def res ($< chrome-cli execute ,function -t ,window-id))
    (match (string/trim res)
      "done" (break)
      "not found" (ev/sleep 0.1)
      x (error (string/format "unexpected response: %s" x)))))

(defn logs [name]
  (def name (string/replace "s." "service." name))
  (def url (string/format "https://slog.tools.s101.nonprod-ffs.io/_dashboards/app/discover#/discover?_a=(interval:auto,query:(language:kuery,query:'service:%%22%s%%22'))&_g=()" name))
  (printf "📜 opening logs for %s in S101" (bold-green name))
  (def peg '(* "Id: " (<- :d+) "\n" (any 1)))
  (def chrome-cli-output ($< chrome-cli open ,url))
  (def window-id (peg/match peg chrome-cli-output))
  (def spinner (ev/call spinner "waiting to trigger query on the page"))
  (refresh-asap (window-id 0))
  (ev/cancel spinner :success))

(cmd/defgroup command
  start (cmd/fn "run a service locally" [args (escape :string)] (start ;args))
  feat  (cmd/fn "create a feature branch" [args (escape ["NAME" :string])] (feature ;args))
  pr    (cmd/fn "create a pull request" [args (escape ["NAME" :string])] (pr ;args))
  logs  (cmd/fn "open the logs for a service" [args (escape ["NAME" :string])] (logs ;args))
)

(cmd/run command (cmd/args))

