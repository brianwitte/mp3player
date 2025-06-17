(ns brianwitte.mp3player
  (:gen-class)
  (:require [cljfx.api :as fx]
            [clojure.core.cache :as cache]
            [clojure.java.io :as io])
  (:import [javax.sound.sampled AudioSystem AudioInputStream Clip DataLine$Info]
           [java.io File]
           [javafx.stage DirectoryChooser]))

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

;; Application state with context
(def app-state
  (atom (fx/create-context
         {:current-clip nil
          :playlist []
          :current-index 0
          :is-playing false
          :current-song nil
          :status-text "No song playing"}
         cache/lru-cache-factory)))

(defn supported-formats []
  "Returns supported audio formats"
  #{".wav" ".au" ".aiff"})

(defn get-file-extension [filename]
  "Extract file extension from filename"
  (let [dot-index (.lastIndexOf filename ".")]
    (when (pos? dot-index)
      (.toLowerCase (.substring filename dot-index)))))

(defn audio-file? [file]
  "Check if file is a supported audio format"
  (when-let [ext (get-file-extension (.getName file))]
    (contains? (supported-formats) ext)))

(defn load-audio-file [filepath]
  "Load an audio file and return a Clip"
  (try
    (let [file (File. filepath)
          audio-stream (AudioSystem/getAudioInputStream file)
          format (.getFormat audio-stream)
          info (DataLine$Info. Clip format)
          clip (AudioSystem/getLine info)]
      (.open clip audio-stream)
      clip)
    (catch Exception e
      (println (str "Error loading audio file: " (.getMessage e)))
      nil)))

;; Subscription functions for cljfx context
(defn playlist-sub [context]
  (fx/sub-val context :playlist))

(defn current-index-sub [context]
  (fx/sub-val context :current-index))

(defn is-playing-sub [context]
  (fx/sub-val context :is-playing))

(defn current-song-sub [context]
  (fx/sub-val context :current-song))

(defn status-text-sub [context]
  (fx/sub-val context :status-text))

(defn playlist-items-sub [context]
  (let [playlist (fx/sub-ctx context playlist-sub)]
    (mapv #(.getName (File. %)) playlist)))

;; Event handlers
(defn handle-event [event]
  (let [{:keys [event/type] :as event-data} event]
    (case type
      ::load-directory
      (let [chooser (doto (DirectoryChooser.)
                      (.setTitle "Select Music Directory"))
            selected-dir (.showDialog chooser nil)]
        (when selected-dir
          (let [dir-path (.getAbsolutePath selected-dir)
                dir (File. dir-path)
                files (filter audio-file? (.listFiles dir))
                filepaths (mapv #(.getAbsolutePath %) files)]
            (swap! app-state fx/swap-context assoc
                   :playlist filepaths
                   :current-index 0
                   :status-text (if (seq filepaths)
                                  (str "Loaded " (count filepaths) " files")
                                  "No audio files found"))
            (println (str "Loaded " (count filepaths) " files into playlist")))))

      ::play-pause
      (let [context @app-state
            playlist (fx/sub-ctx context playlist-sub)
            is-playing (fx/sub-ctx context is-playing-sub)
            current-index (fx/sub-ctx context current-index-sub)]
        (cond
          (empty? playlist)
          (println "Please load a playlist first")
          
          is-playing
          (let [clip (fx/sub-val context :current-clip)]
            (when clip (.stop clip))
            (swap! app-state fx/swap-context assoc :is-playing false))
          
          :else
          (let [clip (fx/sub-val context :current-clip)]
            (if clip
              (do
                (.start clip)
                (swap! app-state fx/swap-context assoc :is-playing true))
              ;; Load and play current song
              (when (< current-index (count playlist))
                (let [filepath (nth playlist current-index)
                      new-clip (load-audio-file filepath)]
                  (when new-clip
                    (.setFramePosition new-clip 0)
                    (.start new-clip)
                    (swap! app-state fx/swap-context assoc
                           :current-clip new-clip
                           :is-playing true
                           :current-song (.getName (File. filepath))
                           :status-text (str "Playing: " (.getName (File. filepath))
                                           " (" (inc current-index) "/" (count playlist) ")")))))))))

      ::stop
      (let [context @app-state
            clip (fx/sub-val context :current-clip)]
        (when clip
          (.stop clip)
          (.close clip))
        (swap! app-state fx/swap-context assoc
               :current-clip nil
               :is-playing false
               :status-text "Stopped"))

      ::next-song
      (let [context @app-state
            playlist (fx/sub-ctx context playlist-sub)
            current-index (fx/sub-ctx context current-index-sub)]
        (when (seq playlist)
          (let [new-index (mod (inc current-index) (count playlist))]
            ;; Stop current clip
            (when-let [clip (fx/sub-val context :current-clip)]
              (.stop clip)
              (.close clip))
            ;; Load and play next song
            (let [filepath (nth playlist new-index)
                  new-clip (load-audio-file filepath)]
              (when new-clip
                (.setFramePosition new-clip 0)
                (.start new-clip)
                (swap! app-state fx/swap-context assoc
                       :current-index new-index
                       :current-clip new-clip
                       :is-playing true
                       :current-song (.getName (File. filepath))
                       :status-text (str "Playing: " (.getName (File. filepath))
                                       " (" (inc new-index) "/" (count playlist) ")")))))))

      ::prev-song
      (let [context @app-state
            playlist (fx/sub-ctx context playlist-sub)
            current-index (fx/sub-ctx context current-index-sub)]
        (when (seq playlist)
          (let [new-index (mod (dec current-index) (count playlist))]
            ;; Stop current clip
            (when-let [clip (fx/sub-val context :current-clip)]
              (.stop clip)
              (.close clip))
            ;; Load and play previous song
            (let [filepath (nth playlist new-index)
                  new-clip (load-audio-file filepath)]
              (when new-clip
                (.setFramePosition new-clip 0)
                (.start new-clip)
                (swap! app-state fx/swap-context assoc
                       :current-index new-index
                       :current-clip new-clip
                       :is-playing true
                       :current-song (.getName (File. filepath))
                       :status-text (str "Playing: " (.getName (File. filepath))
                                       " (" (inc new-index) "/" (count playlist) ")")))))))

      ::play-song-at-index
      (let [context @app-state
            playlist (fx/sub-ctx context playlist-sub)
            index (:index event-data)]
        (when (and (seq playlist) (< index (count playlist)))
          ;; Stop current clip
          (when-let [clip (fx/sub-val context :current-clip)]
            (.stop clip)
            (.close clip))
          ;; Load and play selected song
          (let [filepath (nth playlist index)
                new-clip (load-audio-file filepath)]
            (when new-clip
              (.setFramePosition new-clip 0)
              (.start new-clip)
              (swap! app-state fx/swap-context assoc
                     :current-index index
                     :current-clip new-clip
                     :is-playing true
                     :current-song (.getName (File. filepath))
                     :status-text (str "Playing: " (.getName (File. filepath))
                                     " (" (inc index) "/" (count playlist) ")"))))))

      (println "Unknown event type:" type))))

;; UI Components
(defn control-button [{:keys [text event-type disabled?]}]
  {:fx/type :button
   :text text
   :pref-width 100
   :disable (boolean disabled?)
   :on-action {:event/type event-type}})

(defn control-panel [{:keys [fx/context]}]
  (let [is-playing (fx/sub-ctx context is-playing-sub)]
    {:fx/type :h-box
     :spacing 10
     :alignment :center
     :padding 10
     :children [{:fx/type control-button
                 :text "Load Directory"
                 :event-type ::load-directory}
                {:fx/type control-button
                 :text "Previous"
                 :event-type ::prev-song}
                {:fx/type control-button
                 :text (if is-playing "Pause" "Play")
                 :event-type ::play-pause}
                {:fx/type control-button
                 :text "Stop"
                 :event-type ::stop}
                {:fx/type control-button
                 :text "Next"
                 :event-type ::next-song}]}))

(defn status-bar [{:keys [fx/context]}]
  (let [status-text (fx/sub-ctx context status-text-sub)]
    {:fx/type :label
     :text status-text
     :padding 10
     :style {:-fx-background-color "#f0f0f0"
             :-fx-font-size 12}}))

(defn playlist-item [{:keys [text index current?]}]
  {:fx/type :label
   :text (str (inc index) ". " text)
   :padding 5
   :style (merge
           {:-fx-font-size 12}
           (when current?
             {:-fx-background-color "#2ecc71"
              :-fx-text-fill "white"}))
   :on-mouse-clicked {:event/type ::play-song-at-index
                      :index index}})

(defn playlist-view [{:keys [fx/context]}]
  (let [playlist-items (fx/sub-ctx context playlist-items-sub)
        current-index (fx/sub-ctx context current-index-sub)]
    {:fx/type :scroll-pane
     :fit-to-width true
     :content {:fx/type :v-box
               :children (if (seq playlist-items)
                          (mapv (fn [index item]
                                  {:fx/type playlist-item
                                   :fx/key index
                                   :text item
                                   :index index
                                   :current? (= index current-index)})
                                (range)
                                playlist-items)
                          [{:fx/type :label
                            :text "No playlist loaded. Click 'Load Directory' to get started."
                            :padding 20
                            :style {:-fx-font-size 14
                                    :-fx-text-fill "#666666"}}])}}))

(defn main-stage [{:keys [fx/context]}]
  {:fx/type :stage
   :showing true
   :title "Clojure MP3 Player (cljfx)"
   :width 600
   :height 500
   :scene {:fx/type :scene
           :root {:fx/type :border-pane
                  :top {:fx/type control-panel}
                  :center {:fx/type status-bar}
                  :bottom {:fx/type playlist-view}}}})

(defn root-view [_]
  {:fx/type main-stage})

;; CLI Implementation
(defn show-status []
  "Show current player status"
  (let [context @app-state
        playlist (fx/sub-ctx context playlist-sub)
        current-index (fx/sub-ctx context current-index-sub)]
    (if (seq playlist)
      (do
        (println (str "Playlist: " (count playlist) " songs"))
        (println (str "Current: " (inc current-index) "/" (count playlist)))
        (println (str "Playing: " (.getName (File. (nth playlist current-index))))))
      (println "No playlist loaded"))))

(defn show-help []
  "Display help commands"
  (println "\nMP3 Player Commands:")
  (println "  play <file>     - Play a single audio file")
  (println "  load <dir>      - Load all audio files from directory")
  (println "  p               - Play/resume current song")
  (println "  s               - Stop playback")
  (println "  n               - Next song")
  (println "  b               - Previous song")
  (println "  status          - Show current status")
  (println "  list            - Show playlist")
  (println "  help            - Show this help")
  (println "  quit            - Exit player")
  (println "\nSupported formats: WAV, AU, AIFF")
  (println "Note: MP3 requires additional libraries not in Clojure standard lib"))

(defn show-playlist []
  "Show current playlist"
  (let [context @app-state
        playlist (fx/sub-ctx context playlist-sub)
        current-index (fx/sub-ctx context current-index-sub)]
    (if (seq playlist)
      (do
        (println "\nPlaylist:")
        (doseq [[i filepath] (map-indexed vector playlist)]
          (let [marker (if (= i current-index) "► " "  ")
                filename (.getName (File. filepath))]
            (println (str marker (inc i) ". " filename)))))
      (println "Playlist is empty"))))

(defn load-playlist [directory]
  "Load all supported audio files from a directory into playlist"
  (try
    (let [dir (File. directory)
          files (filter audio-file? (.listFiles dir))
          filepaths (mapv #(.getAbsolutePath %) files)]
      (swap! app-state fx/swap-context assoc
             :playlist filepaths
             :current-index 0
             :status-text (if (seq filepaths)
                           (str "Loaded " (count filepaths) " files")
                           "No audio files found"))
      (println (str "Loaded " (count filepaths) " files into playlist"))
      (doseq [[i filepath] (map-indexed vector filepaths)]
        (println (str (inc i) ". " (.getName (File. filepath))))))
    (catch Exception e
      (println (str "Error loading directory: " (.getMessage e))))))

(defn play-file [filepath]
  "Play a single audio file"
  (handle-event {:event/type ::stop})
  (when-let [clip (load-audio-file filepath)]
    (.setFramePosition clip 0)
    (.start clip)
    (swap! app-state fx/swap-context assoc
           :current-clip clip
           :is-playing true
           :current-song (.getName (File. filepath))
           :status-text (str "Playing: " (.getName (File. filepath))))
    (println (str "Playing: " filepath))))

(defn play-current []
  "Play current song in playlist"
  (handle-event {:event/type ::play-pause}))

(defn next-song []
  "Play next song in playlist"
  (handle-event {:event/type ::next-song}))

(defn prev-song []
  "Play previous song in playlist"
  (handle-event {:event/type ::prev-song}))

(defn stop-current []
  "Stop the currently playing clip"
  (handle-event {:event/type ::stop}))

(defn process-command [input]
  "Process user command"
  (let [parts (.split (.trim input) "\\s+" 2)
        command (.toLowerCase (first parts))
        arg (when (> (count parts) 1) (second parts))]
    (case command
      "play" (if arg 
               (play-file arg)
               (println "Usage: play <filepath>"))
      "load" (if arg
               (load-playlist arg)
               (println "Usage: load <directory>"))
      "p" (play-current)
      "s" (stop-current)
      "n" (next-song)
      "b" (prev-song)
      "status" (show-status)
      "list" (show-playlist)
      "help" (show-help)
      "quit" :quit
      "exit" :quit
      "q" :quit
      (do
        (println (str "Unknown command: " command))
        (println "Type 'help' for available commands")))))

(defn start-repl []
  "Start the interactive command loop"
  (println "Simple Audio Player (Clojure)")
  (println "Type 'help' for commands")
  (loop []
    (print "\naudio> ")
    (flush)
    (let [input (read-line)]
      (when input
        (let [result (process-command input)]
          (when (not= result :quit)
            (recur))))))
  (stop-current)
  (println "Goodbye!"))

;; cljfx app
(def app
  (fx/create-app app-state
    :event-handler handle-event
    :desc-fn root-view))

(defn start-gui []
  "Start the cljfx GUI application"
  (println "Starting cljfx GUI mode...")
  app)

(defn -main
  "Entry point - supports both CLI and GUI modes"
  [& args]
  (cond
    (some #{"--gui"} args)
    (start-gui)
    
    (some #{"--cli"} args)
    (start-repl)
    
    (some #{"--help"} args)
    (do
      (show-help)
      (println "\nUsage modes:")
      (println "  --gui    Start in GUI mode (cljfx)")
      (println "  --cli    Start in CLI mode") 
      (println "  <file>   Play single file and exit"))
    
    (empty? args)
    (start-repl)
    
    :else
    (let [filepath (first args)]
      (if (.exists (File. filepath))
        (if (audio-file? (File. filepath))
          (do
            (play-file filepath)
            (println "Press Enter to exit...")
            (read-line)
            (stop-current))
          (println "Unsupported file format. Supported: WAV, AU, AIFF"))
        (println (str "File not found: " filepath))))))
