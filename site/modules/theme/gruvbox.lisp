(in-package #:autolith)

;;;; Xenolith theme: xenolith-gruvbox.
;;;;
;;;; A gruvbox-dark presentation for the composed frame. Every
;;;; semantic style resolves through *terminal-style-table*; the theme also
;;;; imposes 24-bit terminal defaults while the fullscreen viewport is active,
;;;; so blank rows carry the gruvbox canvas instead of the terminal's own
;;;; background.

(defparameter *xenolith-gruvbox-palette*
  '((:bg0           . "#282828")
    (:bg0-soft      . "#32302f")
    (:bg1           . "#3c3836")
    (:bg2           . "#504945")
    (:bg3           . "#665c54")
    (:bg4           . "#7c6f64")
    (:bg-add        . "#2f3a2a")
    (:bg-remove     . "#3d2b2a")
    (:fg0           . "#fbf1c7")
    (:fg1           . "#ebdbb2")
    (:fg2           . "#d5c4a1")
    (:fg3           . "#bdae93")
    (:gray          . "#928374")
    (:red           . "#cc241d")
    (:red-bright    . "#fb4934")
    (:green         . "#98971a")
    (:green-bright  . "#b8bb26")
    (:yellow        . "#d79921")
    (:yellow-bright . "#fabd2f")
    (:blue          . "#458588")
    (:blue-bright   . "#83a598")
    (:purple        . "#b16286")
    (:purple-bright . "#d3869b")
    (:aqua          . "#689d6a")
    (:aqua-bright   . "#8ec07c")
    (:orange        . "#d65d0e")
    (:orange-bright . "#fe8019"))
  "The gruvbox dark palette as 24-bit sRGB values.")

(defun xenolith-gruvbox-color (name)
  "Return palette color NAME as an Autolith color."
  (let ((hex (cdr (assoc name *xenolith-gruvbox-palette*))))
    (unless hex
      (error "Unknown xenolith-gruvbox palette color ~S." name))
    (hex-color hex)))

(defparameter *xenolith-gruvbox-styles*
  '((:plain)
    (:brand             :blue-bright   :bold t)
    (:user              :orange-bright :bold t)
    (:tool              :fg0)
    (:success           :green-bright)
    (:failure           :red-bright    :bold t)
    (:notice            :orange-bright)
    (:annotation        :fg2)
    (:dim               :bg4)
    (:hint              :gray          :italic t)
    (:selected                         nil :background :bg2)
    (:surface             nil :background :bg1)
    (:surface-code        nil :background :bg0-soft)
    (:surface-diff-add    nil :background :bg-add    :foreground :green)
    (:surface-diff-remove nil :background :bg-remove :foreground :red)
    (:strong            :fg0           :bold t)
    (:emphasis                         nil :italic t)
    (:code              :yellow-bright)
    (:code-copy         :gray          :underline t)
    (:lisp-prompt       :red-bright    :bold t)
    (:plan-active       :blue-bright   :bold t)
    (:timestamp-time    :gray)
    (:mermaid-box       :gray)
    (:mermaid-edge      :yellow)
    (:mermaid-arrow     :purple-bright :bold t)
    (:mermaid-label     :green-bright)
    (:agent-spinner     :green-bright  :bold t)
    (:agent-name        :aqua-bright   :bold t)
    (:agent-role        :purple-bright)
    (:agent-tool        :yellow-bright)
    (:child-name        :purple-bright :bold t)
    (:command-spinner   :green-bright  :bold t)
    (:command-id        :aqua-bright   :bold t)
    (:command-tool      :yellow-bright)
    (:legend-plain      :fg1)
    (:legend-dim        :gray)
    (:legend-accent     :blue-bright   :bold t)
    (:status-plain      :fg1)
    (:status-dim        :gray)
    (:status-accent     :blue-bright   :bold t)
    (:status-model      :aqua-bright   :bold t)
    (:status-effort     :red-bright    :bold t)
    (:status-branch     :green-bright  :bold t)
    (:compaction-label  :blue-bright   :bold t)
    (:compaction-track  :gray)
    (:compaction-head   :blue-bright   :bold t)
    (:syntax-comment    :gray)
    (:syntax-keyword    :red-bright)
    (:syntax-string     :yellow-bright)
    (:syntax-escape     :orange-bright)
    (:syntax-number     :purple-bright)
    (:syntax-type       :aqua-bright)
    (:syntax-function   :green-bright)
    (:syntax-property   :blue-bright)
    (:syntax-heading    :blue-bright   :bold t)
    (:heading-emphasis  :blue-bright   :bold t :italic t)
    (:syntax-link       :aqua-bright   :underline t))
  "Semantic style overrides as (NAME COLOR &rest MAKE-STYLE ARGUMENTS).

A nil COLOR keeps the terminal default for that style.")

(defparameter *xenolith-gruvbox-brand-gradient*
  '(:fg0 :yellow-bright :yellow :orange-bright :orange :red-bright)
  "Startup mark rows, warm like the gruvbox accent ramp.")

(defparameter *xenolith-gruvbox-band-styles*
  '(:status-plain :status-dim :status-accent :status-model :status-effort
    :status-branch :compaction-label :compaction-track :compaction-head)
  "Styles painted on the status band's background.")

(defparameter *xenolith-gruvbox-band-background* :bg2
  "Palette color behind the status band.")

(defun xenolith--gruvbox-arguments (arguments)
  "Return ARGUMENTS with palette keywords resolved in color positions.

A surface carries :background and sometimes :foreground, each naming a palette
keyword; every other argument passes through unchanged."
  (loop for tail on arguments by #'cddr
        for key = (first tail)
        for value = (second tail)
        append (list key
                     (if (and (member key '(:background :foreground))
                              (keywordp value))
                         (xenolith-gruvbox-color value)
                         value))))

(defun xenolith-gruvbox-style (name)
  "Return NAME's gruvbox style, plain when the table names no override."
  (let* ((entry (or (assoc name *xenolith-gruvbox-styles*) (list name)))
         (color (second entry))
         (arguments (cddr entry)))
    (apply #'make-style
           (append (when color
                     (list :foreground (xenolith-gruvbox-color color)))
                   (when (member name *xenolith-gruvbox-band-styles*)
                     (list :background
                           (xenolith-gruvbox-color
                            *xenolith-gruvbox-band-background*)))
                   (xenolith--gruvbox-arguments arguments)))))
(defun xenolith-gruvbox-style-table ()
  "Return the complete xenolith-gruvbox style table."
  (let ((reserved (append *terminal-brand-gradient-names*
                          *terminal-recovery-gradient-names*
                          *terminal-lambda-logo-names*)))
    (append
     (loop for name in *terminal-style-names*
           unless (member name reserved)
             collect (cons name (xenolith-gruvbox-style name)))
     (loop for name in *terminal-brand-gradient-names*
           for color in *xenolith-gruvbox-brand-gradient*
           collect (cons name (make-style
                               :foreground (xenolith-gruvbox-color color)
                               :bold t)))
     (terminal--recovery-gradient-styles)
     (terminal--lambda-logo-styles))))

(defvar *xenolith-original-terminal-ui--boot-art* nil
  "The boot mark builder underneath this module's wrapper, or NIL.")

(defvar *xenolith-gruvbox-boot-wrapper* nil
  "The boot mark wrapper this module installed, for reload detection.")

(defvar *xenolith-gruvbox-boot-failure* nil
  "The most recent condition the boot mark wrapper caught, or NIL.")

(defvar *xenolith-gruvbox-boot-logged-p* nil
  "Whether the boot mark fallback has already logged its condition.")

(defparameter *xenolith-boot-rows*
  '("███   █ ░▒░▀▀▀ ░▒░▀▀▀▄ ░▒░▀▀▀▄ ███    ▀▀▀ ▀▀░▒░▀▀▀ ███   █"
    "░██   █ ▒░█    ▒░█   █ ▒░█   █ ░██    ░██   ▒░█    ░██   █"
    "▄░█▀▀▀▄ ░██▀   ░██   █ ░██   █ ▒░█    ▒░█   ░██    ▒░█▀▀▀█"
    "░▒░   █ ███▄▄▄ ███   █ ▀██▄▄▄▀ ░▒░▄▄▄ ░▒░   ███    ░▒░   █")
  "The Xenolith boot mark: XENOLITH set in the Cardinals block face.

Every row is 58 cells, so the mark centers inside the default boot panel's
60-cell field with room to spare and fits the height the panel reserves for it.")

(defun xenolith--boot-art ()
  "Return the installed theme's boot mark, panel title, and tagline.

The distribution brings its own mark for :xenolith-gruvbox. Every other theme
delegates to the tracked builder, whose ECASE covers :autolith and :almighty; a
name outside that set would abort startup before the panel painted, so a
condition falls back to Autolith's mark instead."
  (if (eq (terminal-theme-name *terminal-theme*) :xenolith-gruvbox)
      (values *xenolith-boot-rows*
              "X E N O L I T H  /  COMPOSED TERMINAL"
              "A SITE TREE THAT AUTOLITH LOADS")
      (handler-case
          (funcall *xenolith-original-terminal-ui--boot-art*)
        (serious-condition (condition)
          (setf *xenolith-gruvbox-boot-failure* condition)
          (unless *xenolith-gruvbox-boot-logged-p*
            (setf *xenolith-gruvbox-boot-logged-p* t)
            (xenolith-log "theme: boot art FAILED ~A" condition))
          (values *terminal-ui-boot-mascot-rows*
                  "A U T O L I T H  /  LISP MACHINE"
                  "READ . EVAL . PRINT . LOOP")))))

(when (fboundp 'terminal-ui--boot-art)
  (xenolith--chain 'terminal-ui--boot-art
                   '*xenolith-original-terminal-ui--boot-art*
                   '*xenolith-gruvbox-boot-wrapper*
                   #'xenolith--boot-art))

(defun xenolith-gruvbox-install (&key (install-p t))
  "Register xenolith-gruvbox and install it when INSTALL-P.

Registration is idempotent, so reloading the site layer replaces the previous
definition instead of stacking a duplicate theme."
  (let ((theme (make-terminal-theme
                :name        :xenolith-gruvbox
                :style-table (xenolith-gruvbox-style-table)
                :foreground  (xenolith-gruvbox-color :fg1)
                :background  (xenolith-gruvbox-color :bg0))))
    (setf *terminal-themes*
          (cons theme
                (remove :xenolith-gruvbox *terminal-themes*
                        :key #'terminal-theme-name)))
    (when install-p
      (terminal-theme-install :xenolith-gruvbox))
    theme))

(xenolith-gruvbox-install
 :install-p
 (and *xenolith-install-themes-p*
      (let ((configured (and *user-init-configuration*
                             (config :terminal-theme *user-init-configuration*))))
        (or (null configured) (eq configured :autolith)))))
