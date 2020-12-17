(require 'dired-x)

(setq dired-omit-files
      (concat dired-omit-files
              "\\|"
              (regexp-orrify "\\.orgx$"
                             "^\.DS_Store$"
                             "^__MACOSX$"
                             "\\.pyc$"
                             "\\.pyo$"
                             "^__pycache__"
                             ".ipynb_checkpoints"
                             "\\.lprof$"
                             "\\.bak$"
                             "^ltximg$"
                             "^\\.~lock\\."
                             "^!.*pdf$"
                             "\\.~.*#")))

(add-hook 'dired-mode-hook
          #'(lambda ()
              (setq dired-guess-shell-alist-user
                    '(("\\.e?ps$" "gv" "lpr")
                      ("\\.pdf$" "mupdf" "xpdf" "lp" "acroread")
                      ("\\.mobi$" "ebook-viewer")
                      ("\\.epub$" "ebook-viewer")
                      ("\\.djvu$" "ebook-viewer")
                      ("\\.csv$" "loffice" "gnumeric")
                      ("\\.docx?$" "loffice")
                      ("\\.xlsx?$" "loffice" "gnumeric")
                      ("\\.pptx?$" "loffice")
                      ("\\.od[spt]$" "loffice")
                      ("\\.divx$" "mplayer")
                      ("\\.flv$" "mplayer")
                      ("\\.avi$" "mplayer")
                      ("\\.mpg$" "mplayer")
                      ("\\.mp4$" "mplayer")
                      ("\\.wmv$" "mplayer")
                      ("\\.mkv$" "mplayer")
                      ("\\.mov$" "mplayer")
                      ("\\.webm$" "mplayer")
                      ("Flash......$" "mplayer")
                      ("mplay......$" "mplayer")
                      ("\\.p[bgpn]m$" "geeqie" "display")
                      ("\\.gif$" "geeqie" "display")
                      ("\\.tif$" "geeqie" "display")
                      ("\\.png$" "geeqie" "display")
                      ("\\.jpe?g$" "geeqie" "display")
                      ("\\.svg$" "geeqie" "display")
                      ("\\.e?ps.g?z$" "gunzip -qc * | gv -"
                       (concat
                        "gunzip"
                        (if dired-guess-shell-gzip-quiet " -q")))
                      ("\\.e?ps.Z$" "zcat * | gv -"
                       (concat "znew"
                               (if dired-guess-shell-gzip-quiet " -q")
                               " " dired-guess-shell-znew-switches))
                      ("viewapp.asp" "xpdf"))
                    dired-listing-switches "-alh")
              (local-set-key "\C-c\C-zf" 'browse-url-of-dired-file)
              (dired-omit-mode)
              (whitespace-mode -1)))

  ;; Auto complete with ignore case
  (setq-default read-buffer-completion-ignore-case t)
  (setq-default read-file-name-completion-ignore-case t)
