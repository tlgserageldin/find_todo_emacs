;;; my-find.el --- substring search across project files -*- lexical-binding: t; -*-

(defconst my-excluded-extensions
  (mapcar #'downcase (mapcar #'string-trim (split-string "o,a,png,jpg,jpeg,gif,pdf,zip,tar,gz,7z,bin,exe" "," t))))

(defconst my-script-dir
  (file-name-directory (file-truename (or load-file-name buffer-file-name)))
  "Directory this script file lives in.")

(defconst my-project-name
  (file-name-directory (file-truename (or load-file-name buffer-file-name)))
  "The name of the project.")

(defconst my-search-root
  my-script-dir
  "Parent directory of this script (search starts here).")

(message "ROOT: %s" my-search-root)

(defun my--ext (file)
  (downcase (or (file-name-extension file) "")))

(defun my--excluded-ext-p (file excluded-exts)
  (member (my--ext file) excluded-exts))

(defun my--probably-text-file-p (file &optional bytes)
  "Heuristic: treat files containing NUL in first BYTES as binary."
  (let ((n (or bytes 4096)))
    (with-temp-buffer
      (insert-file-contents-literally file nil 0 n)
      (not (string-match-p "\0" (buffer-string))))))

(defun my--line-at-pos ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun my-search-substring (needle)
  "Search NEEDLE across files under `my-search-root`, excluding EXCLUDED-EXTS.

Results are written to a *grep* buffer with file:line:col so `next-error` / RET works."
  (let* ((excluded my-excluded-extensions)
         (files (directory-files-recursively my-search-root ".*" nil nil nil))
         (out (get-buffer-create "*my-search*")))
    (with-current-buffer out
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Search root: %s\nNeedle: %s\nExcluded: %S\n\n"
                      my-search-root needle excluded)))
    (if (not (null files))
        (dolist (f files)
          (when (and (file-regular-p f)
                     (not (my--excluded-ext-p f excluded))
                     (my--probably-text-file-p f))
            (with-temp-buffer
              (insert-file-contents f)
              (goto-char (point-min))
              (while (re-search-forward needle nil t)
                (let* ((line (line-number-at-pos))
                       (col  (1+ (- (point) (line-beginning-position))))
                       (txt  (my--line-at-pos)))
                  (with-current-buffer out
                    (insert (format "%s:%d:%d:%s\n" f line col txt))))))))
      (with-current-buffer out
        (insert "No Text Files Found.")))
    (with-current-buffer out
      (grep-mode))
    (pop-to-buffer out)))

(defun my-find-todo ()
  "Use my-search-substring to find all TODO comments"
  (my-search-substring "^[/*;\-#]+\\s-*TODO([^)]*)"))

(my-find-todo)
