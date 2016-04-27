
(require 'helm-config)
(helm-mode 1)
(helm-flx-mode 1)
(helm-fuzzier-mode 1)

(setq helm-split-window-in-side-p t)

(add-to-list 'display-buffer-alist
             '("\\`\\*helm.*\\*\\'"
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

(setq helm-swoop-split-with-multiple-windows nil
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-split-window-function 'helm-default-display-buffer)

(setq helm-echo-input-in-header-line t)

(defvar bottom-buffers nil
  "List of bottom buffers before helm session.
    Its element is a pair of `buffer-name' and `mode-line-format'.")

(defun bottom-buffers-init ()
  (setq-local mode-line-format (default-value 'mode-line-format))
  (setq bottom-buffers
        (cl-loop for w in (window-list)
                 when (window-at-side-p w 'bottom)
                 collect (with-current-buffer (window-buffer w)
                           (cons (buffer-name) mode-line-format)))))


(defun bottom-buffers-hide-mode-line ()
  (setq-default cursor-in-non-selected-windows nil)
  (mapc (lambda (elt)
          (with-current-buffer (car elt)
            (setq-local mode-line-format nil)))
        bottom-buffers))


(defun bottom-buffers-show-mode-line ()
  (setq-default cursor-in-non-selected-windows t)
  (when bottom-buffers
    (mapc (lambda (elt)
            (with-current-buffer (car elt)
              (setq-local mode-line-format (cdr elt))))
          bottom-buffers)
    (setq bottom-buffers nil)))

(defun helm-keyboard-quit-advice (orig-func &rest args)
  (bottom-buffers-show-mode-line)
  (apply orig-func args))


(add-hook 'helm-before-initialize-hook #'bottom-buffers-init)
(add-hook 'helm-after-initialize-hook #'bottom-buffers-hide-mode-line)
(add-hook 'helm-exit-minibuffer-hook #'bottom-buffers-show-mode-line)
(add-hook 'helm-cleanup-hook #'bottom-buffers-show-mode-line)
(advice-add 'helm-keyboard-quit :around #'helm-keyboard-quit-advice)

(setq helm-display-header-line nil)

(defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
(defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
(defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))

(defun helm-toggle-header-line ()
  (if (> (length helm-sources) 1)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground helm-source-header-default-foreground
                          :background helm-source-header-default-background
                          :box helm-source-header-default-box
                          :height 1.0)
    (set-face-attribute 'helm-source-header
                        nil
                        :foreground (face-attribute 'helm-selection :background)
                        :background (face-attribute 'helm-selection :background)
                        :box nil
                        :height 0.1)))


(add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

(defun dwim-helm-find-files-up-one-level-maybe ()
  (interactive)
  (if (looking-back "/" 1)
      (call-interactively 'helm-find-files-up-one-level)
    (delete-backward-char 1)))

(define-key helm-read-file-map (kbd "<backspace>") 'dwim-helm-find-files-up-one-level-maybe)
(define-key helm-read-file-map (kbd "DEL") 'dwim-helm-find-files-up-one-level-maybe)
(define-key helm-find-files-map (kbd "<backspace>") 'dwim-helm-find-files-up-one-level-maybe)
(define-key helm-find-files-map (kbd "DEL") 'dwim-helm-find-files-up-one-level-maybe)

(defun dwim-helm-find-files-navigate-forward (orig-fun &rest args)
  "Adjust how helm-execute-persistent actions behaves, depending on context"
  (if (file-directory-p (helm-get-selection))
      (apply orig-fun args)
    (helm-maybe-exit-minibuffer)))


(define-key helm-map (kbd "<return>") 'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "RET") 'helm-maybe-exit-minibuffer)
(define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "<return>") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "RET") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "RET") 'helm-execute-persistent-action)

(advice-add 'helm-execute-persistent-action :around #'dwim-helm-find-files-navigate-forward)

(require 'cl-lib)

(with-eval-after-load 'helm-files
  (advice-add 'helm-ff-filter-candidate-one-by-one
              :before-while 'no-dots-display-file-p))

(defvar no-dots-whitelist nil
  "List of helm buffers in which to show dots.")

(defun no-dots-in-white-listed-helm-buffer-p ()
  (member helm-buffer no-dots-whitelist))

(defun no-dots-display-file-p (file)
  ;; in a whitelisted buffer display the file regardless of its name
  (or (no-dots-in-white-listed-helm-buffer-p)
      ;; not in a whitelisted buffer display all files
      ;; which does not end with /. /..
      (not (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file))))

(defun my-helm-make-source (f &rest args)
  (nconc args '(:fuzzy-match t))
  (apply f args))

(advice-add 'helm-make-source :around 'my-helm-make-source)

;; garbage collections
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(require 'smex)
(require 'flx)

(add-hook 'after-init-hook (lambda () (smex-initialize)) t)

(defun helm-smex-items ()
  (smex-convert-for-ido smex-cache))


(defun helm-smex-execute-command (command-name)
  (unwind-protect
      (execute-extended-command helm-current-prefix-arg command-name)
    (smex-rank (intern command-name))))


(defvar helm-smex-source
  '((name . "smex")
    (candidates . helm-smex-items)
    (volatile)
    (match-strict identity)
    (candidate-transformer . flx-helm-candidate-transformer)
    (action . (("execute command" . helm-smex-execute-command)
               ("execute command with prefix" . helm-smex-execute-command-with-prefix)))))


(defun flx-helm-candidate-transformer (candidates)
  "hairy but works"
  (if (zerop (length helm-pattern))
      candidates
    (let* ((mp-3-patterns (helm-mm-3-get-patterns helm-pattern))
           (flx-pattern (cdar mp-3-patterns))
           (patterns (cons (cons 'identity
                                 (mapconcat
                                  #'regexp-quote
                                  (split-string flx-pattern "" t)
                                  ".*"))
                           (cdr mp-3-patterns)))
           res)

      (setq res (loop for candidate in candidates
                      for matched = (loop for (predicate . regexp) in patterns
                                          always (funcall
                                                  predicate
                                                  (string-match regexp
                                                                (helm-candidate-get-display
                                                                 candidate))))
                      if matched
                      collect (let ((score (flx-score candidate flx-pattern helm-flx-cache)))
                                (cons (copy-sequence candidate)
                                      (cons candidate
                                            score)))))

      (setq res (sort res
                      (lambda (a b)
                        (> (caddr a) (caddr b)))))
      (helm-smex-flx-highlight-and-format res)
      res)))



(defun helm-smex-flx-highlight-and-format (res)
  (cl-loop for item in res
           for score = (cddr item)
           do
           (progn
             (if (string-match-p " " helm-pattern)
                 (cl-loop with pattern = (split-string helm-pattern)
                          for p in pattern
                          do (when (string-match (substring-no-properties p)
                                                 (substring-no-properties (car item)))
                               (put-text-property
                                (match-beginning 0) (match-end 0) 'face 'helm-match
                                (car item))))
               (setcar item (flx-propertize (car item) score)))
             (setcdr item (cadr item)))))



(defun helm-smex/run (arg)
  (interactive "P")
  (if arg
      (smex-rebuild-cache))
  (let ((helm--mode-line-display-prefarg t))
    (helm :sources 'helm-smex-source
          :buffer "*helm-smex*")))



(defun helm-smex-execute-command-with-prefix (command-name)
  (unwind-protect
      (let ((helm-current-prefix-arg (list 4)))
        (execute-extended-command helm-current-prefix-arg command-name))
    (smex-rank (intern command-name))))
