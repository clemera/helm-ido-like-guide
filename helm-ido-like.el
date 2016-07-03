
(defun helm-ido-like-activate-helm-modes ()
  (require 'helm-config)
  (helm-mode 1)
  (helm-flx-mode 1)
  (helm-fuzzier-mode 1))

(defun helm-ido-like-load-ido-like-bottom-buffer ()
  ;; popup helm-buffer at the bottom
  (setq helm-split-window-in-side-p t)
  (add-to-list 'display-buffer-alist
               '("\\`\\*helm.*\\*\\'"
                 (display-buffer-in-side-window)
                 (window-height . 0.4)))
  (add-to-list 'display-buffer-alist
               '("\\`\\*helm help\\*\\'"
                 (display-buffer-pop-up-window)))

  ;; same for helm swoop
  (setq helm-swoop-split-with-multiple-windows nil
      helm-swoop-split-direction 'split-window-vertically
      helm-swoop-split-window-function 'helm-default-display-buffer)
  ;; dont display the header line
  (setq helm-display-header-line nil)
  ;; input in header line
  (setq helm-echo-input-in-header-line t)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe))

(defvar helm-ido-like-bottom-buffers nil
  "List of bottom buffers before helm session started.
Its element is a pair of `buffer-name' and `mode-line-format'.")


(defun helm-ido-like-bottom-buffers-init ()
  (setq-local mode-line-format (default-value 'mode-line-format))
  (setq helm-ido-like-bottom-buffers
        (cl-loop for w in (window-list)
                 when (window-at-side-p w 'bottom)
                 collect (with-current-buffer (window-buffer w)
                           (cons (buffer-name) mode-line-format)))))


(defun helm-ido-like-bottom-buffers-hide-mode-line ()
  (mapc (lambda (elt)
          (with-current-buffer (car elt)
            (setq-local mode-line-format nil)))
        helm-ido-like-bottom-buffers))


(defun helm-ido-like-bottom-buffers-show-mode-line ()
  (when helm-ido-like-bottom-buffers
    (mapc (lambda (elt)
            (with-current-buffer (car elt)
              (setq-local mode-line-format (cdr elt))))
          helm-ido-like-bottom-buffers)
    (setq helm-ido-like-bottom-buffers nil)))


(defun helm-ido-like-helm-keyboard-quit-advice (orig-func &rest args)
  (helm-ido-like-bottom-buffers-show-mode-line)
  (apply orig-func args))

(defun helm-ido-like-hide-modelines ()
  ;; hide The Modelines while Helm is active
  (add-hook 'helm-before-initialize-hook #'helm-ido-like-bottom-buffers-init)
  (add-hook 'helm-after-initialize-hook #'helm-ido-like-bottom-buffers-hide-mode-line)
  (add-hook 'helm-exit-minibuffer-hook #'helm-ido-like-bottom-buffers-show-mode-line)
  (add-hook 'helm-cleanup-hook #'helm-ido-like-bottom-buffers-show-mode-line)
  (advice-add 'helm-keyboard-quit :around #'helm-ido-like-helm-keyboard-quit-advice))

(defun helm-ido-like-hide-helm-modeline-1 ()
  "Hide mode line in `helm-buffer'."
  (with-helm-buffer
    (setq-local mode-line-format nil)))


(defun helm-ido-like-hide-helm-modeline ()
  (fset 'helm-display-mode-line #'ignore)
  (add-hook 'helm-after-initialize-hook 'helm-ido-like-hide-helm-modeline-1))

(defvar helm-ido-like-source-header-default-background nil)
(defvar helm-ido-like-source-header-default-foreground nil)
(defvar helm-ido-like-source-header-default-box nil)

(defun helm-ido-like-toggle-header-line ()
  ;; Only Show Source Headers If More Than One
  (if (> (length helm-sources) 1)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground helm-ido-like-source-header-default-foreground
                          :background helm-ido-like-source-header-default-background
                          :box helm-ido-like-source-header-default-box
                          :height 1.0)
    (set-face-attribute 'helm-source-header
                        nil
                        :foreground (face-attribute 'helm-selection :background)
                        :background (face-attribute 'helm-selection :background)
                        :box nil
                        :height 0.1)))

(defun helm-ido-like-header-lines-maybe ()
  (setq helm-ido-like-source-header-default-background (face-attribute 'helm-source-header :background))
  (setq helm-ido-like-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
  (setq helm-ido-like-source-header-default-box (face-attribute 'helm-source-header :box))
  (add-hook 'helm-before-initialize-hook 'helm-ido-like-toggle-header-line))

(defun helm-ido-like-find-files-up-one-level-maybe ()
  (interactive)
  (if (looking-back "/" 1)
      (call-interactively 'helm-find-files-up-one-level)
    (delete-char -1)))


(defun helm-ido-like-find-files-navigate-forward (orig-fun &rest args)
  "Adjust how helm-execute-persistent actions behaves, depending on context."
  (let ((sel (helm-get-selection)))
    (if (file-directory-p sel)
        ;; the current dir needs to work to
        ;; be able to select directories if needed
        (cond ((and (stringp sel)
                    (string-match "\\.\\'" (helm-get-selection)))
               (helm-maybe-exit-minibuffer))
              (t
               (apply orig-fun args)))
      (helm-maybe-exit-minibuffer))))


(defun helm-ido-like-load-file-nav ()
  (advice-add 'helm-execute-persistent-action :around #'helm-ido-like-find-files-navigate-forward)
    ;; <return> is not bound in helm-map by default
  (define-key helm-map (kbd "<return>") 'helm-maybe-exit-minibuffer)
  (with-eval-after-load 'helm-files
    (define-key helm-read-file-map (kbd "<backspace>") 'helm-ido-like-find-files-up-one-level-maybe)
    (define-key helm-read-file-map (kbd "DEL") 'helm-ido-like-find-files-up-one-level-maybe)
    (define-key helm-find-files-map (kbd "<backspace>") 'helm-ido-like-find-files-up-one-level-maybe)
    (define-key helm-find-files-map (kbd "DEL") 'helm-ido-like-find-files-up-one-level-maybe)

    (define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)
    (define-key helm-read-file-map (kbd "<return>") 'helm-execute-persistent-action)
    (define-key helm-find-files-map (kbd "RET") 'helm-execute-persistent-action)
    (define-key helm-read-file-map (kbd "RET") 'helm-execute-persistent-action)))

(defvar helm-ido-like-no-dots-whitelist
  '("*Helm file completions*")
  "List of helm buffers in which to show dot directories.")

 (defun helm-ido-like-no-dots-display-file-p (file)
  ;; in a whitelisted buffer display all but the relative path to parent dir
  (or (and (member helm-buffer helm-ido-like-no-dots-whitelist)
           (not (string-match "\\(?:/\\|\\`\\)\\.\\{2\\}\\'" file)))
      ;; in all other buffers display all files but the two relative ones
      (not (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file))))


(defun helm-ido-like-no-dots-auto-add (&rest args)
  "Auto add buffers which want to read directory names to the whitelist."
  (if (eq (car (last args)) 'file-directory-p)
      (add-to-list 'helm-ido-like-no-dots-whitelist
                   (format "*helm-mode-%s*"
                           (helm-symbol-name
                            (or (helm-this-command) this-command))))))


(defun helm-ido-like-no-dots ()
  (require 'cl-lib)
  (advice-add 'helm-ff-filter-candidate-one-by-one
              :before-while 'helm-ido-like-no-dots-display-file-p)
  (advice-add  'helm--generic-read-file-name :before 'helm-ido-like-no-dots-auto-add))

(defvar helm-ido-like-user-gc-setting nil)

(defun helm-ido-like-higher-gc ()
  (setq helm-ido-like-user-gc-setting gc-cons-threshold)
  (setq gc-cons-threshold most-positive-fixnum))


(defun helm-ido-like-lower-gc ()
  (setq gc-cons-threshold helm-ido-like-user-gc-setting))

(defun helm-ido-like-helm-make-source (f &rest args)
  (let ((source-type (cadr args)))
    (unless (or (memq source-type '(helm-source-async helm-source-ffiles))
                (eq (plist-get args :filtered-candidate-transformer)
                    'helm-ff-sort-candidates)
                (eq (plist-get args :persistent-action)
                    'helm-find-files-persistent-action))
      (nconc args '(:fuzzy-match t))))
  (apply f args))

(defun helm-ido-like-load-fuzzy-enhancements ()
  (add-hook 'minibuffer-setup-hook #'helm-ido-like-higher-gc)
  (add-hook 'minibuffer-exit-hook #'helm-ido-like-lower-gc)
  (advice-add 'helm-make-source :around 'helm-ido-like-helm-make-source))

(defun helm-ido-like-fuzzier-deactivate (&rest _)
  (helm-fuzzier-mode -1))


(defun helm-ido-like-fuzzier-activate (&rest _)
  (unless helm-fuzzier-mode
    (helm-fuzzier-mode 1)))


(defun helm-ido-like-fix-fuzzy-files ()
  (add-hook 'helm-find-files-before-init-hook #'helm-ido-like-fuzzier-deactivate)
  (advice-add 'helm--generic-read-file-name :before #'helm-ido-like-fuzzier-deactivate)
  (add-hook 'helm-exit-minibuffer-hook #'helm-ido-like-fuzzier-activate)
  (add-hook 'helm-cleanup-hook #'helm-ido-like-fuzzier-activate)
  (advice-add 'helm-keyboard-quit :before #'helm-ido-like-fuzzier-activate))

;;;###autoload
(defun helm-ido-like ()
  "Configure and activate `helm', `helm-fuzzier' and `helm-flx'."
  (interactive)
  (helm-ido-like-activate-helm-modes)
  (helm-ido-like-load-ido-like-bottom-buffer)
  (helm-ido-like-hide-modelines)
  (helm-ido-like-hide-helm-modeline)
  (helm-ido-like-header-lines-maybe)
  (helm-ido-like-setup-bg-color)
  (helm-ido-like-load-file-nav)
  (helm-ido-like-no-dots)
  (helm-ido-like-load-fuzzy-enhancements)
  (helm-ido-like-fix-fuzzy-files))

(provide 'helm-ido-like)
;;; helm-ido-like.el ends here
