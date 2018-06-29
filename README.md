# Emacs configuration files for the working clojurian

After years of loyal service, I decided it was time to pension
[prelude][prelude] and get a bit more control over my Emacs
configuration. Starting from scratch was a bit daunting, but [standing on the
shoulders of giants][manuel] made it happen faster than it would have otherwise
happened.

## Rationale

Emacs configuration is like underwear: it's intimately a personal item and it's
most awkward to use some else's. Anyway, I'll explain the underlying ideas
behind my emacs configuration

### Basic tenets

- *always* use [`use-package`][use-package] to install software
- stick to [MELPA stable][melpa] and avoid unstable software as much as possible
- split configuration into multiple files and load them in a predictable order

The last bit is done by the following bit in `init.el`:

```elisp
;;;
;;; Split elisp configuration in files under ~/.emacs.d/lisp and automatically
;;; load all of them in lexicographical order
;;;
(add-to-list 'load-path skuro/personal-packages-folder)

(defun skuro/personal-packages ()
  "List all packages declared in the personal `lisp' folder."
  (mapcar 'file-name-sans-extension
          (directory-files skuro/personal-packages-folder nil ".*\.el")))

(defmacro skuro/use-personal-packages ()
  "Ensure and defer PACKAGES using `use-package'."
  (declare (indent defun))
  (let ((packages (skuro/personal-packages)))
    (macroexp-progn
     (mapcar (lambda (package)
               `(use-package ,package))
             packages))))

(skuro/use-personal-packages)
```

[prelude]: https://github.com/bbatsov/prelude
[manuel]: https://github.com/manuel-uberti/.emacs.d/
[use-package]: https://github.com/jwiegley/use-package
[melpa]: http://stable.melpa.org/#/
