(in-package #:cl-user)

(let ((*package* *package*))
  (load (merge-pathnames "initiate.lisp" *gbbopen-install-root*) :verbose nil))

(gbbopen-user)

(let ((options nil))
  ;; aka :agenda-shell-user
  (cl-user::startup-module :agenda-shell-user options :gbbopen-user))
