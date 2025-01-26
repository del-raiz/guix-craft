;;; Copyright © 2021-2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2022-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2023-2024 aurtzy <aurtzy@gmail.com>
;;; Copyright © 2023-2024 Erik P Almaraz <erikalmaraz@fastmail.com>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Additional base services for Guix.
;;;
;;; The impure-symlinks service type is based off of symlink-manager, except
;;; that it supports "impure symlinks" - as it is dubbed here - which are
;;; symlinks that do not lead into the store.


;;; Adapted from:
;;; https://github.com/aurtzy/guix-config/blob/master/modules/my-guix/home/services.scm

(define-module (config home services home-impure-symlinks)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)

  #:export (home-impure-symlinks-service-type))


(define (compute-impure-symlinks-file init-symlinks symlinks)
  "Return a file derivation that stores an alist of SYMLINKS (combined with
INIT-SYMLINKS)."
  (gexp->file
   "impure-symlinks"
   #~#$(append init-symlinks symlinks)))

(define (impure-symlinks-file-entry m-symlinks)
  (mlet %store-monad ((symlinks m-symlinks))
    (return `(("impure-symlinks" ,symlinks)))))

(define (update-impure-symlinks-script)
  (program-file
   "update-impure-symlinks"
   (with-imported-modules (source-module-closure
                           '((guix build utils)
                             (guix i18n)))
     #~(begin
         (use-modules (guix build utils)
                      (guix i18n)
                      (ice-9 ftw)
                      (ice-9 match))

         (define home-directory
           (getenv "HOME"))

         ;; TODO: these XDG variables aren't used for now...

         (define xdg-config-home
           (or (getenv "XDG_CONFIG_HOME")
               (string-append (getenv "HOME") "/.config")))

         (define xdg-data-home
           (or (getenv "XDG_DATA_HOME")
               (string-append (getenv "HOME") "/.local/share")))

         (define backup-directory
           (string-append home-directory "/" (number->string (current-time))
                          "-guix-home-impure-symlinks-legacy-configs-backup"))

         (define (symlink-to? source target)
           "Return #t if file at TARGET is a symlink that points to SOURCE.
Paths are not canonicalized, so it is possible for this procedure to return #f
even if both SOURCE and TARGET lead to the same file."
           (false-if-exception
            (equal? source
                    (readlink target))))

         (define (target-file file)
           (string-append home-directory
                          ;; No need to add separator if file name is empty or
                          ;; starts with separator
                          (if (or (string-null? file)
                                  (string-prefix? file-name-separator-string
                                                  file))
                              ""
                              "/")
                          file))

         (define (backup-file file)
           (define backup
             (string-append backup-directory "/" file))

           (mkdir-p backup-directory)
           (format #t (G_ "Backing up ~a...") (target-file file))
           (mkdir-p (dirname backup))
           (rename-file (target-file file) backup)
           (display (G_ " done\n")))

         (define (find-impure-symlinks-file home-generation)
           (let ((file (string-append home-generation "/impure-symlinks")))
             (if (file-exists? file) file #f)))

         (define (remove-empty-directories directory)
           (when (zero? (length
                         (scandir directory
                                  (lambda (file)
                                    (not (member file '("." "..")))))))
             (rmdir directory)
             (format #t (G_ "Removed ~a.\n") directory)
             (remove-empty-directories (dirname directory))))

         (define (cleanup-symlink target source)
           (if (symlink-to? source (target-file target))
               (begin
                 (format #t
                         (G_ "Removing ~a...")
                         (target-file target))
                 (delete-file (target-file target))
                 (display (G_ " done\n"))
                 (remove-empty-directories
                  (dirname (target-file target))))
               (format #t
                       (G_ "Skipping ~a (not an impure symlink)... done\n")
                       (target-file target))))

         (define (cleanup-symlinks home-generation)
           "Delete impure symlinks created from HOME-GENERATION."
           (define impure-symlinks-file
             (find-impure-symlinks-file home-generation))

           (when impure-symlinks-file
             (format
              #t
              (G_ "Cleaning up impure symlinks from previous home at ~a.~%")
              home-generation)
             (newline)

             (for-each
              (lambda (symlink-spec)
                (match symlink-spec
                  ((target source)
                   (cleanup-symlink target source))
                  ((target-dir source-dir files ..1)
                   (let ((target-dir
                          (string-append
                           target-dir (if (string-suffix? "/" target-dir)
                                          ""
                                          "/")))
                         (source-dir
                          (string-append
                           source-dir (if (string-suffix? "/" source-dir)
                                          ""
                                          "/"))))
                     (for-each
                      (lambda (file)
                        (let ((target (string-append target-dir file))
                              (source (string-append source-dir file)))
                          (cleanup-symlink target source)))
                      files)))
                  (else
                   (format
                    #t
                    (G_ "Skipping cleaning up ~a (INVALID SPEC GIVEN)...\n")
                    symlink-spec))))
              (call-with-input-file impure-symlinks-file read))

             (display (G_ "Cleanup finished.\n\n"))))

         (define (create-symlink target source)
           (when (false-if-exception (lstat (target-file target)))
             (backup-file target))
           (format #t (G_ "Symlinking ~a -> ~a...")
                   (target-file target) source)
           (mkdir-p (dirname (target-file target)))
           (symlink source (target-file target))
           (display (G_ " done\n")))

         (define (create-symlinks home-generation)
           "Create impure symlinks for HOME-GENERATION. Back up files that are
in the way."
           (define impure-symlinks-file
             (find-impure-symlinks-file home-generation))

           (for-each
            (lambda (symlink-spec)
              (match symlink-spec
                ((target source)
                 (create-symlink target source))
                ((target-dir source-dir files ..1)
                 (let ((target-dir
                        (string-append
                         target-dir (if (string-suffix? "/" target-dir)
                                        ""
                                        "/")))
                       (source-dir
                        (string-append
                         source-dir (if (string-suffix? "/" source-dir)
                                        ""
                                        "/"))))
                   (for-each
                    (lambda (file)
                      (let ((target (string-append target-dir file))
                            (source (string-append source-dir file)))
                        (create-symlink target source)))
                    files)))))
            (call-with-input-file impure-symlinks-file read)))

         #$%initialize-gettext

         (let* ((new-home (getenv "GUIX_NEW_HOME"))
                (old-home (getenv "GUIX_OLD_HOME")))
           (when old-home
             (cleanup-symlinks old-home))

           (create-symlinks new-home)

           (display (G_" done\nFinished updating impure symlinks.\n\n")))))))

(define (update-impure-symlinks-gexp m-symlinks)
  ;; symlinks not used here
  #~(primitive-load #$(update-impure-symlinks-script)))

(define home-impure-symlinks-service-type
  (service-type (name 'home-impure-symlinks)
                (default-value '())
                (compose concatenate)
                (extend compute-impure-symlinks-file)
                (extensions
                 (list (service-extension
                        home-service-type
                        impure-symlinks-file-entry)
                       (service-extension
                        home-activation-service-type
                        update-impure-symlinks-gexp)))
                (description "Provide an @code{update-impure-symlinks} script,
which is similar to that provided by @code{symlink-manager} but allows for
symlinks to files outside of the store, dubbed \"impure symlinks\".  This
service type is directly extended with specifications for symlinks to be
created.")))
