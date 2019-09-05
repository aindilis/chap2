(defvar chap2-agent-name "KBFS-Agent1")

(global-set-key "\C-c\C-k\C-vCH" 'chap2-quick-start)

(global-set-key "\C-cche" 'chap2-edit-chap2-file)
(global-set-key "\C-cchgl" 'chap2-action-get-line)
(global-set-key "\C-cchws" 'chap2-set-windows)
(global-set-key "\C-cchs" 'chap2-quick-start)
(global-set-key "\C-cchr" 'chap2-restart)
(global-set-key "\C-cchk" 'chap2-kill)
(global-set-key "\C-cchc" 'chap2-clear-context)

(defvar chap2-default-context "Org::FRDCSA::Chap2")

(defun chap2-issue-command (query)
 ""
 (interactive)
 (uea-query-agent-raw nil chap2-agent-name
  (freekbs2-util-data-dumper
   (list
    (cons "_DoNotLog" 1)
    (cons "Eval" query)))))

(defun chap2-action-get-line ()
 ""
 (interactive)
 (see (chap2-issue-command
  (list "_prolog_list"
   (list "_prolog_list" 'var-Result)
   (list "emacsCommand"
    (list "_prolog_list" "kmax-get-line")
    'var-Result)))))

(defun chap2-quick-start ()
 ""
 (interactive)
 
 (chap2)
 (chap2-fix-windows)
 (chap2-select-windows))

(defun chap2 (&optional load-command)
 ""
 (interactive)
 (if (chap2-running-p)
  (error "ERROR: Chap2 Already running.")
  (progn
   (run-in-shell "cd /var/lib/myfrdcsa/codebases/minor/chap2/scripts" "*Chap2*")
   (sit-for 3.0)
   (ushell)
   (sit-for 1.0)
   (pop-to-buffer "*Chap2*")
   (insert (or load-command "./chap2-start -u"))
   (comint-send-input)
   (sit-for 3.0)
   (run-in-shell "cd /var/lib/myfrdcsa/codebases/minor/chap2/scripts && ./chap2-start-repl" "*Chap2-REPL*" nil 'shell-mode)
   (sit-for 1.0))))

(defun chap2-set-windows ()
 ""
 (interactive)
 (chap2-fix-windows)
 (chap2-select-windows))

(defun chap2-fix-windows ()
 ""
 (interactive)
 (delete-other-windows)
 (split-window-vertically)
 (split-window-horizontally)
 (other-window 2)
 (split-window-horizontally)
 (other-window -2))

(defun chap2-select-windows ()
 ""
 (interactive)
 (switch-to-buffer "*Chap2*")
 (other-window 1)
 ;; (switch-to-buffer "*ushell*")
 (ffap "/var/lib/myfrdcsa/codebases/minor/nlu/data-git/formalized-texts/texts/chess-and-checkers--lasker/formalizations/prolog/lasker.txt.nlu.pl")
 ;; (ffap "/var/lib/myfrdcsa/codebases/minor/chap2/doc/lasker.txt")
 (other-window 1)
 (switch-to-buffer "*Chap2-REPL*")
 (other-window 1)
 (ffap "/var/lib/myfrdcsa/codebases/minor/chap2/board.pl"))

(defun chap2-restart ()
 ""
 (interactive)
 (if (yes-or-no-p "Restart Chap2? ")
  (progn
   (chap2-kill)
   (chap2-quick-start))))

(defun chap2-kill ()
 ""
 (interactive)
 (flp-kill-processes)
 (shell-command "killall -9 \"chap2-start\"")
 (shell-command "killall -9 \"chap2-start-repl\"")
 (shell-command "killall-grep Chap2-Agent1")
 (kmax-kill-buffer-no-ask (get-buffer "*Chap2*"))
 (kmax-kill-buffer-no-ask (get-buffer "*Chap2-REPL*"))
 ;; (kmax-kill-buffer-no-ask (get-buffer "*ushell*"))
 (chap2-running-p))

(defun chap2-running-p ()
 (interactive)
 (setq chap2-running-tmp t)
 (let* ((matches nil)
	(processes (split-string (shell-command-to-string "ps auxwww") "\n"))
	(failed nil))
  (mapcar 
   (lambda (process)
    (if (not (kmax-util-non-empty-list-p (kmax-grep-v-list-regexp (kmax-grep-list-regexp processes process) "grep")))
     (progn
      (see process 0.0)
      (setq chap2-running-tmp nil)
      (push process failed))))
   chap2-process-patterns)
  (setq chap2-running chap2-running-tmp)
  (if (kmax-util-non-empty-list-p failed)
   (see failed 0.1))
  chap2-running))

(defun chap2-clear-context (&optional context-arg)
 (interactive)
 (let* ((context (or context-arg chap2-default-context)))
  (if (yes-or-no-p (concat "Clear Context <" context ">?: "))
   (freekbs2-clear-context context))))

(defvar chap2-process-patterns
 (list
  "chap2-start"
  "chap2-start-repl"
  "/var/lib/myfrdcsa/codebases/internal/unilang/unilang-client"
  "/var/lib/myfrdcsa/codebases/internal/freekbs2/kbs2-server"
  "/var/lib/myfrdcsa/codebases/internal/freekbs2/data/theorem-provers/vampire/Vampire1/Bin/server.pl"
  ))

(defun chap2-eval-function-and-map-to-integer (expression)
 ""
 (interactive)
 (chap2-serpro-map-object-to-integer
  (funcall (car expression) (cdr expression))))

(defun chap2-serpro-map-object-to-integer (object)
 ""
 (interactive)
 (see object)
 (see (formalog-query (list 'var-integer) (list "prolog2TermAlgebra" object 'var-integer))))

(defun chap2-serpro-map-integer-to-object (integer)
 ""
 (interactive)
 (see integer)
 (see (formalog-query (list 'var-integer) (list "termAlgebra2prolog" object 'var-integer))))

(defun chap2-edit-chap2-file ()
 ""
 (interactive)
 (ffap "/var/lib/myfrdcsa/codebases/minor/chap2/chap2.el"))

;; emacsCommand(['kmax-get-line'],Result). 
;; (see (freekbs2-importexport-convert (list (list 'var-Result) (list "emacsCommand" (list "kmax-get-line") 'var-Result)) "Interlingua" "Perl String"))

;; "Eval" => {
;;           "_prolog_list" => {
;;                             "_prolog_list" => [
;;                                               \*{'::?Result'}
;;                                             ],
;;                             "emacsCommand" => [
;;                                               [
;;                                                 "_prolog_list",
;;                                                 "kmax-get-line"
;;                                               ],
;;                                               \*{'::?Result'}
;;                                             ]
;;                           }
;;         },

;; "Eval" => [
;;           [
;;             "_prolog_list",
;;             [
;;               "_prolog_list",
;;               \*{'::?Result'}
;;             ],
;;             [
;;               "emacsCommand",
;;               [
;;                 "_prolog_list",
;; 	        "kmax-get-line",
;;               ],
;;               \*{'::?Result'}
;;             ]
;;           ]
;;         ],


;; <message>
;;   <id>1</id>
;;   <sender>Chap2-Agent1</sender>
;;   <receiver>Emacs-Client</receiver>
;;   <date>Sat Apr  1 10:16:28 CDT 2017</date>
;;   <contents>eval (run-in-shell \"ls\")</contents>
;;   <data>$VAR1 = {
;;           '_DoNotLog' => 1,
;;           '_TransactionSequence' => 0,
;;           '_TransactionID' => '0.667300679865178'
;;         };
;;   </data>
;; </message>

;; (see (eval (read "(run-in-shell \"ls\")")))
;; (see (cons "Result" nil ))

;; (see (freekbs2-util-data-dumper
;;      (list
;;       (cons "_DoNotLog" 1)
;;       (cons "Result" nil)
;;       )
;;       ))

;; ;; (see '(("_DoNotLog" . 1) ("Result")))
;; ;; (see '(("Result"))

;; (freekbs2-util-convert-from-emacs-to-perl-data-structures '(("_DoNotLog" . 1) ("Result")))
;; (mapcar 'freekbs2-util-convert-from-emacs-to-perl-data-structures '(("_DoNotLog" . 1) ("Result")))

;; (mapcar 'freekbs2-util-convert-from-emacs-to-perl-data-structures '(("_DoNotLog" . 1) ("Result")))

;; (see '(("_DoNotLog" . 1) ("Result")))
;; (see '(("Result")))
;; (see '(("_DoNotLog" . 1)))

;; (join ", " (mapcar 'freekbs2-util-convert-from-emacs-to-perl-data-structures '("Result")))


;; (chap2-eval-function-and-map-to-integer (list 'buffer-name))




;;;;;;;;;;;;;;;; FIX Academician to use Chap2
;; see /var/lib/myfrdcsa/codebases/minor/academician/academician-chap2.el

;; (chap2-retrieve-file-id "/var/lib/myfrdcsa/codebases/internal/digilib/data-git/game/16/c/Knowledge Representation and Reasoning.pdf")

(defun chap2-retrieve-file-id (file)
 (let* ((chased-original-file (kmax-chase file))
	(results
	 (formalog-query
	  (list 'var-FileIDs)
	  (list "retrieveFileIDs" chased-original-file 'var-FileIDs)
	  nil "Chap2-Agent1")))
  (see (car (cdadar results)))))

;; (defun academician-get-title-of-publication (&optional overwrite)
;;  ""
;;  (interactive "P")
;;  (let* ((current-cache-dir (doc-view--current-cache-dir))
;; 	(current-document-hash (gethash current-cache-dir academician-parscit-hash))
;; 	(title0 (gethash current-cache-dir academician-title-override-hash)))
;;   (if (non-nil title0)
;;    title0
;;    (progn
;;     (academician-process-with-parscit overwrite)
;;     (let* ((title1
;; 	    (progn
;; 	     ;; (see current-document-hash)
;; 	     (cdr (assoc "content" 
;; 		   (cdr (assoc "title" 
;; 			 (cdr (assoc "variant" 
;; 			       (cdr (assoc "ParsHed" 
;; 				     (cdr (assoc "algorithm" current-document-hash))))))))))))
;; 	   (title2
;; 	    (cdr (assoc "content" 
;; 		  (cdr (assoc "title" 
;; 			(cdr (assoc "variant" 
;; 			      (cdr (assoc "SectLabel" 
;; 				    (cdr (assoc "algorithm" current-document-hash)))))))))))
;; 	   (title 
;; 	    (chomp (or title1 title2))))
;;      (if (not (equal title "nil"))
;;       title
;;       (academician-override-title)))))))

;; (defun academician-process-with-parscit (&optional overwrite)
;;  "Take the document in the current buffer, process the text of it
;;  and return the citations, allowing the user to add the citations
;;  to the list of papers to at-least-skim"
;;  (interactive "P")
;;  (if (derived-mode-p 'doc-view-mode)
;;   (if doc-view--current-converter-processes
;;    (message "Academician: DocView: please wait till conversion finished.")
;;    (let ((academician-current-buffer (current-buffer)))
;;     (academician-doc-view-open-text-without-switch-to-buffer)
;;     (while (not academician-converted-to-text)
;;      (sit-for 0.1))
;;     (let* ((filename (buffer-file-name))
;; 	   (current-cache-dir (doc-view--current-cache-dir))
;; 	   (txt (expand-file-name "doc.txt" current-cache-dir)))
;;      (if (equal "fail" (gethash current-cache-dir academician-parscit-hash "fail"))
;;       (progn
;;        ;; check to see if there is a cached version of the parscit data
;;        (if (file-readable-p txt)
;; 	(let* ((command
;; 		(concat 
;; 		 "/var/lib/myfrdcsa/codebases/minor/academician/scripts/process-parscit-results.pl -f "
;; 		 (shell-quote-argument filename)
;; 		 (if overwrite " -o " "")
;; 		 " -t "
;; 		 (shell-quote-argument txt)
;; 		 " | grep -vE \"^(File is |Processing with ParsCit: )\""
;; 		 ))
;; 	       (debug-1 (if academician-debug (see (list "command: " command))))
;; 	       (result (progn
;; 			(message (concat "Processing with ParsCit: " txt " ..."))
;; 			(shell-command-to-string command)
;; 			)))
;; 	 (if academician-debug (see (list "result: " result)))
;; 	 (ignore-errors
;; 	  (puthash current-cache-dir (eval (read result)) academician-parscit-hash))
;; 	 )
;; 	(message (concat "File not readable: " txt)))
;;        ;; (freekbs2-assert-formula (list "has-title") academician-default-context)
;;        )))))))
