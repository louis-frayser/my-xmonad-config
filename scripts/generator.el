;;; Just code to generate code
(setq wss
     '( "Admin" "Home" "PIM" "Practice" "Research" "Project" "Graphics" "A/V" "Scratch"))

(defun fkeys (n ls)
  (if ls
      (let* ((ws (car ls)) )
	(princ (format "    , ((modm, xK_F%d)," n))
	(princ "             ")
	(princ
	 (format "view2 \"%s\" \"%s+\")\n"
		 ws ws ))
	(loop (+ n 1) (cdr ls) ))
    t))
(defun gen-fkeys ()
  (fkeys 1 wss))

(gen-fkeys)


