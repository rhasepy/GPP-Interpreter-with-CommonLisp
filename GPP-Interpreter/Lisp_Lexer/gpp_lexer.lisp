;; 			Muharerm Ozan Yesiller
;;			171044033
;;			Assignment 1

;; Letter List
(setf alphabet '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))

;; Number List
(setf numeral '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

;; Token key to find token values with assoc func
(setf tokenKey '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false" "+" "-" "/" "*" "(" ")" "**" ","))
(setf tokenVal '("KW_AND" "KW_OR" "KW_ NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" " KW_FALSE" "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_DBLMULT" "OP_COMMA"))

;; " character counter to syntax error or not
(setf quomark_count 0)


;; REPL MODE
(defun runInterpreter()

	(with-open-file (file_out "parsed_lisp.txt"		;; OPEN FILE FOR WRITE RESULT
		:direction :output
		:if-does-not-exist :create)

		(setq input_ "X")
		(loop while (not (string= input_ ""))		;; INPUT LINE
			do	
			(progn
				(format t "~%> ")
				(setq input_ (read-line))
				(tokanizeAdapter input_ file_out)))))

(defun runWithFile (fileName)

	(with-open-file (file_out "parsed_lisp.txt"		;; OPEN FILE FOR WRITE RESULT
		:direction :output
		:if-does-not-exist :create)

		(let ((in (open fileName :if-does-not-exist nil)))
			(when in
				(loop for line = (read-line in nil)
					while line do (tokanizeAdapter line file_out))
				(close in)))))

(defun tokanizeAdapter (input_ &optional (form t))	;; TOKANIZE  ADAPTER function that fragments input and call tokenize with fragmented input in lambda function

	(if (not (string= input_ ""))

		(progn
			(setq fragmentedInput (fragmentsInput input_))
			(if (string= (car fragmentedInput) ";;")
				(format form "COMMENT~%")
				(progn
					(map nil #'(lambda (x)
						(setf temp (tokenize x (pairlis tokenKey tokenVal) form)) ;; Lambda function that print result for fragmented input with token map that has token key and token value
						(if (not (equal nil temp)) (format form "~A~%" temp)))
					fragmentedInput))))
		(progn
			(exit))))

(defun tokenize (token tokenMap &optional (form t))		;; Tokenize function does tests on fragmented input and return result for lambda function that in tokanizeAdapter function

	(setf token0 (string (char token 0)))

	(if (testComment token form)
			(if (isLetter token0)
				(testIdent token tokenMap)
				(if (isNumeral token0)
					(testValue token)
					(if (isQuomarkp token0)
						(testQuomark token)
						(if (testOperator token tokenMap)
							(testOperator token tokenMap)
							(testErr token)))))))

(defun fragmentsInput (input_)					;; Fragments input

	(setq input_ (listToString (rounder input_)))		;; string parsing and convert list
	(setq input_ (removeEmptyString (splitby_Space input_))))	;; empty string remove on this list

(defun rounder (param)
	(map 'list #'(lambda (x)
					(if (isBracket (string x)) 						;; bracket priority test and parse if x is bracket
						(concatenate 'string " " (string x) " ")
						(string x)))
			(string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)
      							param)))

(defun listToString (param_list)		;; convert list to string
	(format nil "~{~A~}" param_list))

(defun isComment (param) (string= param ";"))		;; if comment

(defun isSpace (param) (string= " " param))			;; if space

(defun isLetter (param) 							;; if letter
	(if (equal nil (find param alphabet :test #'equal)) nil
		T))

(defun isNumeral (param)							;; if number
	(if (equal nil (find param numeral :test #'equal)) nil
		T))

(defun isZeroNumeral (param)						;; if zero "0"
	(string= "0" param))

(defun isSemicolon (param)							;; if semicolon
	(string= ";" param))

(defun isQuomarkp (param)							;; if quomark "  \"  "
	(if (string= "\"" param) T
		nil))

(defun isBracket (param)							;; if bracket
	(or (string= "(" param) (string= ")" param)))

(defun splitby_Space (str)							;; split by space

    (loop for i = 0 then (1+ j) as j = (position #\Space str :start i)
        collect (subseq str i j) while j))

(defun removeEmptyString (lst)						;; remove empty string on list

	(setf returnList '())

	(loop for element in lst do
			(if (not (string= element ""))
				(setf returnList (append returnList (list element)))))
	(setf lst returnList))

(defun testErr (token)								;; return error message for printing why format nil because maybe format t or format anyfile it is easy
	(format nil "SYNTAX_ERROR '~a' cannot be tokenized" token))

(defun testOperator (token tokenMap)				;; return operator message for printing why format nil because maybe format t or format anyfile it is easy

	(setf val (assoc token tokenMap :test #'string=))
	(if val (format nil "~a" (cdr val)) nil))

(defun testKeywords (token tokenMap)			;; return kw message for printing why format nil because maybe format t or format anyfile it is easy

	(setf val (assoc token tokenMap :test #'string=))
	(if val (format nil "~a" (cdr val)) nil))

(defun testQuomark (token)					;; return \" message for printing why format nil because maybe format t or format anyfile it is easy

	(setf quomark_count 0)

	(loop for ch across token
		do
			(if (isQuomarkp (string ch))
				(incf quomark_count)))

	(if (= 0 (mod quomark_count 2))
		(format nil "OP_OC ~%STRING ~%OP_CC")
		(format nil "OP_OC ~%SYNTAX_ERROR '\"' cannot be tokenized")))

(defun testIdent (token tokenMap)			;; return ID message for printing why format nil because maybe format t or format anyfile it is easy

	(loop for ch across token 				;; enhanced for loop on string with character
		do
			(if (not (or (isNumeral (string ch)) (isLetter (string ch))))
				(return-from testIdent (testErr token))))

	(setf tok (testKeywords token tokenMap))
	(if (equal nil tok) (format nil "IDENTIFIER") tok))

(defun testComment (token &optional (form t))		;; return comment message for printing why format nil because maybe format t or format anyfile it is easy

	(setq commentCount 0)
	(setq commentFlag T)
	(setq comment_ch ";")

	(loop for ch across token 					;; enhanced for loop on string with character
		do
			(if (> 2 commentCount)
				(progn
					(if (string= ";" (string ch))
						(incf commentCount)
						(decf commentCount)))))

	(if (not (= 2 commentCount)) T
		(progn
			(format form "COMMENT~%")
			nil)))

(defun testValue (token)					;; return value message for printing why format nil because maybe format t or format anyfile it is easy

	(if (and 
			(isZeroNumeral (string (char token 0)))
			(< 1 (length token)))
		(progn
			(return-from testValue (testErr token))))

	(loop for ch across token 				;; enhanced for loop on string with char
		do
			(if (not (isNumeral (string ch)))
				(return-from testValue (testErr token ch))))
	(format nil "VALUE"))

(defun main()

	(if *args* 
		(runWithFile (car *args*))
		(runInterpreter)))
(main)