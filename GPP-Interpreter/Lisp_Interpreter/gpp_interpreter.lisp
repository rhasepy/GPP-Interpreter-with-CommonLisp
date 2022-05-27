;; Muharrem Ozan Yeşiller
;; 171044033 
;; CSE341 HW2
;; G++ INTERPRETER

(defvar KEYWORD_LIST (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(defvar KW_LITERALS (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(defvar OPERATOR_LIST (list "+" "-" "/" "**" "*" "(" ")" "\"" "\"" ","))
(defvar OP_TOKEN_LIST (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_DBLMULT" "OP_MULT" "OP_OP" "OP_CP" "OP_OC" "OP_CC" "OP_COMMA"))

;; counter for paranthes
(defvar paranthes_count 0)
;; all tokens parsed
(defvar tokens '())

;; tokens will parse
(defvar token_list '())

;; map table for identifier and values ( (id0, value0), (id1, value1) ... )
(defvar memory_table '())

;; function table (memory that stores the functions)
;; format ( (function1,parameters,expression list) , (function2,parameters,expression list) ..)
(defvar function_table '())

;;;;;;;;; lexical analysis for parse operation ;;;;;;;;;

;; get lines
(defun get-lines (filename)
	(let ((in (open filename :if-does-not-exist nil)) (contextFile '()))
		(when in
			(loop for line = (read-line in nil)
			while line do (setq contextFile (append contextFile (list line))))
			(close in))
			contextFile))

;; parse the given line
(defun parseLine (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\Space string :start start)
        collecting (string-trim '(#\Space #\Tab #\Newline) (subseq string start finish))
        until (null finish)))

;; find a string in list
(defun search-list (string list)
	(let ((i 0))
		(loop for str in list
			do
			(if (string= str string)
				(return-from search-list i)
				(incf i)))
	nil))

;; chech that given token is value or not
(defun isIdentifier (token)
	(if (digit-char-p (aref token 0))
		(return-from isIdentifier nil))

	(loop for char across token
		do
		(if (not (or (alpha-char-p char) (char= char #\_) (digit-char-p char)))
			(return-from isIdentifier nil)))
	t)

;; check that given token is value or not
(defun isValue (token)
	(loop for char across token
		do
		(if (not (digit-char-p char))
			(return-from isValue nil)))
	t)

;; print the token type 
(defun collectTokenType (token token_val)
	
	(setq token_list (append token_list (list token_val)))

)
;; print syntax error message 
(defun syntaxError (token)
	(format t "(!) Syntax Error: ~S can not be tokenized!" token)
	(terpri))

;; parse the given line and iterate over it
(defun parse-token-line (line)
	(let ((token_check 0) (temptoken))
		(loop for token in (parseLine line)
			do
			(progn
				(setq temptoken (string-trim '(#\Space #\Tab #\Newline) token))
				(setq token_check (get-token-type temptoken))
				(if (or (= token_check 2) (= token_check -1)) (return token_check))
			))
		token_check))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check the next token for lexer check
(defun checkNextToken (token all_tokens subtoken i j len token_type)
	(let((check 0) (output '()) (temp_token (subseq token i (+ i 1))))
	 	(if (search-list temp_token '("(" ")" "\""))
		(progn
			(setq all_tokens (append all_tokens (list subtoken)))
			(setq j i)
			(setq check 1)
			(format nil "~{~A~}"(reverse (cdr (coerce (reverse token) 'list))))
			(if (string= token_type "IDENTIFIER")
				(collectTokenType token_type subtoken)
				(collectTokenType token_type (subseq token 0 j))))
		(progn
			(setq check -1)
			(syntaxError(subseq token j len))))
	 	(setq output (append output (list all_tokens)))
	 	(setq output (append output (list j)))
	 	(setq output (append output (list check)))))

;; test the opreator token and next token for control.
(defun TOKEN_operatorCheck (lexer_check token subtoken paranthes_count tokens j i len temp_token)

	(let ((output '()) (token_check) (tmp_out '()))
		;; operator lexer_check
		(if (= lexer_check 0)
			(progn
				(setq token_check (search-list subtoken OPERATOR_LIST))
				(if token_check
					(progn
						(if (string= (nth token_check OP_TOKEN_LIST) "OP_OC") 
							(progn 
								(setq token_check (+ token_check (mod paranthes_count 2)))
								(incf paranthes_count))) ;; increment the counter
						;; test "**" operator
						(if (string= (nth token_check OP_TOKEN_LIST) "OP_MULT")
							(if (and (< i len) (string= (subseq token i (+ i 1)) "*"))
								(setq token_check 3)))

						(if (or (string= (nth token_check OP_TOKEN_LIST) "OP_OP") (string= (nth token_check OP_TOKEN_LIST) "OP_CP") (string= (nth token_check OP_TOKEN_LIST) "OP_OC") (string= (nth token_check OP_TOKEN_LIST) "OP_COMMA"))
							(progn
								(setq tokens (append tokens (list subtoken)))
								(setq j i)
								(setq lexer_check 1)
								(collectTokenType (nth token_check OP_TOKEN_LIST) subtoken))
							
							(if (>= i len)
								(progn
									(setq tokens (append tokens (list subtoken)))
									(setq lexer_check 1)
									(collectTokenType (nth token_check OP_TOKEN_LIST) subtoken))
								(progn
								 	(setq tmp_out (checkNextToken token tokens subtoken i j len (nth token_check OP_TOKEN_LIST)))
								 	(setq tokens (nth 0 tmp_out))
								 	(setq j (nth 1 tmp_out))
								 	(setq lexer_check (nth 2 tmp_out))))))))
			(return-from TOKEN_operatorCheck nil))

		(setq output (append output (list tokens)))
		(setq output (append output (list j)))
		(setq output (append output (list lexer_check)))
		(setq output (append output (list token_check)))))

;; test the value token and next token for control.
(defun TOKEN_valueCheck (lexer_check token subtoken paranthes_count tokens j i len temp_token)
	(let ((tmp_out '()) (output '())) 
		(if (= lexer_check 0)
			(progn
				(if (isValue subtoken)
					(progn
						(loop while (and (<= i len) (isValue temp_token))
							do
							(setq temp_token (string-downcase (subseq token j i)))
							(incf i))
						(decf i)
						(if (equal (isValue temp_token) nil) 
							(decf i))								
						(if (>= i len)
							(progn
								(setq tokens (append tokens (list subtoken)))
								(setq lexer_check 1)
								(collectTokenType "VALUE" (subseq token j len)))
							(progn
							 	(setq tmp_out (checkNextToken token tokens subtoken i j len "VALUE"))
							 	(setq tokens (nth 0 tmp_out))
							 	(setq j (nth 1 tmp_out))
							 	(setq lexer_check (nth 2 tmp_out)))))))
			(return-from TOKEN_valueCheck nil))
	
	(setq output (append output (list tokens)))
	(setq output (append output (list j)))
	(setq output (append output (list lexer_check)))
	(setq output (append output (list i)))))
	
;; test the keyword token and next token for control.
(defun TOKEN_keywordCheck (lexer_check token subtoken paranthes_count tokens j i len temp_token)
	(let ((tmp_out '()) (output '())) 
		(if (= 0 lexer_check)
				(progn
					(setq token_check (search-list subtoken KEYWORD_LIST))
					(if token_check
						(if (>= i len)
							(progn
								(setq tokens (append tokens (list subtoken)))
								(setq lexer_check 1)
								(collectTokenType (nth token_check KW_LITERALS) subtoken))
							(progn
							 	(setq temp_token (subseq token i (+ i 1)))
							 	(if (search-list temp_token '("(" ")" "\""))
							 		(progn
							 			(setq tokens (append tokens (list subtoken)))
							 			(setq j i) 
							 			(setq lexer_check 1)
							 			(collectTokenType (nth token_check KW_LITERALS) subtoken))
							 		(if (equal (isIdentifier (concatenate 'string subtoken temp_token)) nil) 
							 			(progn
							 				(setq lexer_check -1)
							 				(syntaxError (subseq token j len)))))))))
				(return-from TOKEN_keywordCheck nil))
	
	(setq output (append output (list tokens)))
	(setq output (append output (list j)))
	(setq output (append output (list lexer_check)))))

;; test the identifier token and check the next token for control.
(defun TOKEN_idefCheck (lexer_check token subtoken paranthes_count tokens j i len temp_token)
	(let ((tmp_out '()) (output '())) 
		(if (= lexer_check 0)
				(progn
					(setq token_check (isIdentifier subtoken))
					(if (equal token_check t)
						(if (= i len)
							(progn 
								(setq tokens (append tokens (list subtoken)))
								(setq lexer_check 1)
								(collectTokenType "IDENTIFIER" subtoken))
							(progn
								(setq temp_token (string-downcase (subseq token j (+ i 1))))
								(if (not (equal token_check (isIdentifier temp_token)))
									(progn
									 	(setq tmp_out (checkNextToken token tokens subtoken i j len "IDENTIFIER"))
									 	(setq tokens (nth 0 tmp_out))
									 	(setq j (nth 1 tmp_out))
									 	(setq lexer_check (nth 2 tmp_out))))))
						(progn 
							(setq lexer_check -1) 
							(syntaxError (subseq token j len)))))
				
				(return-from TOKEN_idefCheck nil))
	
	(setq output (append output (list tokens)))
	(setq output (append output (list j)))
	(setq output (append output (list lexer_check)))))

(defun TOKEN_commentCheck (lexer_check token subtoken paranthes_count tokens j i len temp_token)
	(let ((tmp_out '()) (output '())) 
		(if (and (= lexer_check 0) (string= subtoken ";"))
				(if (and (< i len) (string= (subseq token i (+ i 1)) ";"))
					(progn 
						(setq tokens (append tokens (list "COMMENT")))
						(setq j i)
						(setq lexer_check 2)
						(collectTokenType "COMMENT" token)))
				(return-from TOKEN_commentCheck nil))
	
	(setq output (append output (list tokens)))
	(setq output (append output (list j)))
	(setq output (append output (list lexer_check)))))

(defun get-token-type (token)
	; iterate over the string by two pointers(indices) (i and j)
	(let ((i 1) (j 0) (tmp_out '()) (len (length token)) (tmplen) (subtoken) (token_check) (temp_token) (lexer_check 0))
		(loop while (<= i len)
			do
			(if (= 1 lexer_check) 
				(setq lexer_check 0))
			(setq subtoken (string-downcase (subseq token j i)))
			
			;; control token, check if it is value.
			(setq tmp_out (TOKEN_valueCheck lexer_check token subtoken paranthes_count tokens j i len temp_token))
			(if tmp_out
				(progn
					(setq j (nth 1 tmp_out))
					(setq lexer_check (nth 2 tmp_out))
					(setq i (nth 3 tmp_out))))

			;; control token, check if it is operator.
			(setq tmp_out (TOKEN_operatorCheck lexer_check token subtoken paranthes_count tokens j i len temp_token))
			(if tmp_out
				(progn
					(setq j (nth 1 tmp_out))
					(setq lexer_check (nth 2 tmp_out))
					(setq token_check (nth 3 tmp_out))))
			

			;; control token, check if it is keyword.
			(setq tmp_out (TOKEN_keywordCheck lexer_check token subtoken paranthes_count tokens j i len temp_token))
			(if tmp_out
				(progn
					(setq j (nth 1 tmp_out))
					(setq lexer_check (nth 2 tmp_out))))

			;; control token, check if it is IDENTIFIER.
			(setq tmp_out (TOKEN_idefCheck lexer_check token subtoken paranthes_count tokens j i len temp_token))
			(if tmp_out 
				(progn 
					(setq j (nth 1 tmp_out))
					(setq lexer_check (nth 2 tmp_out))))

			;; control token, check if it is comment.
			(setq tmp_out (TOKEN_commentCheck lexer_check token subtoken paranthes_count tokens j i len temp_token))
			(if tmp_out
				(progn
					(setq tokens (nth 0 tmp_out))
					(setq j (nth 1 tmp_out))
					(setq lexer_check (nth 2 tmp_out))))
			
			(setq tmplen (list-length tokens))

			(if (> tmplen 2)
			(if (and (string= (nth (- tmplen 3) tokens) "(") (string= (nth (- tmplen 2) tokens) "exit")  (string= (nth (- tmplen 1) tokens) ")"))
				(setq lexer_check -1)))

			(if (or (= lexer_check -1) (= lexer_check 2)) 
				(return lexer_check))
			(incf i)	
		)
		lexer_check			
	)
)

;;;;;;;;;;; PARSER FUNCTIONS ;;;;;;;;;;;;;;;;;;

;; get target closing paranthes index of open paranthes 
(defun get_target_cp (extracted_tokens token_length i)
	(let((target_j -1) (op_ctr 0) (found 0) (si i) (token_length (list-length extracted_tokens)))
		(loop while (and (= found 0) (< si token_length))
			do
			(if (string= "(" (nth si extracted_tokens))
				(setq op_ctr (+ 1 op_ctr)))
			(if (string= ")" (nth si extracted_tokens))
				(progn
					(setq op_ctr (- op_ctr 1))
					(if (= op_ctr 0)
						(progn
							(setq target_j si)
							(setq found 1)))))
			(setq si (+ 1 si))

		)
		(if (= -1 target_j)
			(progn
				(print "(!) Syntax Error! Paranthes don't match!")
				(exit)))
		target_j)
)

;; Parser calls this function in order to collect the related tokens about
;; current evaluating expression. It collects the tokens (non-terminals or terminals) 
;; according to production rules.
(defun collect_operands (i j extracted_tokens token_length)

	(let ((operands '()) (target_j) (opr_list '()) (k 0))
		(loop while (<= i j)
			do
			(if (string= "(" (nth i extracted_tokens))
				(progn
					(setq target_j (get_target_cp extracted_tokens (list-length extracted_tokens) i))
					(loop while (<= (+ i k) target_j)
						do
						(setq opr_list (append opr_list (list (nth (+ i k) extracted_tokens))))
						(setq k (+ k 1))
					)
					(setq k 0)
					(setq operands (append operands (list opr_list)))
					(setq i (+ target_j 1))
					(setq opr_list '())
				)
				(progn
					(setq opr_list (append opr_list (list (nth i extracted_tokens))))
					(setq operands (append operands (list opr_list)))
					(setq i (+ i 1))
					(setq opr_list '())
				)
			)
		)
		operands
	)
)

(defun valid_operand_check_number(tmp_res1)
	(if (not (atom tmp_res1))
		(progn
			(format t "~%(!) GPP Syntax Error: argument (~S) is not real number." tmp_res1)
			(exit))))

(defun valid_operand_check_list(tmp_res1)
	(if (atom tmp_res1)
		(progn
			(format t "~%(!) GPP Syntax Error: argument (~S) is not a list." tmp_res1)
			(exit))))

;; Lexer collects the tokens, and this function analyze these tokens using production rules
;; to check the validty of the expressions.
;; This function evalutes given expression according to CFG algorithm recursively.
;;
;; PRODUCTION RULES :
;;
;;
;;
(defun evaluate_expression(extracted_tokens token_length i j)

	(let ((result) (inner_res 0) (operands) (o_list) (k) (op_list) (tmp_res1) (tmp_res2) (param_list) (temp_table) (func_exec 0))
		(cond
			((string= "(" (nth i extracted_tokens))
				; if '(' operator achieved, go recursion to catch expressions.
				(setq result (evaluate_expression extracted_tokens token_length (+ i 1) (- (get_target_cp extracted_tokens (list-length extracted_tokens) i) 1)))
			)
			((string= "+" (nth i extracted_tokens))
				(setq inner_res 0)
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (< (list-length operands) 2)
					(progn
						(format t "~%(!) GPP Syntax Error: + operation expects at least 2 arguments, found ~D." (list-length operands))
						(exit)))
				(loop for op_list in operands
					do
					(setq tmp_res1 (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))
					(valid_operand_check_number tmp_res1)
					(setq inner_res (+ inner_res tmp_res1)))
				(setq result inner_res)
				(setq tmp_res1 nil)
				(setq tmp_res2 nil)
			)
			((string= "-" (nth i extracted_tokens)) ; - operator evaluation
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (< (list-length operands) 2)
					(progn
						(format t "~%(!) GPP Syntax Error: - operation expects at least 2 arguments, found ~D." (list-length operands))
						(exit)))
				(setq op_list (nth 0 operands))
				(setq inner_res (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))
				(valid_operand_check_number inner_res)
				(setq k 1)
				(loop while (< k (list-length operands))
					do
					(setq op_list (nth k operands))
					(setq k (+ k 1))
					(setq tmp_res1 (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))
					(valid_operand_check_number tmp_res1)
					(setq inner_res (- inner_res tmp_res1)))
				(setq result inner_res)
				(setq k 0)
				(setq op_list '())
				(setq tmp_res1 nil)
				(setq tmp_res2 nil)
			)
			((string= "*" (nth i extracted_tokens)) ; * operator evaluation
				(setq inner_res 1)
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))
				(if (< (list-length operands) 2)
					(progn
						(format t "~%(!) GPP Syntax Error: * operation expects at least 2 arguments, found ~D." (list-length operands))
						(exit)))
				(loop for op_list in operands
					do
					(setq tmp_res1 (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))
					(valid_operand_check_number tmp_res1)
					(setq inner_res (* inner_res tmp_res1)))
				(setq result inner_res)
				(setq tmp_res1 nil)
				(setq tmp_res2 nil)
			)
			((string= "/" (nth i extracted_tokens)) ; / operator evaluation
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (< (list-length operands) 2)
					(progn
						(format t "~%(!) GPP Syntax Error: / operation expects at least 2 arguments, found ~D." (list-length operands))
						(exit)))
				(setq op_list (nth 0 operands))
				(setq inner_res (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))
				(valid_operand_check_number inner_res)
				(setq k 1)
				(loop while (< k (list-length operands))
					do
					(setq op_list (nth k operands))
					(setq k (+ k 1))
					(setq tmp_res1 (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))
					(valid_operand_check_number tmp_res1)
					(setq inner_res (/ inner_res tmp_res1)))
				(setq result inner_res)
				(setq k 0)
				(setq op_list '())
				(setq tmp_res1 nil)
				(setq tmp_res2 nil)
			)
		
			((string= "disp" (nth i extracted_tokens)) ; disp keyword operation
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))
				(setq op_list (nth 0 operands))

				; argument check
				(if (not (= (list-length operands) 1)) 
					(progn
						(format t "~%(!) GPP Syntax Error: disp expects 1 argument, found ~D" (list-length operands))
						(exit)))
				(setq inner_res (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))
				(setq result inner_res)
				(print inner_res)
				(setq op_list '())
			)
			((string= "equal" (nth i extracted_tokens)) ; equal keyword operation
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (not (= (list-length operands) 2)) 
					(progn
						(format t "~%(!) GPP Syntax Error: equal expects 2 argument, found ~D" (list-length operands))
						(exit)))
				(setq inner_res (equal (evaluate_expression (nth 0 operands) (list-length (nth 0 operands)) 0 (- (list-length (nth 0 operands)) 1)) (evaluate_expression (nth 1 operands) (list-length (nth 1 operands)) 0 (- (list-length (nth 1 operands)) 1))))
				(setq result inner_res)
			)
			((string= "less" (nth i extracted_tokens)) ; less keyword operation
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (not (= (list-length operands) 2)) 
					(progn
						(format t "~%(!) GPP Syntax Error: less expects 2 argument, found ~D." (list-length operands))
						(exit)))
				(setq tmp_res1 (evaluate_expression (nth 0 operands) (list-length (nth 0 operands)) 0 (- (list-length (nth 0 operands)) 1)))
				(setq tmp_res2 (evaluate_expression (nth 1 operands) (list-length (nth 1 operands)) 0 (- (list-length (nth 1 operands)) 1)))
				(valid_operand_check_number tmp_res1)
				(valid_operand_check_number tmp_res2)
				(setq inner_res (< tmp_res1 tmp_res2))
				(setq result inner_res)
				(setq tmp_res1 nil)
				(setq tmp_res2 nil)
			)
			((string= "nil" (nth i extracted_tokens)) ; nil keyword operation
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (not (= (list-length operands) 0)) 
					(progn
						(format t "~%(!) GPP Syntax Error: nil is constant, argument found ~D." (list-length operands))
						(exit)))
				(setq result nil)
			)
			;; EXPB START
			((string= "true" (nth i extracted_tokens)) ; true keyword operation
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (not (= (list-length operands) 0)) 
					(progn
						(format t "~%(!) GPP Syntax Error: true is constant, argument found ~D." (list-length operands))
						(exit)))
				(setq result t)
			)
			((string= "false" (nth i extracted_tokens)) ; false keyword operation
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (not (= (list-length operands) 0)) 
					(progn
						(format t "~%(!) GPP Syntax Error: false is constant, argument found ~D." (list-length operands))
						(exit)))
				(setq result nil)
			)
			((string= "and" (nth i extracted_tokens)) ; and keyword evaulation
				(setq inner_res t)
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (< (list-length operands) 2)
					(progn
						(format t "~%(!) GPP Syntax Error: and operation expects at least 2 arguments, found ~D." (list-length operands))
						(exit)))

				(loop for op_list in operands
					do
					(setq tmp_res1 (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))
					(setq inner_res (and inner_res tmp_res1)))
				(setq result inner_res)
			)
			((string= "or" (nth i extracted_tokens)) ; or keyword evaulation
				(setq inner_res nil)
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (< (list-length operands) 2)
					(progn
						(format t "~%(!) GPP Syntax Error: or operation expects at least 2 arguments, found ~D." (list-length operands))
						(exit)))

				(loop for op_list in operands
					do
					(setq tmp_res1 (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))
					(setq inner_res (or inner_res tmp_res1)))
				(setq result inner_res)
			)
			((string= "not" (nth i extracted_tokens)) ; not keyword evaulation
				(setq inner_res nil)
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (not (= (list-length operands) 1))
					(progn
						(format t "~%(!) GPP Syntax Error: or operation expects 1 argument, found ~D." (list-length operands))
						(exit)))

				(setq op_list (nth 0 operands))
				(setq result (not (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1))))	
				(setq op_list '())
			)

			((string= "if" (nth i extracted_tokens)) ; if keyword evaulation
				(setq inner_res nil)
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (< (list-length operands) 2)
					(progn
						(format t "~%(!) GPP Syntax Error: if operation expects at least 2 arguments, found ~D." (list-length operands))
						(exit)))
				(if (> (list-length operands) 3)
					(progn
						(format t "~%(!) GPP Syntax Error: if operation expects maximum 3 arguments, found ~D." (list-length operands))
						(exit)))

				(setq op_list (nth 0 operands))
				(setq inner_res (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1))) ; evaluate if expr
				(if inner_res
					(progn
						(setq op_list (nth 1 operands))
						(setq inner_res (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1))))
					(progn
						(setq op_list (nth 2 operands))
						(setq inner_res (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))))
				(setq result inner_res)
				(setq op_list '())
			)
			;; EXPB END

			;; EXPLISTI START
			((string= "list" (nth i extracted_tokens)) ; list keyword operation
				(setq inner_res '())
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (< (list-length operands) 1)
					(progn
						(format t "~%(!) GPP Syntax Error: list operation expects at least 1 arguments, found ~D." (list-length operands))
						(exit)))
				(loop for op_list in operands
					do
					(setq inner_res (append inner_res (list (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))))
				)
				(setq result inner_res)
			)

			((string= "append" (nth i extracted_tokens)) ; append keyword evaulation
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (< (list-length operands) 2)
					(progn
						(format t "~%(!) GPP Syntax Error: append operation expects at least 2 arguments, found ~D." (list-length operands))
						(exit)))

				(setq op_list (nth 0 operands))
				(setq inner_res (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))
				(valid_operand_check_list inner_res)
				(setq k 1)
				(loop while (< k (list-length operands))
					do
					(setq op_list (nth k operands))
					(setq k (+ k 1))
					(setq tmp_res1 (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))
					(valid_operand_check_list tmp_res1)
					(setq inner_res (append inner_res tmp_res1)))
				(setq result inner_res)
				(setq k 0)
				(setq op_list '())
			)
			((string= "concat" (nth i extracted_tokens)) ; concat keyword evaulation
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))

				; argument check
				(if (< (list-length operands) 2)
					(progn
						(format t "~%(!) GPP Syntax Error: concat operation expects at least 2 arguments, found ~D." (list-length operands))
						(exit)))

				(setq op_list (nth 0 operands))
				(setq inner_res (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))
				(valid_operand_check_list inner_res)
				(setq k 1)
				(loop while (< k (list-length operands))
					do
					(setq op_list (nth k operands))
					(setq k (+ k 1))
					(setq tmp_res1 (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))
					(valid_operand_check_list tmp_res1)
					(setq inner_res (append inner_res tmp_res1)))
				(setq result inner_res)
				(setq k 0)
				(setq op_list '())
			)
			;; EXPLISTI END

			((string= "set" (nth i extracted_tokens)) ; set keyword evaulation
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))
				(if (not (= (list-length operands) 2))
					(progn
						(format t "~%(!) GPP Syntax Error: set operation expects 2 arguments, found ~D." (list-length operands))
						(exit)))
				
				; store variable name
				(setq tmp_res1 (nth 0 operands))
				(setq op_list (nth 1 operands))
				; store variable value
				(setq tmp_res2 (evaluate_expression op_list (list-length op_list) 0 (- (list-length op_list) 1)))
				
				; update the symbol table
				; if new variable add new entry, if variable exists, change the value
				(if (= 0 (list-length memory_table))
					(setq memory_table (append memory_table (list (list (nth 0 tmp_res1) tmp_res2))))
					(progn
						(setq inner_res 0)
						(setq k 1)
						(loop while (and (= k 1)(< inner_res (list-length memory_table)))
							do
							(if (string= (nth 0 (nth inner_res memory_table)) (nth 0 tmp_res1)) 
								(progn
									(setf (nth 1 (nth inner_res memory_table)) tmp_res2)
									(setq k 0))
								(progn
									(setq memory_table (append memory_table (list (list (nth 0 tmp_res1) tmp_res2))))
									(setq k 0))) 
							(setq inner_res (+ 1 inner_res))
						)
					)
				) 
				
				(setq result tmp_res2)
				(setq tmp_res1 nil)
				(setq tmp_res2 nil)
				(setq k nil)

			)
			((string= "exit" (nth i extracted_tokens))
				(exit)
			)
			((string= "deffun" (nth i extracted_tokens))
				(setq i (+ i 1))
				(setq operands (collect_operands i j extracted_tokens token_length))
				(setq param_list (subseq (nth 1 operands) 1 (- (list-length (nth 1 operands)) 1)))
				;set new function table entry
				(setq tmp_res1 (append (nth 0 operands) (list param_list)))

				;; check that function name is a keyword name
				(loop for kw in KEYWORD_LIST
					do
					(if (string= kw (nth 0 (nth 0 operands)))
						(progn
							(format t "~%(!) GPP Syntax Error: ~S is a reserved keyword." kw)
							(exit))))

				;; check that function is defined before
				(loop for funct in function_table
					do
					(if (string= (nth 0 funct) (nth 0 (nth 0 operands)))
						(progn
							(format t "~%(!) GPP Syntax Error: redefinition of function : ~S" (nth 0 funct))
							(exit))))

				;; collect all expressions of function
				(setq tmp_res2 2)
				(setq o_list '())
				(loop while (< tmp_res2 (list-length operands))
					do
					(setq o_list (append o_list (list (nth tmp_res2 operands))))
					(setq tmp_res2 (+ 1 tmp_res2)))	

				(setq tmp_res1 (append tmp_res1 (list o_list)))
				;; add new function to function table
				(setq function_table (append function_table (list tmp_res1)))

				(setq tmp_res1 nil)
				(setq tmp_res2 nil)
				(setq o_list nil)
			)
			((= 1 1)
				(setq func_exec 0)
				(setq tmp_res1 (parse-integer (nth i extracted_tokens) :junk-allowed t))
				(setq result nil)
				(if tmp_res1
					(setq result (parse-integer (nth i extracted_tokens)))
					(progn
						;; look symbol table
						(setq tmp_res2 0)
						(loop while (and (equal nil result) (< tmp_res2 (list-length memory_table)))
							do
							(setq symbol (nth tmp_res2 memory_table))
							(if (string= (nth i extracted_tokens) (nth 0 symbol)) ; check if variable (symbol) found in memory
								(setq result (nth 1 symbol)))
							(setq tmp_res2 (+ 1 tmp_res2)))
						
						;; look function table, if found, execute function
						(if (equal nil result)
							(progn
								(setq tmp_res2 0)
								(loop while (and (equal nil result) (< tmp_res2 (list-length function_table)))
									do
									(setq symbol (nth tmp_res2 function_table))
									(if (string= (nth i extracted_tokens) (nth 0 symbol))
										(setq result symbol))
									(setq tmp_res2 (+ 1 tmp_res2)))

								(if result
									(progn
										(setq i (+ i 1))
										(setq operands (collect_operands i j extracted_tokens token_length))
										(setq param_list '())
										(setq i (- i 1))
										
										;; collect parameter values
										(loop for param in operands
											do
											(setq param_list (append param_list (list (evaluate_expression param (list-length param) 0 (- (list-length param) 1))))))

										;; parameter number check
										(if (not (= (list-length param_list) (list-length (nth 1 result))))
											(progn
												(format t "~%(!) GPP Syntax Error: ~S function expects ~D arguments, found ~D." (nth 0 result) (list-length (nth 1 result)) (list-length operands))
												(exit)))

										;; binding the parameters, assigning values to the parameters
										(setq temp_table memory_table)
										(setq memory_table '())
										(setq tmp_res2 0)
										(loop for value in param_list
											do
											(setq memory_table (append memory_table (list (list (nth tmp_res2 (nth 1 result)) value))))
											(setq tmp_res2 (+ 1 tmp_res2)))

										;; execute function expressions
										(loop for expr in (nth 2 result)
											do
											(setq result (evaluate_expression expr (list-length expr) 0 (- (list-length expr) 1))))
										(setq func_exec 1)

										;; fix the memory 
										(setq memory_table temp_table)))
							)
						)	
					)
				)

				(setq tmp_res1 nil)
				(setq tmp_res2 nil)
				
				; if result is still nil, that means syntax error
				(if (and (= func_exec 0 )(equal nil result))
					(progn
						(format t "~%(!) GPP Syntax Error: symbol not recognized: ~S." (nth i extracted_tokens))
						(exit)))
			)
		)
		result
	)
)

(defun gppinterpreter (args)
	(let ((contextFile) (input) (expres 0) (i 0) (j 0) (all_expressions '()))
		(if args
			(progn
				(setq contextFile (get-lines (nth 0 args))) 
				
				; parse file, (lexer)
				(loop for line in contextFile
					do
					(parse-token-line line))

				; collect all expressions from source code file
				(loop while (< i (list-length token_list))
					do
					(setq j (get_target_cp token_list (list-length token_list) i))
					(setq all_expressions (append all_expressions (list (subseq token_list i (+ 1 j)))))
					(setq i (+ 1 j)))

				; evaluate all expressions (parser, interpreter)
				(loop for expr in all_expressions
					do
					(evaluate_expression expr (list-length expr) 0 (- (list-length expr) 1)))
			)
			(progn
				(format t "Gpp Interpreter Started. Type (exit) for exit~%")
				(terpri)(terpri)
				(setq i 0)
				(loop while(= -1 -1)
					do
					(setq input (read-line))
					(parse-token-line input)
					(setq j (get_target_cp token_list (list-length token_list) i))
					(setq expres (subseq token_list i (+ 1 j)))
					(evaluate_expression expres (list-length expres) 0 (- (list-length expres) 1))
					(setq token_list nil)
					(terpri)(terpri))))))


;; start the gppinterpreter with command line arguments
(gppinterpreter *args*)
