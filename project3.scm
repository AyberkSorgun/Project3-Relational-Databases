;;; YOUR NAME: Ayberk Sorgun        		 	
;;;         		 	
;;; Comp200 Project 3         		 	
;;;         		 	
;;; Before you start:         		 	
;;;         		 	
;;; * Please read the detailed instructions for this project from the
;;; file project3.pdf available in the Assignments section of the
;;; course website.         		 	
;;;         		 	
;;; * Please read "Project Submission Instructions" carefully and make
;;; sure you understand everything before you start working on your
;;; project in order to avoid problems.
;;;         		 	
;;; While you are working:         		 	
;;; * Type all your work and notes in the appropriate sections of this file.
;;; * Please do not delete any of the existing lines.
;;; * Use the procedure names given in the instructions.
;;; * Remember to frequently save (C-x C-s in edwin) your file.
;;; * Use semicolon (;) to comment out text, tests and unused code.
;;; * Remember to document your code.
;;; * Remember our collaboration policy: you can discuss with your friends but:
;;;         		 	
;;;   *** NOTHING WRITTEN GETS EXCHANGED ***
;;;         		 	
;;; When you are done:         		 	
         		 	
;;; * Perform a final save and copy the file to the following location
;;;   DriveF@VOL\UGRADS\COMP200\HOMEWORK\username\project3.zip
;;;   where the username is your login name.
;;; * Please do not make any modifications after midnight on the due date.
;;; * Please send an email comp200@ku.edu.tr if you have any questions.
;;; * Make sure your file loads without errors:
;;;         		 	
;;; *** IF (load "project3.scm"), ALT-O(MIT/GNU Scheme) or Run
;;; button(DrScheme0 GIVES ERRORS YOUR PROJECT WILL NOT BE GRADED ***
;;;         		 	
;;; Before the definition of each procedure, please write a description
;;; about what the procedure does and what its input and output should
;;; be, making sure the lines are commented out with semi-colons.
(define your-answer-here -1)         		 	
(load "databases.scm")        		 	
         		 	
;;; problem 1 ;;;         		 	
         		 	
;; your code should have the following general form
         		 	
(define example-table         		 	
  (make-empty-table (list (make-column 'name 'symbol)
                          (make-column 'major 'number)))         		 	
   )

;;example-table creates an empty table with the associated columns
(table-insert! (list 'jen 3) example-table )
(table-insert! (list 'ben 6) example-table )
(table-insert! (list 'alex 6) example-table )
(table-insert! (list 'amy 12) example-table )
(table-insert! (list 'kim 13) example-table )

         		 	
;; test cases         		 	
 (table-display example-table)
         		 	
;;; problem 2 ;;;

;;table-insert-all! takes a list of rows and insertts the one by one to the table, recursively.
(define (table-insert-all! lst table)
(if (null? lst) table (table-insert-all! (cdr lst) (table-insert! (car lst) table)))         		 	
)         		 	
;; test cases         		 	
         		 	
 (define books (make-empty-table
 	       (list (make-column 'title 'symbol)
 		     (make-column 'author 'symbol)
 		     (make-column 'rating 'number))))
         		 	
         		 	
 (table-insert-all! '((sicp abelson-sussman 8)
 		     (return-of-the-king jrr-tolkien 9)
 		     (treatment-of-subordinates darth-vader 4)
 		     (project-grading tom 2)
 		     (all-things-stata frank-gehry 5)
 		     (biting-the-hand-that-feeds-me my-cat 1))
 		   books)         		 	
 (table-display books)         		 	
         		 	
;;; problem 3 ;;;         		 	
;; Hint: Writing (filter predicate lst) might be helpful
         		 	
(define (table-select selector table)
(make-table (get-table-columns table) (filter selector (get-table-data table)))         		 	
  )         		 	
;;table-select takes a predicate and applies to the rows of table
;; and adds the ones that retuns true to a new empty table
;; test cases         		 	
         		 	
 (display "Testing Problem 3\n")
 (table-display         		 	
  (table-select         		 	
   (lambda (row)         		 	
     (> (get 'rating row) 4))
   books))         		 	
         		 	
;;; problem 4 ;;;         		 	
         		 	
;; Hint: Be careful about the comparator operator of the corresponding
;; row.  Writing a (get-column-type row column-name) might be helpful.
;;table-order-by takes a column, by using  its type sorts the corresponding values of the
;;elements(rows).

(define (table-order-by column table)
  
(make-table (get-table-columns table) (sort (make-row-comparator column table) (get-table-data table))))         		 	
;; test cases         		 	
 (display "Testing Problem 4\n")
 (table-display         		 	
  (table-order-by 'rating books)
 )         		 	
         		 	
 (table-display         		 	
  (table-order-by 'title books)
 )         		 	
         		 	
;;; problem 5 ;;;         		 	
         		 	
(define (table-delete! pred table)
       (change-table-data! table (filter  (lambda (x) (not (pred x))) (get-table-data table)))  		 	
  )         		 	
;;table-delete! deletes the element that returns true the pred. It uses filter to detect the
;;deleted element.
;; test cases         		 	
 (display "Testing Problem 5\n")
 (table-delete!         		 	
  (lambda (row)         		 	
   (eq? (get 'author row) 'my-cat))
 books)         		 	
         		 	
 (table-display books)         		 	
         		 	
;;; problem 6 ;;;
;; table-update! is for editingg a specific rows data, by detecting it with pred and update the table
(define (table-update! pred column proc table) 

(define (map* row)

(if (pred row)

(let* ((row-new (row-col-replace row column (proc row))))
(if (row-type-check row-new)
	row-new
	'type-mismatch-error))
	row))
(let* (
(rowdata (get-table-data table))
(row-new (map map* rowdata))
)
(make-table (get-table-columns table)row-new)))	
;; test cases         		 	
         		 	
 (display "Testing Problem 6\n")
 (table-update! (lambda (row) (or (eq? (get 'name row) 'amy) (eq? (get 'name row) 'alex)))
               'major         		 	
               (lambda (row) '9)
               example-table)
 (table-display example-table)
         		 	
;;; problem 7 ;;;         		 	
 ;we add a new type, string, to our types for creating the name variables with that new type.       		 	
(define *type-table*         		 	
  (list         		 	
        (list 'number number? <)
        (list 'symbol symbol? symbol<?)
        (list 'string string? string<?)))         		 	
         		 	
(define example-table2         		 	
(make-empty-table (list (make-column 'name 'string)
                          (make-column 'major 'number)))         		 	
   )         		 	
;;We create the String type because symbols produces unique variables,
;;means that we can't create two different person with same name
;; but in strings, we can create a variable with same value and they points different objects in memory.
;; test cases         		 	
 (display "Testing Problem 7\n")
 (table-insert! '("jen" 3) example-table2)
 (table-insert! '("ben" 6) example-table2)
 (table-insert! '("alex" 6) example-table2)
 (table-insert! '("amy" 12) example-table2)
 (table-insert! '("kim" 13) example-table2)
         		 	
         		 	
 (table-display example-table2)
 (display "\nordered example-table2\n")
 (table-display         		 	
  (table-order-by 'name example-table2)
 )         		 	
         		 	
;;; problem 8 ;;;         		 	
         		 	
;; Hint: Writing these two procedures might be helpful (contains? lst
;; x) returns true if x in the lst and (get-pos lst x) returns the
;; position of x if it is in the list.
;; Ex: (get-pos '(1 2 3 4) 2) => 2
;;     (get-pos '(1 2 3 4) 5) => 0
;; get-pos gives us the position of an object, in orderto use this data in a comparator
;; contains? gives us the informatin whether the input that we are looking for is in a list
(define (get-pos lst x)
  
    (define (helper lst x counter)
      (cond
             ((null? lst) 0)
             ((eq? (car lst) x ) counter)
             (else (helper (cdr lst) x (+ counter 1)))))
    (helper lst x 1))
(define (contains? lst x)

   (cond
     ((null? lst) #f)
     ((eq? (car lst) x) #t)
     (else (contains? (cdr lst) x))))

(define (make-enum-checker lst)
(lambda(elt) (contains? lst elt))         		 	
  )         		 	
(define (make-enum-comparator lst)
(lambda (elt1 elt2) (if (< (get-pos lst elt1) (get-pos lst elt2)) #t #f ))         		 	
)         		 	
(define *days* '(sunday monday tuesday Wednesday thursday friday saturday))
(define day-checker (make-enum-checker *days*))
(define day-comparator (make-enum-comparator *days*))
         		 	
;; test cases         		 	
 (display "Testing Problem 8\n")
 (day-checker 'monday)   ;=> #t
 (day-checker 7)         ;=> #f
 (day-comparator 'monday 'tuesday)   ;=> #t (monday is "less than" tuesday)
 (day-comparator 'friday 'sunday)    ;=> #f (sunday is before friday)
         		 	
         		 	
(define *type-table*         		 	
(list         		 	
        (list 'number number? <)
        (list 'symbol symbol? symbol<?)
        (list 'string string? string<?)
        (list 'day day-checker day-comparator))         		 	
)         		 	
         		 	
 (define example-table3         		 	
   (make-empty-table         		 	
    (list (make-column 'name 'string)
          (make-column 'date 'day)
          (make-column 'major 'number)))
    )         		 	
         		 	
 (table-insert! '("jen" monday 3) example-table3)
 (table-insert! '("ben" sunday 6) example-table3)
 (table-insert! '("alex" friday 6) example-table3)
 (table-insert! '("amy" tuesday 1) example-table3)
 (table-insert! '("kim" saturday 2) example-table3)
         		 	
 (table-display example-table3)
 (display "\nordered example-table3\n")
 (table-display         		 	
  (table-order-by 'date example-table3)
 )         		 	
         		 	
;;; Problem 9         		 	
;; Hint: Similar with Problem 8
;; gender and race are two of our new columns. their checkers and cmparators are implemented with contains? and get-pos methods, respectively.
(define *gender* '(male female))
(define gender-checker         		 	
(lambda(gender) (contains? *gender* gender))         		 	
)         		 	
(define gender-comparator         		 	
(lambda (elt1 elt2) (if (< (get-pos *gender* elt1) (get-pos *gender* elt2)) #t #f ))         		 	
)         		 	
(define *race* '(white black red))
         		 	
(define race-checker         		 	
(lambda(race) (contains? *race* race))         		 	
)         		 	
(define race-comparator         		 	
(lambda (elt1 elt2) (if (< (get-pos *race* elt1) (get-pos *race* elt2)) #t #f ))         		 	
)         		 	
         		 	
(define *type-table*
  (list
   (list 'number number? <)
   (list 'symbol symbol? symbol<?)
   (list 'string string? string<?)
   (list 'gender gender-checker gender-comparator)
   (list 'race race-checker race-comparator)
))   		 	
         		 	
;;; Problem 10         		 	
; In here, we are creating our table whose rows are people		 	
(define person-table         		 	
(make-empty-table (list (make-column 'name 'string)
          (make-column 'race 'race)
          (make-column 'gender 'gender)
          (make-column 'birthyear 'number)))         		 	
)         		 	
;;; tests         		 	
 (display "Testing Problem 10\n")
 (table-insert! '("jen" white female 1983) person-table)
 (table-insert! '("axe" black male 1982) person-table)
 (table-display person-table)
         		 	
         		 	
;;; Problem 11
;;table-delete! is for remove the rows we wanted to.
 (table-delete! (lambda (x) #t) person-table)
 (display "\nDeleted Person Table\n")
 (table-display person-table)
         		 	
(define (make-person name race gender birthyear)
        (table-insert! (list name race gender birthyear ) person-table)  		 	
  name)        		 	
         		 	
;; test cases         		 	
         		 	
 (display "Testing Problem 11\n")
         		 	
 (define p1 (make-person "Alex" 'white 'male 1983))
 (define p2 (make-person "Clark" 'black 'male 1982))
 (table-display person-table)
         		 	
;;; Problem 12         		 	
  ;;lookup-person-row provides the same meaning with contains? but does it in a table

 (define (person-name person) person)
         		 	
(define (lookup-person-row person)
 (let ((m (filter (lambda(x) (eq? person (get 'name x))) (get-table-data person-table))))
   (if (null? m) (make-row (get-table-columns person-table)(list "Not found" "Not found" "Not found" "Not found"))
                  (car m) ))         		 	
  )         		 	
         		 	
(define (person-race person)         		 	
  (get 'race (lookup-person-row person)))
         		 	
(define (person-gender person)         		 	
  (get 'gender (lookup-person-row person)))
         		 	
(define (person-birthyear person)
  (get 'birthyear (lookup-person-row person)))
         		 	
(define (person-age person)         		 	
; returns an approximation to the person's age in years
  (let ((*current-year* 2012))         		 	
    (- *current-year* (person-birthyear person))))
         		 	
;; test cases         		 	
 (display "Testing Problem 12\n")
 (lookup-person-row p1)         		 	
 (person-race p1)         		 	
 (person-gender p1)         		 	
 (person-birthyear p1)         		 	
 (person-age p1)         		 	
 (lookup-person-row "Muslera")
         		 	
;;; Problem 13         		 	
;this method updates a person's data with a given colname and value         		 	
(define (update-person-row! person colname newvalue)
      (table-update! (lambda (row) (eq? (get 'name row) person)) colname (lambda(row) newvalue) person-table))

         		 	
(define (set-person-name! person newname)
  (update-person-row! person 'name newname))
         		 	
(define (set-person-race! person newrace)
  (update-person-row! person 'race newrace))
         		 	
(define (set-person-gender! person newgender)
  (update-person-row! person 'gender newgender))
         		 	
(define (set-person-birthyear! person newbirthyear)
  (update-person-row! person 'birthyear newbirthyear))
         		 	
;; QUESTION What happens? Why? Comments?
;;when we call (person-name alyssa) it still returns like we created it in the first place.
;;;because we cannot set a variable inside a procedure while its given as an argument because of lexical scoping.
;;;We can set it in the procedure, but outside the proc, its stay as it is.
         		 	
;;; test cases         		 	
         		 	
 (display "Testing Problem 13\n")
 (define alyssa (make-person "alyssa-p-hacker" 'black 'female 1978))
 (set-person-name! alyssa "alyssa-p-hacker-bitdiddle")  ; got married!
 (table-display person-table)
 (person-name alyssa)
 (person-race alyssa)         		 	
         		 	
;;; Problem 14         		 	
;;; We create a new table with the format that we wanted to.      		 	
(define life-table         		 	
(make-empty-table (list (make-column 'year 'number)
                          (make-column 'all-all 'number)
                          (make-column 'all-male 'number)
                          (make-column 'all-female 'number)
                          (make-column 'white-all 'number)
                          (make-column 'white-male 'number)
                          (make-column 'white-female  'number)
                          (make-column 'black-all 'number)
                          (make-column 'black-male 'number)
                          (make-column 'black-female 'number)))         		 	
)         		 	

;(1952 68.6 65.8 71.6 69.5 66.6 72.6 61.4 59.1 63.8)
(table-insert-all! life-expect-data life-table) 
         		 	
 (display "Selecting 1950\n")
 (table-display         		 	
  (table-select         		 	
  (lambda (row)         		 	
    (= (get 'year row) 1950))
  life-table))         		 	
         		 	
;;; Problem 15
;;; At that part, we convert the table to a different format. this time each year has four entries as
;;;black male or female and white male or female
(define new-life-table         		 	
(make-empty-table (list (make-column 'year 'number)
                          (make-column 'race 'race)
                          (make-column 'gender 'gender)
                          (make-column 'expectedlifespan 'number)))         		 	
)              		 	
(define (convert-lifetable lst)
;; Converts the data to the (year race gender expected) format
(if(null? (cdr lst))
       (list (list (car (car lst)) 'black 'male (car  (cddddr (cddddr (car lst)))))
        (list (car (car lst)) 'black 'female (car (cdr (cddddr (cddddr (car lst))))))
         (list (car (car lst)) 'white 'male (car (cdr (cddddr  (car lst)))))
          (list (car (car lst)) 'white 'female (car (cddr (cddddr (car lst))))))

       (append (list(list (car(car lst)) 'black'male (car (cddddr (cddddr (car lst )))))
               (list (car (car lst)) 'black 'female (car (cdr (cddddr (cddddr (car lst ))))))
               (list (car (car lst)) 'white 'male (car (cdr (cddddr (car lst)))))
           (list (car (car lst)) 'white 'female (car (cddr (cddddr (car lst ))))))
(convert-lifetable (cdr lst)))))
 

         		 	
;; test cases         		 	
        		 	
         		 	
(define life-expect-data-new (convert-lifetable life-expect-data))
         		 	
(define life-table-new         		 	
   (make-empty-table         		 	
   (list (make-column 'year 'number)
         (make-column 'race 'race)
         (make-column 'gender 'gender)
         (make-column 'expected 'number)
   )))         		 	
(table-insert-all! life-expect-data-new life-table-new)
(display "Selecting 1950 from new data\n")
(table-display         		 	
(table-select         		 	
 (lambda (row)         		 	
  (= (get 'year row) 1950))         		 	
  life-table-new))     	 	
         		 	
;;; Problem 16
;;; At this problem, we sort the people born in between 1950-1959 whose white females
(display "\nTesting Problem 16\n")
(define problem16-table         		 	
     (table-order-by 'expected (make-table (get-table-columns life-table-new) (filter (lambda(x)  (and(and (< (get 'year x) 1960) (> (get 'year x) 1949))
                  (and (eq? (get 'race x) 'white) (eq? (get 'gender x) 'female))))        		 	
  (get-table-data life-table-new)))))          		 	
         		 	
;;; QUESTION Was life expectancy for white women steadily increasing
;;; in this decade?         		 	
;;yes         		 	
         		 	
(table-display         		 	
  (table-order-by 'expected problem16-table)
)         		 	
         		 	
;; Paste the output of Problem 16 for black female
;; between 1950 and 1959         		 	
         		 	
;;year	race	gender	expected	
;;1950	black	female	62.9	
;;1951	black	female	63.4	
;;1952	black	female	63.8	
;;1953	black	female	64.5	
;;1957	black	female	65.5	
;;1958	black	female	65.8	
;;1954	black	female	65.9	
;;1955	black	female	66.1	
;;1956	black	female	66.1	
;;1959	black	female	66.5         		 	
         		 	
;;; Problem 17
;;; at that point we have 2 separate person and we calculated their estimated-remaining lifetimes.
;;;We take the corresponding data from table and compute person's ages and find the difference in betwwen them (person age-expectancy)  
             
(define p3 (make-person "GeorgeBest" 'white 'female 1987))
(define p4 (make-person "Lizarazu" 'white 'male 1940))
(define (expected-years person)
  (let ((single (filter (lambda
                (row)
                (and (and (eq? (get 'year row) (person-birthyear person))
                (eq? (get 'gender row) (person-gender person)))
                (eq? (get 'race row) (person-race person)))
              ) (get-table-data life-table-new))))
    (- (get 'expected  (car single)) (- 2016   (person-birthyear person)))))
  

;; test cases                
(display "Testing problem 17")
(display "\n")
(expected-years p3)                
(expected-years p4)        		 	
