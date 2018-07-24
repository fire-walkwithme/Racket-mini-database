(define NULL 'null)

;====================================
         Definition of the
	 control elements
;====================================

;= Acces functions

(define init-database
  (λ ()
    '()))

(define create-table
  (λ (table columns-name)
    (cons table (list columns-name))))

(define get-name
  (λ (table)
    (car table)))

(define get-columns
  (λ (table)
    (cadr table)))

(define get-tables
  (λ (db)
    db))

(define get-table
  (λ (db table-name)
    (car (filter (lambda (x) (equal? (car x) table-name)) db))))

(define add-table
  (λ (db table)
    (append db (list table))))

(define remove-table
  (λ (db table-name)
    (filter (lambda (x) (not (equal? (car x) table-name))) db)))

(define (get-entries table)
  (cddr table))

(define (zip-aux l1 l2) (map cons l1 l2))

(define (zip table)
  (let [(new (map (lambda(x) (zip-aux (get-columns table) x)) (get-entries table)))]
  (apply append new)))

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================
(define db (list (list "Studenți" (list "Număr matricol" "Nume" "Prenume" "Grupă" "Medie")
                                  (list 123 "Ionescu" "Gigel" "321CA" 9.82)
                                  (list 124 "Popescu" "Maria" "321CB"  9.91)
                                  (list 125 "Popa" "Ionel" "321CC" 9.99)
                                  (list 126 "Georgescu" "Ioana" "321CD" 9.87))
                 
                 (list "Cursuri" (list "Anul" "Semestru" "Disciplină" "Număr credite" "Număr teme")
                                 (list "I" "I" "Programarea calculatoarelor" 5 2)
                                 (list "II" "II" "Paradigme de programare" 6 3)
                                 (list "III" "I" "Algoritmi paraleli și distribuiți" 5 3)
                                 (list "IV" "I" "Inteligență artificială" 6 3)
                                 (list "I" "II" "Structuri de date" 5 3)
                                 (list "III" "II" "Baze de date" 5 0))
                       ))

;====================================
;=         Insert                   =
;====================================

(define (get-by-columns table)
  (apply map list (cdr table)))

(define (get-pair column-name record)
   (filter (lambda(x) (equal? column-name (car x))) record))

(define (update-insert db table)
                   (map (lambda(x) (if (equal? (car x) (car table)) table x)) db))

(define insert
  (λ (db table-name record)
  (update-insert db (append (get-table db table-name) (list (let* ([table (get-table db table-name)] [columns (get-columns table)] [new-row '()])
      
    (map (lambda(col) (let ([pair (get-pair col record)])
                     (if (equal? pair '()) (append new-row 'null) (append new-row (cdar pair)))))
            columns))) ))))


;====================================
;=                                  =
;=     	Simple-select       	    =
;=                                  =
;====================================

(define simple-select
  (λ (db table-name columns)
    (if (equal? (get-entries (get-table db table-name)) '()) '()
    (map (lambda (x) (map cdr (filter (lambda(y) (equal? (car y) x)) (zip (get-table db table-name))))) columns))))

;====================================
;=                     		    =
;=           Select        	    =
;=                                  =
;====================================
(define (zip-entries table)
  (let [(new (map (lambda(x) (zip-aux (get-columns table) x)) (get-entries table)))]
  new))
;prelucreaza randurile ce vor fi returnate

(define (display-column rows column-name)
  (map (lambda (x) (cdr x)) (car (filter (lambda (x) (equal? (caar x) column-name)) (apply map list rows)))))

;cauta intr-un rand coloana care ma intereseaza
(define (search-value column-name row)
  (cdar (filter (lambda (pair) (equal? (car pair) column-name)) row)))

;verifica daca un rand respecta toate conditiile
(define (check-row row conditions)
  (andmap (lambda (cond) (let* ([f (car cond)] [column (second cond)] [valueToCompare (third cond)] [value (search-value column row)])
                           (if (equal? value 'null) #f (f value valueToCompare)))) conditions))
(define (sum list)
   (foldl + 0 list))
  
(define (avg list)
  (/ (foldl + 0 list) (length list)))

(define (count list)
  (length (remove-duplicates list)))

(define (operation op col)
    (cond
    [(equal? op 'min) (apply min col)]
    [(equal? op 'max) (apply max col)]
    [(equal? op 'avg) (avg col)]
    [(equal? op 'sum) (sum col)]
    [(equal? op 'count) (count col)]
    [(equal? op 'sort-asc) (sort col <)]
    [(equal? op 'sort-desc) (sort col >)]))

(define (display-all filtered-rows columns)
  (map (lambda (x) (if (pair? x) (let ([op (car x)] [col (display-column filtered-rows (cdr x))])
                                  (operation op col)) (display-column filtered-rows x)))  columns))

(define select
  (λ (db table-name columns conditions)
   (let ([filtered-rows (filter (lambda(x) (check-row x conditions)) (zip-entries (get-table db table-name)))])
     (if (equal? filtered-rows '()) '() (display-all filtered-rows columns)))))


;====================================
;=            Update                =
;====================================
(define (update-row row values)
  (map (lambda (r) (let ([pair (get-pair (car r) values)])
                     (if (equal? pair '()) r (car pair)))) row)
  )
(define (unzip-row row)
  (map cdr row))

(define (update-aux db table-name rows)
  (list* table-name (get-columns (get-table db table-name)) (map (lambda (r) (unzip-row r)) rows )))

(define update
  (λ (db table-name values conditions)
   (update-insert db (update-aux db table-name (map (lambda (row) (if (equal? (check-row row conditions) #t) (update-row row values) row))
                           (zip-entries (get-table db table-name)))))))

;====================================
;=           Delete                 =
;====================================
(define delete
  (λ (db table-name conditions)
    (update-insert db (update-aux db table-name
                             (filter-not (lambda (x) (check-row x conditions)) (zip-entries (get-table db table-name)))))))


