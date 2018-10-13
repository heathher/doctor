#lang scheme/base

(define (visit-doctor stopword count)
	(if (= count 0)
            (print '(appointment is over))                    
            (begin 
              (let ((patient-name (ask-patient-name)))
                (if (equal? patient-name stopword)                 
                    (print '(go home))
                    (begin 
                      (printf "Hello, ~a!\n" patient-name)
                      (println '(what seems to be the trouble?))
                      (doctor-driver-loop patient-name '())
                      (visit-doctor stopword (- count 1))))))
	))

(define (ask-patient-name)
  	(begin
          (println '(next!))
          (println '(who are you?))
          (print '**)
          (car (read))))

(define (doctor-driver-loop name answers-list)
	(newline)
	(print '**) 
	(let ((user-response (read)))
	  	(cond ((equal? user-response '(goodbye)) (printf "Goodbye, ~a!\n" name))
                      (else (print (reply2 user-response answers-list))
                            (doctor-driver-loop name (check-list user-response answers-list))))
	))

(define (reply user-response answers-list)
	(case (random 6)
		((0) (qualifier-answer user-response))
		((1) (hedge))
		((2) (if (null? answers-list) (hedge) (history-answer answers-list)))
		(else (if (find-in-keywords user-response) (answer-for-keywords user-response) (hedge)))
        ))

(define answer-with-weights  (list
                               (list (lambda (x y) #t) 2 (lambda (x y)(hedge)))
                               (list (lambda (x y) #t) 1 (lambda (x y)(qualifier-answer x)))
                               (list (lambda (x y) (not(null? y))) 1 (lambda (x y)(history-answer y)))
                               (list (lambda (x y) (find-in-keywords x)) 4 (lambda (x y)(answer-for-keywords x)))
                               ))

; Упражнение №7
(define (reply2 user-response answers-list)
  (define (choose-strategy rand-number strategy-list)       
    (define (loop counter strat-list)  ; функция, которая выбирает стратегию
      (let* ((cur_proc (if (not (null? (cdr strat-list))) (car strat-list) strat-list)) ; рассматриваемая стратегия  ; решить проблему со списком
            (cur_counter (+ counter (car cur_proc))))                             ; текущий счетчик
      (begin 
        ;(println cur_counter)
        ;(println rand-number)
        ;(println strat-list)
        (if (>= cur_counter rand-number) ((cadr cur_proc) user-response answers-list)
            (loop cur_counter (cdr strat-list))))))
    (loop 0 strategy-list))

  (let* ((strat-list (foldl                ; создаем список из возможных стратегий, для которых функция-предикат #t
                     (lambda (x y) (if ((car x) user-response answers-list) (cons (cdr x) y) y))
                     '() answer-with-weights))
        (sum-of-weights (foldl (lambda (x y) (+ y (car x))) 0 strat-list)) ; суммируем веса возможных стратегий из полученного списка 
        (random-number (+ 1 (random sum-of-weights))))                     ; генерируем рандомное число от 1 до sum-of-weights (включительно)
    (begin
      ;(println random-number)
      ;(println sum-of-weights)
    (choose-strategy random-number strat-list)
    )
  ))

(define (fifty-fifty)
  (= (random 2) 0)
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
	(append (pick-random '((you seem to think that)
                               (you feel that)
                               (why do you believe that)
                               (why do you say that)
                               (you think that)          ; моя подстановка №1 
                               (why do you think that)   ; моя подстановка №2
                               (why do you feel that))   ; моя подстановка №3			  
			)
		(change-person user-response)))

(define (pick-random lst)
  	(list-ref lst (random (length lst))))
		
(define (change-person phrase)
	(many-replace '((am are)
                        (are am)
                        (i you)
                        (me you)
                        (mine yours)
                        (my your)
                        (myself yourself)
                        (you i)
                        (your my)
                        (yours mine)
                        (yourself myself))
                      phrase))
  
(define (many-replace replacement-pairs lst)
	(map (lambda (x) (let ((pat-rep (assoc x replacement-pairs)))
                           (if pat-rep (cadr pat-rep) x)))
             lst))

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
	(pick-random '((please go on)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       (please continue)
                       (you are not alone) ; моя подстановка №1
                       (tell me more)   ; моя подстановка №2
                       (think about it))   ; моя подстановка №3				   
	))

; Упражнение №4: 3й способ ответной реплики
(define (history-answer answers-list)
	(append '(earlier you said that) (pick-random answers-list)))

(define (check-list elem lst)
	(if (member elem lst) 
  		lst
	  	(cons elem lst)))

; Упражнение №6
(define keys '( 
  	((depressed suicide exams university)
		(
	  		(when you feel depressed, go out for ice cream)
	  		(depression is a disease that can be treated)
	  		(suicide isn't a good idea)
	  		(you will fell yourself better with time)
		)
	)
  	((mother father parents brother sister uncle ant grandma grandpa)
		(
	  		(tell me more about your * , i want to know all about your *)
	  		(why do you feel that way about your * ?)
	  		(your * loves you)
	  		(let's talk about your *)
		)
  	)
  	((university scheme lections)
		(
	  		(your education is important)
	  		(how many time do you spend to learning ?)
	  		(what do you think about your university?)
	  		(do you like lections?)
		)
  	)
  	((love girlfriend boyfriend)
  		(
  			(do you think that it's true love?)
  			(let's talk about your * , i think it's important for you)
  			(do you fell yourself better with your *?)
  			(let's talk about it)
  		)
  	)
  	((work job employment)
  		(
  			(do you like your work?)
  			(you should do what you like)
  			(it seems you work very hard)
  			(how long do you work?)
  		)
  	)
))

(define unique-keywords ; список ключевых слов из структуры keys
  (foldl
   (lambda (x y) (foldl
                  (lambda (key result) (if (member key result) result (cons key result)))
                  y (car x)))
   '() keys))

(define (find-in-keywords phrase) ; проверка: есть ли во фразе пациента ключевые слова
  (ormap (lambda (x) (if (member x unique-keywords) #t #f)) phrase))


(define (answer-for-keywords phrase)
  (define (make-answers-list)
    (define (make-word-answers-list word keys-list) ; если word - ключевое слово, то получаем для него список возможных ответов
      (foldl (lambda (x y) (if (member word (car x)) (append y (cadr x)) y)) '() keys-list))
    (foldl (lambda (x y)
             (let ((result (make-word-answers-list x keys)))
               (cond ((not (null? result)) (append y (list (cons x result))))) ; создаем список из пар:
               ))                                                              ; ключ - список возможных ответов для ключа
           '() phrase))
  (let* ((answers-list (make-answers-list))                     ; получаем вышеупомянутый список пар
        (pick-ans-list (pick-random answers-list))              ; выбираем ключевое слово
        (pick-ans (pick-random (cdr pick-ans-list)))            ; выбираем ответ по ключевому слову
        (key (car pick-ans-list)))                              ; ключевое слово
    (map (lambda (x) (if (equal? x '*) key x)) pick-ans))       ; если в выбранном выражении есть *, то заменяем на ключ
)

