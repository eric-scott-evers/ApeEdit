

 (net_kernel:start ( list ( erlang:list_to_atom "lfe@127.0.0.1" ) 'longnames ) )         
;;  #(error #(already_started #Pid<0.120.0>))

 (erlang:set_cookie 'cookie)                                                             
;;  true
 
 (defun send (out)  
   (progn
     ( timer:sleep 100 )
     (! (tuple 'event_queue
          ( erlang:list_to_atom "bookA@127.0.0.1" )  
        )
        (list (tuple 'store_line (+ 5 (random:uniform 25)) out) )
     )
     out
   )
 )

  (defun send_at (out at_loc)  
   (progn
     ( timer:sleep 100 )
     (! (tuple 'event_queue
          ( erlang:list_to_atom "bookA@127.0.0.1" )  
        )
        (list (tuple 'store_line at_loc out) )
     )
     out
   )
 )


;; left data spread out to nodes
 ( timer:sleep 1000)
 
  (set data (registered))
  (set sorted_data (lists:sort data))
  (set i 1)
  (set a_list (lists:map (lambda (a) (send (atom_to_list a))) sorted_data ))
  ; c = (a b)
  (set acc '(1 ""))
  (set raw_data '("one" "two"))
  (set newline 10)
  ;(set b_list (lists:foldr  
  ;              (lambda (data c)  (list (+ (car c) 1) (++ '(10 32) (++ data (car (cdr c)))  )  )  )
  ;              acc  
  ;              a_list ) )
  (set len (length a_list))
  (set pairs (lists:zip a_list (lists:seq 1 len)))
  (lists:map (lambda (a) (send_at (car (erlang:tuple_to_list a)) (car (cdr (erlang:tuple_to_list a))) )) pairs )

  ;  last arg was raw_data 

  (set out (car a_list))
  ; (set glued (lists:foldr (lambda (add x) (++ add x)) "" a_list))
  ; (send glued)     

;;  #(show 42)
;;
;; fold with a counter 
;;
;; lists:foldr( fun(Data, [A, B]) -> [A+1, Data++B ] end, [1,""],  ["one", "two"] ).      
;;    [3,"onetwo"]
 
; (io:format " ~w " a_list  ) 




