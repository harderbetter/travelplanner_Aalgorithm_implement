


(defun sample-test ()
; This is an example call to TravelPlannerAgent
   (TravelPlannerAgent "newark" "bangor" 'goal-test-TP? 'successors-TP 'get-goal-estimate-TP))

; States are represented as a string giving the name of the city which one is at
;
; Nodes in the search tree will be represented by atoms 
;     A node for a state will be represented by turning the 
;     string "Node-" concatenated with a string giving the city 
;     (reresenting the state) into an atom.  
;     This can be done via the intern function.  Thus a node for the 
;     state that is the city denver will be gotten via
;     (intern(concatenate 'string "Node-" "denver"))
;     and will appear as |Node-denver| if printed.
; Nodes have the following properties:
;  state - The string that is the name of the city that is the 
;          current state of the node.  
;          For example, the node |Node-denver| will have as its state  
;          property the string "denver". This is the node's state, 
;          since it is the current location of the agent.
;  parent - The node that is the predecessor of the node on the best 
;           path that has been found so far from start city to the  
;           city represented by the node.
;  action - The action, such as ("baltimore" fly) that was used to 
;           get to the node, meaning fly to Baltimore
;  best-path-cost - The cost of the best known path from the initial 
;                   state to the node (this is g)
;  cost-to-goal-estimate - The estimate of the cost to a goal from 
;          the state represented by this  node (this is h)
;  least-cost-estimate - The overall estimate of the cost from the
;           initial state to goal going through this node (this is f)


;
; TravelPlannerAgent takes five problem-dependant arguments:
;
;     start-city - a string giving the name of the city from which to 
;                  start the search. 
;     goal-city -  a string giving the name of the city that one 
;                  wishes to reach
;     goal-test? -  a predicate that returns true for goal nodes 
;                   and false for non-goal nodes.
;     get-successors -  a function to compute successors of a state
;                   represented by a node.  The successors are each
;                   represented as (new-state means arc-cost) triples,
;                   such as ("miami" fly 2776)  
;     get-goal-estimate - a function which takes a string giving the
;                   name of a city and returns an estimate of the cost 
;                   of getting to the goal city

; TravelPlannerAgent returns a 2-tuple whose first element is an 
; optimal path from the start city to the goal city represented as  
; a list of actions that are performed to get from the start city to 
; the goal city and whose second element is the cost of this path.
;

 (defun TravelPlannerAgent
  (start-city  goal-city goal-test? get-successors get-goal-estimate) 
;create a node for start-city, and find the path to goal-city 
;   using Algorithm A*
  (defun search-graph (open-closed)
  ; open-closed is a 2-element list whose first element is the
  ;   open list and whose second element is the closed list
  ; search-graph is the function that iterates through the open list.
  ;     It selects the front node on open list and tests whether 
  ;     its state is the goal.  If so, it gets the best path that has 
  ;     been found to this node, along with the path cost and returns 
  ;     it.  Otherwise it recursively calls search-graph with the new 
  ;     open and closed lists that result from expanding the graph 
  ;     with the successors of the selected node.
  ; returns a 2-element list, containing the sequence of actions
  ;     leading to the goal and the total cost of the path;
       (cond((null (car open-closed)) nil)
          (t (let((selected-node (caar open-closed)))
                 (terpri)
         
                 (format t 
                    "The nodes, f-values, and actions on open list are ~A" 
                     (mapcar #'(lambda (x)
                              (list x (get x 'least-cost-estimate) 
                                      (get x 'action)))
                              (car open-closed)))
                 (terpri)
                 (format t 
                     "The nodes, f-values, and actions on closed list are ~A" 
                      (mapcar #'(lambda (x)
                              (list x (get x 'least-cost-estimate) 
                                      (get x 'action)))
                              (cadr open-closed)))
                 (terpri) (terpri)
                 (format t "Select a new node from open list")
                 (terpri) 
                (format t "The selected node is ~A" 
                          (caar open-closed))
                (terpri)
                (format t "Check if this node is the goal node")
                (terpri)
                (cond((funcall goal-test? selected-node goal-city)
                          (terpri)
                          (format t "This is the goal node")
                          (terpri)
                          (format t "Here is the list of actions and total path cost in the solution")
                          (terpri)
                          (get-path-and-total-cost selected-node))
                     (t (let ((successors (funcall get-successors
                                                   selected-node)))
                        (format t "This is NOT the goal node")
                        (terpri)
                        (format t "Its successors (and their arc costs) are ~A"
                                  successors)
                        (terpri)

                        (search-graph
                           (expand-graph 
                             successors
                             selected-node
                             (list (cdr (car open-closed))
                                   (cons selected-node 
                                         (cadr open-closed)))
                             get-successors
                             get-goal-estimate 
                             goal-city)))))))))
                         
; create a node for start-city and begin the search
  (search-graph 
   (list(list (create-node start-city nil nil 0 
                           get-goal-estimate goal-city))
   nil)))
      
 (defun expand-graph
   (succs parent-node open-closed succ-fn est-goal goal-city)
   ; (break "expand-graph")
        ;; succs is the list of sucessors of parent-node
        ;; each element of succs is a tuple of the form 
        ;;    (new-state means arc-cost) triples such as 
        ;;    ("miami" fly 2776).
        ;; expand-graph adds the list of successors of parent to 
        ;;    the graph and to open list.
        ;; It must make sure that a successor has not already 
        ;;    been encountered (ie., is not already on open 
        ;;    or closed) and must check for updating the 
        ;;    shortest path if the state has been encountered 
        ;;    before
        ;; returns the resulting 2-tuple giving the open 
        ;;    and closed lists
   (cond ((null succs) open-closed)
         (t 
;         process the next successor
           (let* ((state (caar succs))
                  (node-name 
                      (intern (concatenate 'string 
                                 "Node-" state)))
                  (arccost (caddar succs))
                  (action (list (caar succs) (cadar succs)))
                  (cost (+ (get parent-node 'best-path-cost)
                            arccost)))
              (format t "     The next successor is ~A" (car succs))
              (terpri)
      (cond ((and (not (state-on state (car open-closed)))
                          (not (state-on state (cadr open-closed))))
; this successor is not on open or closed list
                       (format t "this successor is not on open or closed list")

                       (terpri)    
                       (expand-graph (cdr succs)
                                      parent-node
                                     (list (add-to-open 
                                           (create-node (caar succs) 
                                                     action
                                                     parent-node 
                                                     cost 
                                                     est-goal 
                                                     goal-city)
                                            (car open-closed))
                                         (cadr open-closed))
                                      succ-fn
                                      est-goal
                                      goal-city))
                    ((and (state-on state (car open-closed))
                          (< cost (get node-name 'best-path-cost)))
; this successor is already on open list and we have
;    found a better path to it
                     (format t "**** ON OPEN AND IT HAS A NEW BETTER PATH COST***")
                     (terpri)
                     (expand-graph (cdr succs)
                                    parent-node
                                   (update-node-open node-name
                                                      parent-node
                                                      succ-fn
                                                      cost
                                                      action
                                                      open-closed)
                                    succ-fn
                                    est-goal
                                    goal-city)
                     )
        ((and (state-on state (cadr open-closed))
                           (< cost (get node-name 'best-path-cost)))
; this successor is already on closed list and we have
;    found a better path to it
                     (format t "*** ON CLOSED AND IT HAS A NEW BETTER PATH COST***")
                     (terpri)
                     (expand-graph (cdr succs)
                                    parent-node
                                    (update-node-closed node-name
                                                        parent-node
                                                        succ-fn
                                                        cost
                                                        action
                                                        open-closed)
                                    succ-fn
                                    est-goal
                                    goal-city))
                    (t 
; this successor is already on open or closed and the new path
;   to the node is not better than the existing path
                      (format t "this successor is on open or closed but path is not better")
                      (terpri)
                      (expand-graph (cdr succs)
                                    parent-node
                                    open-closed 
                                    succ-fn
                                    est-goal
                                    goal-city)))))))

(defun update-node-open 
  (n parent successor-fn cost-of-short-path action open-closed )
  ; (break "update-node-open")
  ; open-closed is a 2-element list whose first element is the
  ;   open list and whose second element is the closed list
  ; node n is on the open list.
  ; a new shortest path from the initial state to node n has 
  ;   been found.
  ; parent is the parent node of node n on this new path.
  ; action is the action that moved from parent to node n.  

  ; cost-of-short-path is the cost of this new path from the
  ;   initial state to node n and goes through parent. 
  ; successor-fn is the parameter giving the function for
  ;   computing successors 
  ; update the properties of node n and, if necessary, its position
  ;  on open list
  ; return the adjusted open-closed list
; YOU MUST WRITE THIS FUNCTION
    (setf (get n 'parent) parent)
    (setf (get n 'action) action)
    (setf (get n 'best-path-cost) cost-of-short-path)
    (setf (get n 'least-cost-estimate)
        (+ cost-of-short-path (get n 'cost-to-goal-estimate)))
    (list (adjust-open n (car open-closed)) (cadr open-closed))
)

(defun update-node-closed (n parent successor-fn cost-of-short-path 
                           action open-closed)
  ; (break "update-node-closed")
  ; open-closed is a 2-element list whose first element is the
  ;   open list and whose second element is the closed list
  ; node n is on the closed list.
  ; a new shortest path from the initial state to node n has 
  ;   been found.
  ; parent is the parent node of node n on this new path.
  ; action is the action that moved from parent to node n.  
  ; cost-of-short-path is the cost of this new path from the
  ;   initial state to node n and goes through parent.  
  ; successor-fn is the parameter giving the function for
  ;   computing successors
  ; update the properties of node n and, if necessary, its
  ;   descendants on open and closed lists.
  ; return the adjusted open-closed list
; YOU MUST WRITE THIS FUNCTION
    (setf (get n 'parent) parent)
    (setf (get n 'action) action)
    (setf (get n 'best-path-cost) cost-of-short-path)
    (setf (get n 'least-cost-estimate)
        (+ cost-of-short-path (get n 'cost-to-goal-estimate)))
    (let ((open-lst (update-node-cost-close n (cadr open-closed) (car open-closed))))
    (list (update-node-cost-open n open-lst open-lst) (cadr open-closed))
    )
)

(defun update-node-cost-close (n lst open)
  (cond
    ((null lst) open)
    ((equal n (get (car lst) 'parent)) 
      ; update child node cost
      (update-node-cost-it n (car lst))
      ; update child nodes of this node in open list
      (update-node-cost-close n (cdr lst) (update-node-cost-open (car lst) open open))
      )
    (t (update-node-cost-close n (cdr lst) open))
  )
)

(defun update-node-cost-open (n lst open)
  (cond
    ((null lst) open)
    ((equal n (get (car lst) 'parent))
     ; update child node cost
     (update-node-cost-it n (car lst))
     ; adjust open list
     (update-node-cost-open n (cdr lst) (adjust-list (car lst) (remove-node (car lst) open)))

    )
    (t (update-node-cost-open n (cdr lst) open))
  )
)

(defun update-node-cost-it (parent node)
  (setf (get node 'best-path-cost) (+ (get parent 'best-path-cost) (get-node-arccost (successors-TP parent) node)))
  (setf (get node 'least-cost-estimate)
        (+ (get node 'best-path-cost) (get node 'cost-to-goal-estimate)))
)

(defun get-node-arccost (successors-lst node)
  (cond 
    ((null successors-lst) 0)
    ((and (equal (car (car successors-lst)) (get node 'state)) 
          (equal (cadr (car successors-lst)) (cadr (get node 'action)))) (caddr (car successors-lst)))
    (t (get-node-arccost (cdr successors-lst) node))
  )
)
               
(defun state-on (state lst)
; state is a city represented as a string such as "denver"
; lst is an open or closed list
; return true if a node on lst has this city as its state
; YOU MUST WRITE THIS FUNCTION
(cond 
  ((null lst) nil)
  ((equal  state (get (car lst) 'state)) t)
  (t (state-on state (cdr lst)))
)

)
       
(defun add-to-open (n open)
; n is a node and open is the open list
; add n to the open list in the correct position 
; return the new open list
; YOU MUST WRITE THIS FUNCTION
(adjust-list n open)
)

(defun adjust-open (n open)
; (break "entering adjust-open")
; n is a node and open is the open list
; make sure that n is in its proper position on open list, and if not
;   move it to the proper position
; the reason that n may not be in its proper position is that a better
;   path to it may have been found, thereby changing its f value
; return the new open list
; YOU MUST WRITE THIS FUNCTION
    (adjust-list n (remove-node n open))
)

(defun adjust-list (n lst)
  (cond
    ((null lst) (list n))
    ((< (get n 'least-cost-estimate) (get (car lst) 'least-cost-estimate) ) (cons n lst))
    (t (cons (car lst) (adjust-list n (cdr lst))))
  )
)

(defun remove-node (n open)
  (cond 
      ((null open) ())
      ((equal (get n 'state) (get (car open) 'state)) (cdr open))
      (t (cons (car open) (remove-node n (cdr open))))
  )
)


(defun create-node 
  (city action parent cost-of-short-path est-goal goal-city)
  ; city is a string representing the name of a city.
  ;   Create a new node with this city as its state and
  ;   with the appropriate properties
  ; action is the action that moved from parent to city.  
  ; parent is the parent node.
  ; cost-of-short-path is the cost of the path from the
  ;   initial state to the state represented by this new
  ;   node and goes through parent.
  ; goal-city is a string representing the goal city
  ; est-goal is a parameter giving the function for estimating
  ;   the cost of getting to the goal from this new node 
  ; create a new node with the appropriate properties
  ; return the created node.
(let ((node (intern (concatenate 'string 
                                 "Node-" 
                                 city
                                 ))))
  (setf (get node 'state) city)
  (setf (get node  'parent) parent)
  (setf (get node 'action) action)
  (setf (get  node 'best-path-cost) cost-of-short-path)
  (setf (get node 'cost-to-goal-estimate) (funcall est-goal city goal-city)) 
  (setf (get  node 'least-cost-estimate)
        (+ cost-of-short-path (get node 'cost-to-goal-estimate)))
  node)
)
    
(defun get-path-and-total-cost (node)
; node is a node in the graph
; return a list consisting of two elements: the path (in terms of 
;    a list of successive actions) that was taken to get to node   
;    and cost of that path
; YOU MUST WRITE THIS FUNCTION
  (list (get-path node) (get node 'best-path-cost))
)
(defun get-path (node)
  (cond
    ((null (get node 'parent)) ())
    (t (append (get-path (get node 'parent)) (list (get node 'action))))
  )
)

(defun successors-TP (cnode)
; cnode is a node 
;   return a list of the successors of cnode, with each successor 
;   given as
;   (city  means arc-cost) triples, such as ("baltimore" fly 2426)
; YOU MUST WRITE THIS FUNCTION
(cond
  (t
    (append
      (successors-TP-check-carries (get (intern (get cnode 'state)) 'fly) (get (intern (get cnode 'state)) 'distance) 'fly)
      (successors-TP-check-carries (get (intern (get cnode 'state)) 'take-bus) (get (intern (get cnode 'state)) 'distance) 'take-bus)
      (successors-TP-check-carries (get (intern (get cnode 'state)) 'take-train) (get (intern (get cnode 'state)) 'distance) 'take-train)
    )
  )
)
)


(defun successors-TP-check-carries (carries distance mean)
  ; carries ("boston" "miami")
  ; distance (("roanoke" 1254)...)
  (cond
    ((null carries) ())
    ((null (successors-TP-check-carry (car carries) distance)) ())
    (t 
      (cond
        ((equal mean 'fly) 
          (cons (list (car carries) mean (successors-TP-check-carry (car carries) distance))
             (successors-TP-check-carries (cdr carries) distance mean)))
        ((equal mean 'take-bus)
          (cond 
          ((< (successors-TP-check-carry (car carries) distance) 400)
            (cons (list (car carries) mean (successors-TP-check-carry (car carries) distance))
             (successors-TP-check-carries (cdr carries) distance mean)))
            (t (cons (list (car carries) mean (* (successors-TP-check-carry (car carries) distance) 2))
             (successors-TP-check-carries (cdr carries) distance mean)))
          ) 
        ) 
        ((equal mean 'take-train)
          (cond 
            ((< (successors-TP-check-carry (car carries) distance) 800)
            (cons (list (car carries) mean (successors-TP-check-carry (car carries) distance))
             (successors-TP-check-carries (cdr carries) distance mean)))
            (t (cons (list (car carries) mean (* (successors-TP-check-carry (car carries) distance) 1.5))
             (successors-TP-check-carries (cdr carries) distance mean)))
          )  
        )
      )
    )
  )
)

(defun successors-TP-check-carry (carry distance)
  (cond
    ((null distance) ())
    ((equal carry (caar distance)) (cadar distance))
    (t (successors-TP-check-carry carry (cdr distance)))
  )
)

(defun goal-test-TP? (node goal-city)
; node is a node and goal-city is a string giving the name of the 
;    goal city
; return True if the city for this node is goal-city
; YOU MUST WRITE THIS FUNCTION
(cond 
  ((equal (get node 'state) goal-city) t)
  (t nil)
)
)

(defun get-goal-estimate-TP (city goal-city)
  ; (break "get-goal-estimate-TP")
; city and goal-city are both strings giving city names
; return an estimate of the cost of getting from city to goal-city
; YOU MUST WRITE THIS FUNCTION
  (cond
    ((equal city goal-city) 0)
    (t (get-goal-estimate-TP-check (get (intern city) 'distance) goal-city))
  )
)
(defun get-goal-estimate-TP-check (city-lst goal-city)
  (cond 
    ((null city-lst) nil)
    ((equal (caar city-lst) goal-city) (cadar city-lst))
    (t (get-goal-estimate-TP-check (cdr city-lst) goal-city))
  )
)

bash-3.2$ lisp
;;; Liquid Common Lisp / SPARC Solaris,  Version: 5.0.6, ANSI packages
;;; Development Environment (DBCS),  Release Date: Sep 30, 1998
;;; Copyright (C) 1985-1997 Harlequin Group plc.  All rights reserved.
;;; 
;;; Common LispWorks: The Common Lisp Programming Environment
;;; Version 1.0.0,  Release Date:  1 February 1997.
;;; Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
;;; 
;;; This software product contains confidential and trade secret information
;;; belonging to Harlequin.  It may not be copied for any reason other than
;;; for archival and backup purposes.
;;;
;;; This software is subject to the following Restricted Rights legend:
;;;  "Use,  duplication or disclosure by the United States Government is
;;;   subject to restrictions as set forth in (i) FAR 52.227-14 Alt III,
;;;   (ii) FAR 52.227-19, (iii) DFAR 252.227-7013(c)(1)(ii) or (iv) the
;;;   accompanying license agreement, as applicable.  For purposes of the
;;;   FAR, this Software shall be deemed 'unpublished' and licensed with
;;;   disclosure prohibitions, rights reserved under the copyright laws of
;;;   the United States.   Harlequin Incorporated, One Cambridge Center,
;;;   Cambridge, Massachusetts  02142."
;;;
;;; Liquid Common Lisp is a trademark of The Harlequin Group plc.
;;; Other brand or product names are trademarks or registered trademarks of 
;;; their respective holders.

;;; Loading source file "/opt/lib/liquid_common_lisp_5.0/solaris/lcl/5-0/config/siteinit.lisp"
;;; Loading source file "/home/usra/f5/62914/lisp-init.lisp"
;;; Warning: File "/home/usra/f5/62914/lisp-init.lisp" does not begin with IN-PACKAGE.
;;;          Loading into package "COMMON-LISP-USER".

CL-USER 1 > (load "lisp-init.lisp")
;;; Loading source file "lisp-init.lisp"
;;; Warning: File "lisp-init.lisp" does not begin with IN-PACKAGE.
;;;          Loading into package "COMMON-LISP-USER".
;;; Warning: (DEFMACRO PP) being redefined in /home/3170a/usra/f5/62914/prog2/lisp-init.lisp (previously in /home/3170a/usra/f5/62914/lisp-init.lisp).
;;; Warning: (DEFUN EXIT) being redefined in /home/3170a/usra/f5/62914/prog2/lisp-init.lisp (previously in /home/usra/f5/62914/lisp-init.lisp).
;;; Warning: (DEFUN VIL) being redefined in /home/3170a/usra/f5/62914/prog2/lisp-init.lisp (previously in /home/usra/f5/62914/lisp-init.lisp).
;;; Warning: (DEFUN VI) being redefined in /home/3170a/usra/f5/62914/prog2/lisp-init.lisp (previously in /home/usra/f5/62914/lisp-init.lisp).
;;; Warning: (DEFUN EMACSL) being redefined in /home/3170a/usra/f5/62914/prog2/lisp-init.lisp (previously in /home/usra/f5/62914/lisp-init.lisp).
;;; Warning: (DEFUN EMACS) being redefined in /home/3170a/usra/f5/62914/prog2/lisp-init.lisp (previously in /home/usra/f5/62914/lisp-init.lisp).
;;; Warning: (DEFUN PRINT-EVAL) being redefined in /home/3170a/usra/f5/62914/prog2/lisp-init.lisp (previously in /home/usra/f5/62914/lisp-init.lisp).
;;; Warning: (DEFUN IMPLODE) being redefined in /home/3170a/usra/f5/62914/prog2/lisp-init.lisp (previously in /home/usra/f5/62914/lisp-init.lisp).
;;; Warning: (DEFUN EXPLODE) being redefined in /home/3170a/usra/f5/62914/prog2/lisp-init.lisp (previously in /home/usra/f5/62914/lisp-init.lisp).
;;; Warning: (DEFUN EXPLODES) being redefined in /home/3170a/usra/f5/62914/prog2/lisp-init.lisp (previously in /home/usra/f5/62914/lisp-init.lisp).
#P"/home/3170a/usra/f5/62914/prog2/lisp-init.lisp"

CL-USER 2 > (load "TravelPlanner-setup-16.lisp")
(load "TravelPlannerAgent-16-initial.lisp")
(load "TravelPlanner-finaltests-16.lisp");;; Loading source file "TravelPlanner-setup-16.lisp"
;;; Warning: File "TravelPlanner-setup-16.lisp" does not begin with IN-PACKAGE.
;;;          Loading into package "COMMON-LISP-USER".
#P"/home/3170a/usra/f5/62914/prog2/TravelPlanner-setup-16.lisp"

CL-USER 3 > ;;; Loading source file "TravelPlannerAgent-16-initial.lisp"
;;; Warning: File "TravelPlannerAgent-16-initial.lisp" does not begin with IN-PACKAGE.
;;;          Loading into package "COMMON-LISP-USER".
#P"/home/3170a/usra/f5/62914/prog2/TravelPlannerAgent-16-initial.lisp"

CL-USER 4 > (database-setup)

;;; Loading source file "TravelPlanner-finaltests-16.lisp"
;;; Warning: File "TravelPlanner-finaltests-16.lisp" does not begin with IN-PACKAGE.
;;;          Loading into package "COMMON-LISP-USER".
#P"/home/3170a/usra/f5/62914/prog2/TravelPlanner-finaltests-16.lisp"

CL-USER 5 > NIL

CL-USER 6 > (test1)
The next testcases call Travelers-Dilemma

(TRAVELPLANNERAGENT "newark"
                    "bangor"
                    'GOAL-TEST-TP?
                    'SUCCESSORS-TP
                    'GET-GOAL-ESTIMATE-TP) 

The nodes, f-values, and actions on open list are ((Node-newark 632 NIL))
The nodes, f-values, and actions on closed list are NIL

Select a new node from open list
The selected node is Node-newark
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((boston FLY 316))
     The next successor is (boston FLY 316)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-boston 666
                                                    (boston FLY)))
The nodes, f-values, and actions on closed list are ((Node-newark 632 NIL))

Select a new node from open list
The selected node is Node-boston
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((bangor FLY 350)
                                          (losangeles FLY 4176)
                                          (westpalm TAKE-TRAIN 2884.5))
     The next successor is (bangor FLY 350)
this successor is not on open or closed list
     The next successor is (losangeles FLY 4176)
this successor is not on open or closed list
     The next successor is (westpalm TAKE-TRAIN 2884.5)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-bangor 666
                                                    (bangor FLY))
                                                   (Node-westpalm 5453.5
                                                    (westpalm TAKE-TRAIN))
                                                   (Node-losangeles 8839
                                                    (losangeles FLY)))
The nodes, f-values, and actions on closed list are ((Node-boston 666
                                                      (boston FLY))
                                                     (Node-newark 632 NIL))

Select a new node from open list
The selected node is Node-bangor
Check if this node is the goal node

This is the goal node
Here is the list of actions and total path cost in the solution

((("boston" FLY) ("bangor" FLY)) 666) 


******************************************************

(TRAVELPLANNERAGENT "denver"
                    "westpalm"
                    'GOAL-TEST-TP?
                    'SUCCESSORS-TP
                    'GET-GOAL-ESTIMATE-TP) 

The nodes, f-values, and actions on open list are ((Node-denver 2720 NIL))
The nodes, f-values, and actions on closed list are NIL

Select a new node from open list
The selected node is Node-denver
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((baltimore FLY 2426)
                                          (stlouis FLY 1275) (miami FLY 2776)
                                          (boston TAKE-BUS 5688)
                                          (miami TAKE-BUS 5552))
     The next successor is (baltimore FLY 2426)
this successor is not on open or closed list
     The next successor is (stlouis FLY 1275)
this successor is not on open or closed list
     The next successor is (miami FLY 2776)
this successor is not on open or closed list
     The next successor is (boston TAKE-BUS 5688)
this successor is not on open or closed list
     The next successor is (miami TAKE-BUS 5552)
this successor is on open or closed but path is not better

The nodes, f-values, and actions on open list are ((Node-miami 2884
                                                    (miami FLY))
                                                   (Node-stlouis 2904
                                                    (stlouis FLY))
                                                   (Node-baltimore 3860
                                                    (baltimore FLY))
                                                   (Node-boston 7611
                                                    (boston TAKE-BUS)))
The nodes, f-values, and actions on closed list are ((Node-denver 2720 NIL))

Select a new node from open list
The selected node is Node-miami
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((stlouis FLY 1712) (boston FLY 2029)
                                          (ithaca FLY 1887)
                                          (philadelphia FLY 1645)
                                          (westpalm TAKE-BUS 108)
                                          (stlouis TAKE-TRAIN 2568.0))
     The next successor is (stlouis FLY 1712)
this successor is on open or closed but path is not better
     The next successor is (boston FLY 2029)
**** ON OPEN AND IT HAS A NEW BETTER PATH COST***
     The next successor is (ithaca FLY 1887)
this successor is not on open or closed list
     The next successor is (philadelphia FLY 1645)
this successor is not on open or closed list
     The next successor is (westpalm TAKE-BUS 108)
this successor is not on open or closed list
     The next successor is (stlouis TAKE-TRAIN 2568.0)
this successor is on open or closed but path is not better

The nodes, f-values, and actions on open list are ((Node-westpalm 2884
                                                    (westpalm TAKE-BUS))
                                                   (Node-stlouis 2904
                                                    (stlouis FLY))
                                                   (Node-baltimore 3860
                                                    (baltimore FLY))
                                                   (Node-philadelphia 5921
                                                    (philadelphia FLY))
                                                   (Node-ithaca 6442
                                                    (ithaca FLY))
                                                   (Node-boston 6728
                                                    (boston FLY)))
The nodes, f-values, and actions on closed list are ((Node-miami 2884
                                                      (miami FLY))
                                                     (Node-denver 2720 NIL))

Select a new node from open list
The selected node is Node-westpalm
Check if this node is the goal node

This is the goal node
Here is the list of actions and total path cost in the solution

((("miami" FLY) ("westpalm" TAKE-BUS)) 2884) 


******************************************************

(TRAVELPLANNERAGENT "westpalm"
                    "boston"
                    'GOAL-TEST-TP?
                    'SUCCESSORS-TP
                    'GET-GOAL-ESTIMATE-TP) 

The nodes, f-values, and actions on open list are ((Node-westpalm 1923 NIL))
The nodes, f-values, and actions on closed list are NIL

Select a new node from open list
The selected node is Node-westpalm
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((pittsburgh FLY 1500)
                                          (baltimore FLY 1434))
     The next successor is (pittsburgh FLY 1500)
this successor is not on open or closed list
     The next successor is (baltimore FLY 1434)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-baltimore 2014
                                                    (baltimore FLY))
                                                   (Node-pittsburgh 2300
                                                    (pittsburgh FLY)))
The nodes, f-values, and actions on closed list are ((Node-westpalm 1923 NIL))

Select a new node from open list
The selected node is Node-baltimore
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((miami FLY 1543)
                                          (philadelphia FLY 200))
     The next successor is (miami FLY 1543)
this successor is not on open or closed list
     The next successor is (philadelphia FLY 200)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-philadelphia 2074
                                                    (philadelphia FLY))
                                                   (Node-pittsburgh 2300
                                                    (pittsburgh FLY))
                                                   (Node-miami 5006
                                                    (miami FLY)))
The nodes, f-values, and actions on closed list are ((Node-baltimore 2014
                                                      (baltimore FLY))
                                                     (Node-westpalm 1923 NIL))

Select a new node from open list
The selected node is Node-philadelphia
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((baltimore FLY 200)
                                          (stlouis FLY 1304) (newark FLY 263)
                                          (newark TAKE-BUS 263)
                                          (baltimore TAKE-BUS 200)
                                          (ithaca TAKE-BUS 299))
     The next successor is (baltimore FLY 200)
this successor is on open or closed but path is not better
     The next successor is (stlouis FLY 1304)
this successor is not on open or closed list
     The next successor is (newark FLY 263)
this successor is not on open or closed list
     The next successor is (newark TAKE-BUS 263)
this successor is on open or closed but path is not better
     The next successor is (baltimore TAKE-BUS 200)
this successor is on open or closed but path is not better
     The next successor is (ithaca TAKE-BUS 299)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-newark 2213
                                                    (newark FLY))
                                                   (Node-pittsburgh 2300
                                                    (pittsburgh FLY))
                                                   (Node-ithaca 2379
                                                    (ithaca TAKE-BUS))
                                                   (Node-stlouis 4612
                                                    (stlouis FLY))
                                                   (Node-miami 5006
                                                    (miami FLY)))
The nodes, f-values, and actions on closed list are ((Node-philadelphia 2074
                                                      (philadelphia FLY))
                                                     (Node-baltimore 2014
                                                      (baltimore FLY))
                                                     (Node-westpalm 1923 NIL))

Select a new node from open list
The selected node is Node-newark
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((boston FLY 316))
     The next successor is (boston FLY 316)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-boston 2213
                                                    (boston FLY))
                                                   (Node-pittsburgh 2300
                                                    (pittsburgh FLY))
                                                   (Node-ithaca 2379
                                                    (ithaca TAKE-BUS))
                                                   (Node-stlouis 4612
                                                    (stlouis FLY))
                                                   (Node-miami 5006
                                                    (miami FLY)))
The nodes, f-values, and actions on closed list are ((Node-newark 2213
                                                      (newark FLY))
                                                     (Node-philadelphia 2074
                                                      (philadelphia FLY))
                                                     (Node-baltimore 2014
                                                      (baltimore FLY))
                                                     (Node-westpalm 1923 NIL))

Select a new node from open list
The selected node is Node-boston
Check if this node is the goal node

This is the goal node
Here is the list of actions and total path cost in the solution

((("baltimore" FLY) ("philadelphia" FLY) ("newark" FLY) ("boston" FLY)) 2213) 


******************************************************

(TRAVELPLANNERAGENT "memphis"
                    "westpalm"
                    'GOAL-TEST-TP?
                    'SUCCESSORS-TP
                    'GET-GOAL-ESTIMATE-TP) 

The nodes, f-values, and actions on open list are ((Node-memphis 1326 NIL))
The nodes, f-values, and actions on closed list are NIL

Select a new node from open list
The selected node is Node-memphis
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((stlouis TAKE-TRAIN 391))
     The next successor is (stlouis TAKE-TRAIN 391)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-stlouis 2020
                                                    (stlouis TAKE-TRAIN)))
The nodes, f-values, and actions on closed list are ((Node-memphis 1326 NIL))

Select a new node from open list
The selected node is Node-stlouis
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((philadelphia FLY 1304)
                                          (boston FLY 1674))
     The next successor is (philadelphia FLY 1304)
this successor is not on open or closed list
     The next successor is (boston FLY 1674)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-philadelphia 3195
                                                    (philadelphia FLY))
                                                   (Node-boston 3988
                                                    (boston FLY)))
The nodes, f-values, and actions on closed list are ((Node-stlouis 2020
                                                      (stlouis TAKE-TRAIN))
                                                     (Node-memphis 1326 NIL))

Select a new node from open list
The selected node is Node-philadelphia
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((baltimore FLY 200)
                                          (stlouis FLY 1304) (newark FLY 263)
                                          (newark TAKE-BUS 263)
                                          (baltimore TAKE-BUS 200)
                                          (ithaca TAKE-BUS 299))
     The next successor is (baltimore FLY 200)
this successor is not on open or closed list
     The next successor is (stlouis FLY 1304)
this successor is on open or closed but path is not better
     The next successor is (newark FLY 263)
this successor is not on open or closed list
     The next successor is (newark TAKE-BUS 263)
this successor is on open or closed but path is not better
     The next successor is (baltimore TAKE-BUS 200)
this successor is on open or closed but path is not better
     The next successor is (ithaca TAKE-BUS 299)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-baltimore 3329
                                                    (baltimore FLY))
                                                   (Node-newark 3558
                                                    (newark FLY))
                                                   (Node-ithaca 3773
                                                    (ithaca TAKE-BUS))
                                                   (Node-boston 3988
                                                    (boston FLY)))
The nodes, f-values, and actions on closed list are ((Node-philadelphia 3195
                                                      (philadelphia FLY))
                                                     (Node-stlouis 2020
                                                      (stlouis TAKE-TRAIN))
                                                     (Node-memphis 1326 NIL))

Select a new node from open list
The selected node is Node-baltimore
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((miami FLY 1543)
                                          (philadelphia FLY 200))
     The next successor is (miami FLY 1543)
this successor is not on open or closed list
     The next successor is (philadelphia FLY 200)
this successor is on open or closed but path is not better

The nodes, f-values, and actions on open list are ((Node-miami 3546
                                                    (miami FLY))
                                                   (Node-newark 3558
                                                    (newark FLY))
                                                   (Node-ithaca 3773
                                                    (ithaca TAKE-BUS))
                                                   (Node-boston 3988
                                                    (boston FLY)))
The nodes, f-values, and actions on closed list are ((Node-baltimore 3329
                                                      (baltimore FLY))
                                                     (Node-philadelphia 3195
                                                      (philadelphia FLY))
                                                     (Node-stlouis 2020
                                                      (stlouis TAKE-TRAIN))
                                                     (Node-memphis 1326 NIL))

Select a new node from open list
The selected node is Node-miami
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((stlouis FLY 1712) (boston FLY 2029)
                                          (ithaca FLY 1887)
                                          (philadelphia FLY 1645)
                                          (westpalm TAKE-BUS 108)
                                          (stlouis TAKE-TRAIN 2568.0))
     The next successor is (stlouis FLY 1712)
this successor is on open or closed but path is not better
     The next successor is (boston FLY 2029)
this successor is on open or closed but path is not better
     The next successor is (ithaca FLY 1887)
this successor is on open or closed but path is not better
     The next successor is (philadelphia FLY 1645)
this successor is on open or closed but path is not better
     The next successor is (westpalm TAKE-BUS 108)
this successor is not on open or closed list
     The next successor is (stlouis TAKE-TRAIN 2568.0)
this successor is on open or closed but path is not better

The nodes, f-values, and actions on open list are ((Node-westpalm 3546
                                                    (westpalm TAKE-BUS))
                                                   (Node-newark 3558
                                                    (newark FLY))
                                                   (Node-ithaca 3773
                                                    (ithaca TAKE-BUS))
                                                   (Node-boston 3988
                                                    (boston FLY)))
The nodes, f-values, and actions on closed list are ((Node-miami 3546
                                                      (miami FLY))
                                                     (Node-baltimore 3329
                                                      (baltimore FLY))
                                                     (Node-philadelphia 3195
                                                      (philadelphia FLY))
                                                     (Node-stlouis 2020
                                                      (stlouis TAKE-TRAIN))
                                                     (Node-memphis 1326 NIL))

Select a new node from open list
The selected node is Node-westpalm
Check if this node is the goal node

This is the goal node
Here is the list of actions and total path cost in the solution

((("stlouis" TAKE-TRAIN) ("philadelphia" FLY) ("baltimore" FLY) ("miami" FLY)
  ("westpalm" TAKE-BUS))
 3546) 


******************************************************

(TRAVELPLANNERAGENT "westpalm"
                    "ithaca"
                    'GOAL-TEST-TP?
                    'SUCCESSORS-TP
                    'GET-GOAL-ESTIMATE-TP) 

The nodes, f-values, and actions on open list are ((Node-westpalm 1779 NIL))
The nodes, f-values, and actions on closed list are NIL

Select a new node from open list
The selected node is Node-westpalm
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((pittsburgh FLY 1500)
                                          (baltimore FLY 1434))
     The next successor is (pittsburgh FLY 1500)
this successor is not on open or closed list
     The next successor is (baltimore FLY 1434)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-baltimore 1784
                                                    (baltimore FLY))
                                                   (Node-pittsburgh 1865
                                                    (pittsburgh FLY)))
The nodes, f-values, and actions on closed list are ((Node-westpalm 1779 NIL))

Select a new node from open list
The selected node is Node-baltimore
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((miami FLY 1543)
                                          (philadelphia FLY 200))
     The next successor is (miami FLY 1543)
this successor is not on open or closed list
     The next successor is (philadelphia FLY 200)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-pittsburgh 1865
                                                    (pittsburgh FLY))
                                                   (Node-philadelphia 1933
                                                    (philadelphia FLY))
                                                   (Node-miami 4864
                                                    (miami FLY)))
The nodes, f-values, and actions on closed list are ((Node-baltimore 1784
                                                      (baltimore FLY))
                                                     (Node-westpalm 1779 NIL))

Select a new node from open list
The selected node is Node-pittsburgh
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((philadelphia FLY 100))
     The next successor is (philadelphia FLY 100)
**** ON OPEN AND IT HAS A NEW BETTER PATH COST***

The nodes, f-values, and actions on open list are ((Node-philadelphia 1899
                                                    (philadelphia FLY))
                                                   (Node-miami 4864
                                                    (miami FLY)))
The nodes, f-values, and actions on closed list are ((Node-pittsburgh 1865
                                                      (pittsburgh FLY))
                                                     (Node-baltimore 1784
                                                      (baltimore FLY))
                                                     (Node-westpalm 1779 NIL))

Select a new node from open list
The selected node is Node-philadelphia
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((baltimore FLY 200)
                                          (stlouis FLY 1304) (newark FLY 263)
                                          (newark TAKE-BUS 263)
                                          (baltimore TAKE-BUS 200)
                                          (ithaca TAKE-BUS 299))
     The next successor is (baltimore FLY 200)
this successor is on open or closed but path is not better
     The next successor is (stlouis FLY 1304)
this successor is not on open or closed list
     The next successor is (newark FLY 263)
this successor is not on open or closed list
     The next successor is (newark TAKE-BUS 263)
this successor is on open or closed but path is not better
     The next successor is (baltimore TAKE-BUS 200)
this successor is on open or closed but path is not better
     The next successor is (ithaca TAKE-BUS 299)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-ithaca 1899
                                                    (ithaca TAKE-BUS))
                                                   (Node-newark 2133
                                                    (newark FLY))
                                                   (Node-stlouis 4143
                                                    (stlouis FLY))
                                                   (Node-miami 4864
                                                    (miami FLY)))
The nodes, f-values, and actions on closed list are ((Node-philadelphia 1899
                                                      (philadelphia FLY))
                                                     (Node-pittsburgh 1865
                                                      (pittsburgh FLY))
                                                     (Node-baltimore 1784
                                                      (baltimore FLY))
                                                     (Node-westpalm 1779 NIL))

Select a new node from open list
The selected node is Node-ithaca
Check if this node is the goal node

This is the goal node
Here is the list of actions and total path cost in the solution

((("pittsburgh" FLY) ("philadelphia" FLY) ("ithaca" TAKE-BUS)) 1899) 


******************************************************

(TRAVELPLANNERAGENT "roanoke"
                    "losangeles"
                    'GOAL-TEST-TP?
                    'SUCCESSORS-TP
                    'GET-GOAL-ESTIMATE-TP) 

The nodes, f-values, and actions on open list are ((Node-roanoke 3460 NIL))
The nodes, f-values, and actions on closed list are NIL

Select a new node from open list
The selected node is Node-roanoke
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((memphis FLY 932)
                                          (miami TAKE-BUS 2566))
     The next successor is (memphis FLY 932)
this successor is not on open or closed list
     The next successor is (miami TAKE-BUS 2566)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-memphis 3518
                                                    (memphis FLY))
                                                   (Node-miami 6324
                                                    (miami TAKE-BUS)))
The nodes, f-values, and actions on closed list are ((Node-roanoke 3460 NIL))

Select a new node from open list
The selected node is Node-memphis
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((stlouis TAKE-TRAIN 391))
     The next successor is (stlouis TAKE-TRAIN 391)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-stlouis 3874
                                                    (stlouis TAKE-TRAIN))
                                                   (Node-miami 6324
                                                    (miami TAKE-BUS)))
The nodes, f-values, and actions on closed list are ((Node-memphis 3518
                                                      (memphis FLY))
                                                     (Node-roanoke 3460 NIL))

Select a new node from open list
The selected node is Node-stlouis
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((philadelphia FLY 1304)
                                          (boston FLY 1674))
     The next successor is (philadelphia FLY 1304)
this successor is not on open or closed list
     The next successor is (boston FLY 1674)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-miami 6324
                                                    (miami TAKE-BUS))
                                                   (Node-philadelphia 6473
                                                    (philadelphia FLY))
                                                   (Node-boston 7173
                                                    (boston FLY)))
The nodes, f-values, and actions on closed list are ((Node-stlouis 3874
                                                      (stlouis TAKE-TRAIN))
                                                     (Node-memphis 3518
                                                      (memphis FLY))
                                                     (Node-roanoke 3460 NIL))

Select a new node from open list
The selected node is Node-miami
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((stlouis FLY 1712) (boston FLY 2029)
                                          (ithaca FLY 1887)
                                          (philadelphia FLY 1645)
                                          (westpalm TAKE-BUS 108)
                                          (stlouis TAKE-TRAIN 2568.0))
     The next successor is (stlouis FLY 1712)
this successor is on open or closed but path is not better
     The next successor is (boston FLY 2029)
this successor is on open or closed but path is not better
     The next successor is (ithaca FLY 1887)
this successor is not on open or closed list
     The next successor is (philadelphia FLY 1645)
this successor is on open or closed but path is not better
     The next successor is (westpalm TAKE-BUS 108)
this successor is not on open or closed list
     The next successor is (stlouis TAKE-TRAIN 2568.0)
this successor is on open or closed but path is not better

The nodes, f-values, and actions on open list are ((Node-westpalm 6405
                                                    (westpalm TAKE-BUS))
                                                   (Node-philadelphia 6473
                                                    (philadelphia FLY))
                                                   (Node-boston 7173
                                                    (boston FLY))
                                                   (Node-ithaca 8183
                                                    (ithaca FLY)))
The nodes, f-values, and actions on closed list are ((Node-miami 6324
                                                      (miami TAKE-BUS))
                                                     (Node-stlouis 3874
                                                      (stlouis TAKE-TRAIN))
                                                     (Node-memphis 3518
                                                      (memphis FLY))
                                                     (Node-roanoke 3460 NIL))

Select a new node from open list
The selected node is Node-westpalm
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((pittsburgh FLY 1500)
                                          (baltimore FLY 1434))
     The next successor is (pittsburgh FLY 1500)
this successor is not on open or closed list
     The next successor is (baltimore FLY 1434)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-philadelphia 6473
                                                    (philadelphia FLY))
                                                   (Node-boston 7173
                                                    (boston FLY))
                                                   (Node-pittsburgh 7613
                                                    (pittsburgh FLY))
                                                   (Node-baltimore 7838
                                                    (baltimore FLY))
                                                   (Node-ithaca 8183
                                                    (ithaca FLY)))
The nodes, f-values, and actions on closed list are ((Node-westpalm 6405
                                                      (westpalm TAKE-BUS))
                                                     (Node-miami 6324
                                                      (miami TAKE-BUS))
                                                     (Node-stlouis 3874
                                                      (stlouis TAKE-TRAIN))
                                                     (Node-memphis 3518
                                                      (memphis FLY))
                                                     (Node-roanoke 3460 NIL))

Select a new node from open list
The selected node is Node-philadelphia
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((baltimore FLY 200)
                                          (stlouis FLY 1304) (newark FLY 263)
                                          (newark TAKE-BUS 263)
                                          (baltimore TAKE-BUS 200)
                                          (ithaca TAKE-BUS 299))
     The next successor is (baltimore FLY 200)
**** ON OPEN AND IT HAS A NEW BETTER PATH COST***
     The next successor is (stlouis FLY 1304)
this successor is on open or closed but path is not better
     The next successor is (newark FLY 263)
this successor is not on open or closed list
     The next successor is (newark TAKE-BUS 263)
this successor is on open or closed but path is not better
     The next successor is (baltimore TAKE-BUS 200)
this successor is on open or closed but path is not better
     The next successor is (ithaca TAKE-BUS 299)
**** ON OPEN AND IT HAS A NEW BETTER PATH COST***

The nodes, f-values, and actions on open list are ((Node-baltimore 6557
                                                    (baltimore FLY))
                                                   (Node-ithaca 6656
                                                    (ithaca TAKE-BUS))
                                                   (Node-newark 6817
                                                    (newark FLY))
                                                   (Node-boston 7173
                                                    (boston FLY))
                                                   (Node-pittsburgh 7613
                                                    (pittsburgh FLY)))
The nodes, f-values, and actions on closed list are ((Node-philadelphia 6473
                                                      (philadelphia FLY))
                                                     (Node-westpalm 6405
                                                      (westpalm TAKE-BUS))
                                                     (Node-miami 6324
                                                      (miami TAKE-BUS))
                                                     (Node-stlouis 3874
                                                      (stlouis TAKE-TRAIN))
                                                     (Node-memphis 3518
                                                      (memphis FLY))
                                                     (Node-roanoke 3460 NIL))

Select a new node from open list
The selected node is Node-baltimore
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((miami FLY 1543)
                                          (philadelphia FLY 200))
     The next successor is (miami FLY 1543)
this successor is on open or closed but path is not better
     The next successor is (philadelphia FLY 200)
this successor is on open or closed but path is not better

The nodes, f-values, and actions on open list are ((Node-ithaca 6656
                                                    (ithaca TAKE-BUS))
                                                   (Node-newark 6817
                                                    (newark FLY))
                                                   (Node-boston 7173
                                                    (boston FLY))
                                                   (Node-pittsburgh 7613
                                                    (pittsburgh FLY)))
The nodes, f-values, and actions on closed list are ((Node-baltimore 6557
                                                      (baltimore FLY))
                                                     (Node-philadelphia 6473
                                                      (philadelphia FLY))
                                                     (Node-westpalm 6405
                                                      (westpalm TAKE-BUS))
                                                     (Node-miami 6324
                                                      (miami TAKE-BUS))
                                                     (Node-stlouis 3874
                                                      (stlouis TAKE-TRAIN))
                                                     (Node-memphis 3518
                                                      (memphis FLY))
                                                     (Node-roanoke 3460 NIL))

Select a new node from open list
The selected node is Node-ithaca
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are NIL

The nodes, f-values, and actions on open list are ((Node-newark 6817
                                                    (newark FLY))
                                                   (Node-boston 7173
                                                    (boston FLY))
                                                   (Node-pittsburgh 7613
                                                    (pittsburgh FLY)))
The nodes, f-values, and actions on closed list are ((Node-ithaca 6656
                                                      (ithaca TAKE-BUS))
                                                     (Node-baltimore 6557
                                                      (baltimore FLY))
                                                     (Node-philadelphia 6473
                                                      (philadelphia FLY))
                                                     (Node-westpalm 6405
                                                      (westpalm TAKE-BUS))
                                                     (Node-miami 6324
                                                      (miami TAKE-BUS))
                                                     (Node-stlouis 3874
                                                      (stlouis TAKE-TRAIN))
                                                     (Node-memphis 3518
                                                      (memphis FLY))
                                                     (Node-roanoke 3460 NIL))

Select a new node from open list
The selected node is Node-newark
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((boston FLY 316))
     The next successor is (boston FLY 316)
this successor is on open or closed but path is not better

The nodes, f-values, and actions on open list are ((Node-boston 7173
                                                    (boston FLY))
                                                   (Node-pittsburgh 7613
                                                    (pittsburgh FLY)))
The nodes, f-values, and actions on closed list are ((Node-newark 6817
                                                      (newark FLY))
                                                     (Node-ithaca 6656
                                                      (ithaca TAKE-BUS))
                                                     (Node-baltimore 6557
                                                      (baltimore FLY))
                                                     (Node-philadelphia 6473
                                                      (philadelphia FLY))
                                                     (Node-westpalm 6405
                                                      (westpalm TAKE-BUS))
                                                     (Node-miami 6324
                                                      (miami TAKE-BUS))
                                                     (Node-stlouis 3874
                                                      (stlouis TAKE-TRAIN))
                                                     (Node-memphis 3518
                                                      (memphis FLY))
                                                     (Node-roanoke 3460 NIL))

Select a new node from open list
The selected node is Node-boston
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((bangor FLY 350)
                                          (losangeles FLY 4176)
                                          (westpalm TAKE-TRAIN 2884.5))
     The next successor is (bangor FLY 350)
this successor is not on open or closed list
     The next successor is (losangeles FLY 4176)
this successor is not on open or closed list
     The next successor is (westpalm TAKE-TRAIN 2884.5)
this successor is on open or closed but path is not better

The nodes, f-values, and actions on open list are ((Node-losangeles 7173
                                                    (losangeles FLY))
                                                   (Node-pittsburgh 7613
                                                    (pittsburgh FLY))
                                                   (Node-bangor 7694
                                                    (bangor FLY)))
The nodes, f-values, and actions on closed list are ((Node-boston 7173
                                                      (boston FLY))
                                                     (Node-newark 6817
                                                      (newark FLY))
                                                     (Node-ithaca 6656
                                                      (ithaca TAKE-BUS))
                                                     (Node-baltimore 6557
                                                      (baltimore FLY))
                                                     (Node-philadelphia 6473
                                                      (philadelphia FLY))
                                                     (Node-westpalm 6405
                                                      (westpalm TAKE-BUS))
                                                     (Node-miami 6324
                                                      (miami TAKE-BUS))
                                                     (Node-stlouis 3874
                                                      (stlouis TAKE-TRAIN))
                                                     (Node-memphis 3518
                                                      (memphis FLY))
                                                     (Node-roanoke 3460 NIL))

Select a new node from open list
The selected node is Node-losangeles
Check if this node is the goal node

This is the goal node
Here is the list of actions and total path cost in the solution

((("memphis" FLY) ("stlouis" TAKE-TRAIN) ("boston" FLY) ("losangeles" FLY))
 7173) 


******************************************************

(TRAVELPLANNERAGENT "denver"
                    "ithaca"
                    'GOAL-TEST-TP?
                    'SUCCESSORS-TP
                    'GET-GOAL-ESTIMATE-TP) 

The nodes, f-values, and actions on open list are ((Node-denver 2398 NIL))
The nodes, f-values, and actions on closed list are NIL

Select a new node from open list
The selected node is Node-denver
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((baltimore FLY 2426)
                                          (stlouis FLY 1275) (miami FLY 2776)
                                          (boston TAKE-BUS 5688)
                                          (miami TAKE-BUS 5552))
     The next successor is (baltimore FLY 2426)
this successor is not on open or closed list
     The next successor is (stlouis FLY 1275)
this successor is not on open or closed list
     The next successor is (miami FLY 2776)
this successor is not on open or closed list
     The next successor is (boston TAKE-BUS 5688)
this successor is not on open or closed list
     The next successor is (miami TAKE-BUS 5552)
this successor is on open or closed but path is not better

The nodes, f-values, and actions on open list are ((Node-stlouis 2514
                                                    (stlouis FLY))
                                                   (Node-baltimore 2776
                                                    (baltimore FLY))
                                                   (Node-miami 4663
                                                    (miami FLY))
                                                   (Node-boston 6134
                                                    (boston TAKE-BUS)))
The nodes, f-values, and actions on closed list are ((Node-denver 2398 NIL))

Select a new node from open list
The selected node is Node-stlouis
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((philadelphia FLY 1304)
                                          (boston FLY 1674))
     The next successor is (philadelphia FLY 1304)
this successor is not on open or closed list
     The next successor is (boston FLY 1674)
**** ON OPEN AND IT HAS A NEW BETTER PATH COST***

The nodes, f-values, and actions on open list are ((Node-baltimore 2776
                                                    (baltimore FLY))
                                                   (Node-philadelphia 2878
                                                    (philadelphia FLY))
                                                   (Node-boston 3395
                                                    (boston FLY))
                                                   (Node-miami 4663
                                                    (miami FLY)))
The nodes, f-values, and actions on closed list are ((Node-stlouis 2514
                                                      (stlouis FLY))
                                                     (Node-denver 2398 NIL))

Select a new node from open list
The selected node is Node-baltimore
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((miami FLY 1543)
                                          (philadelphia FLY 200))
     The next successor is (miami FLY 1543)
this successor is on open or closed but path is not better
     The next successor is (philadelphia FLY 200)
this successor is on open or closed but path is not better

The nodes, f-values, and actions on open list are ((Node-philadelphia 2878
                                                    (philadelphia FLY))
                                                   (Node-boston 3395
                                                    (boston FLY))
                                                   (Node-miami 4663
                                                    (miami FLY)))
The nodes, f-values, and actions on closed list are ((Node-baltimore 2776
                                                      (baltimore FLY))
                                                     (Node-stlouis 2514
                                                      (stlouis FLY))
                                                     (Node-denver 2398 NIL))

Select a new node from open list
The selected node is Node-philadelphia
Check if this node is the goal node
This is NOT the goal node
Its successors (and their arc costs) are ((baltimore FLY 200)
                                          (stlouis FLY 1304) (newark FLY 263)
                                          (newark TAKE-BUS 263)
                                          (baltimore TAKE-BUS 200)
                                          (ithaca TAKE-BUS 299))
     The next successor is (baltimore FLY 200)
this successor is on open or closed but path is not better
     The next successor is (stlouis FLY 1304)
this successor is on open or closed but path is not better
     The next successor is (newark FLY 263)
this successor is not on open or closed list
     The next successor is (newark TAKE-BUS 263)
this successor is on open or closed but path is not better
     The next successor is (baltimore TAKE-BUS 200)
this successor is on open or closed but path is not better
     The next successor is (ithaca TAKE-BUS 299)
this successor is not on open or closed list

The nodes, f-values, and actions on open list are ((Node-ithaca 2878
                                                    (ithaca TAKE-BUS))
                                                   (Node-newark 3112
                                                    (newark FLY))
                                                   (Node-boston 3395
                                                    (boston FLY))
                                                   (Node-miami 4663
                                                    (miami FLY)))
The nodes, f-values, and actions on closed list are ((Node-philadelphia 2878
                                                      (philadelphia FLY))
                                                     (Node-baltimore 2776
                                                      (baltimore FLY))
                                                     (Node-stlouis 2514
                                                      (stlouis FLY))
                                                     (Node-denver 2398 NIL))

Select a new node from open list
The selected node is Node-ithaca
Check if this node is the goal node

This is the goal node
Here is the list of actions and total path cost in the solution

((("stlouis" FLY) ("philadelphia" FLY) ("ithaca" TAKE-BUS)) 2878) 


******************************************************
Now test update-node-closed

The starting closed list is (Node-philadelphia Node-bangor Node-westpalm
                             Node-boston Node-stlouis)
Properties of Node-philadelphia follow
best-path-cost is 1304
cost-to-goal-estimate is 299
least-cost-estimate is 1603
action is (philadelphia FLY)
parent is Node-stlouis
city is philadelphia

Properties of Node-bangor follow
best-path-cost is 2024
cost-to-goal-estimate is 673
least-cost-estimate is 2697
action is (bangor FLY)
parent is Node-boston
city is bangor

Properties of Node-westpalm follow
best-path-cost is 4558.5
cost-to-goal-estimate is 1779
least-cost-estimate is 6337.5
action is (westpalm TAKE-TRAIN)
parent is Node-boston
city is westpalm

Properties of Node-boston follow
best-path-cost is 1674
cost-to-goal-estimate is 446
least-cost-estimate is 2120
action is (boston FLY)
parent is Node-stlouis
city is boston

Properties of Node-stlouis follow
best-path-cost is 0
cost-to-goal-estimate is 1239
least-cost-estimate is 1239
action is NIL
parent is NIL
city is stlouis

The starting open list is (Node-ithaca Node-baltimore Node-pittsburgh
                           Node-losangeles)
Properties of Node-ithaca follow
best-path-cost is 2697
cost-to-goal-estimate is 0
least-cost-estimate is 2697
action is (ithaca FLY)
parent is Node-bangor
city is ithaca

Properties of Node-baltimore follow
best-path-cost is 5992.5
cost-to-goal-estimate is 350
least-cost-estimate is 6342.5
action is (baltimore FLY)
parent is Node-westpalm
city is baltimore

Properties of Node-pittsburgh follow
best-path-cost is 6058.5
cost-to-goal-estimate is 365
least-cost-estimate is 6423.5
action is (pittsburgh FLY)
parent is Node-westpalm
city is pittsburgh

Properties of Node-losangeles follow
best-path-cost is 5850
cost-to-goal-estimate is 3730
least-cost-estimate is 9580
action is (losangeles FLY)
parent is Node-boston
city is losangeles

The new node is Node-boston with a better path cost
Now call update-node-closed
The open-closed list is ((Node-ithaca Node-baltimore Node-pittsburgh
                          Node-losangeles)
                         (Node-philadelphia Node-bangor Node-westpalm
                          Node-boston Node-stlouis))

The updated closed list is (Node-philadelphia Node-bangor Node-westpalm
                            Node-boston Node-stlouis)

Properties of Node-philadelphia follow
best-path-cost is 1304
cost-to-goal-estimate is 299
least-cost-estimate is 1603
action is (philadelphia FLY)
parent is Node-stlouis
city is philadelphia

Properties of Node-bangor follow
best-path-cost is 1850
cost-to-goal-estimate is 673
least-cost-estimate is 2523
action is (bangor FLY)
parent is Node-boston
city is bangor

Properties of Node-westpalm follow
best-path-cost is 4384.5
cost-to-goal-estimate is 1779
least-cost-estimate is 6163.5
action is (westpalm TAKE-TRAIN)
parent is Node-boston
city is westpalm

Properties of Node-boston follow
best-path-cost is 1500
cost-to-goal-estimate is 446
least-cost-estimate is 1946
action is (boston TAKE-BUS)
parent is Node-philadelphia
city is boston

Properties of Node-stlouis follow
best-path-cost is 0
cost-to-goal-estimate is 1239
least-cost-estimate is 1239
action is NIL
parent is NIL
city is stlouis

The updated open list is (Node-ithaca Node-baltimore Node-pittsburgh
                          Node-losangeles)

Properties of Node-ithaca follow
best-path-cost is 2523
cost-to-goal-estimate is 0
least-cost-estimate is 2523
action is (ithaca FLY)
parent is Node-bangor
city is ithaca

Properties of Node-baltimore follow
best-path-cost is 5818.5
cost-to-goal-estimate is 350
least-cost-estimate is 6168.5
action is (baltimore FLY)
parent is Node-westpalm
city is baltimore

Properties of Node-pittsburgh follow
best-path-cost is 5884.5
cost-to-goal-estimate is 365
least-cost-estimate is 6249.5
action is (pittsburgh FLY)
parent is Node-westpalm
city is pittsburgh

Properties of Node-losangeles follow
best-path-cost is 5676
cost-to-goal-estimate is 3730
least-cost-estimate is 9406
action is (losangeles FLY)
parent is Node-boston
city is losangeles
NIL

CL-USER 7 > (quit)

