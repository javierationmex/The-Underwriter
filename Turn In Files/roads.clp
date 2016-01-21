;;Part 1 - Roads Problem
;;Javier Garrido and Nati Tessema
;;CSC 180
;;03/04/2015



(deffacts initial-state
 (from Chico to Davis)
 (from Sacramento to Davis)
 (from Fairfield to Galt)
 (from Galt to Turlock)
 (from Galt to Sacramento)
 (from Stockton to Fairfield)
 (from Tahoe to Fairfield)
 (from Chico to Galt)
 (from Davis to Modesto)
 (from Davis to San-Francisco)
 (from San-Francisco to San-Jose)
 (from Modesto to Atwater)
 (travel Galt Davis))


 ;;Get the first traveling fact set up
(defrule initial-setup
 (initial-fact)
 (travel ?start ?finish)
 (from ?any to ?finish)
 =>
 (assert (steps 1))
 (assert (traveling ?any ?finish)))


 ;;Work through paths from destination
 ;; to origin
(defrule cycle-through-paths
 (traveling ?any ?finish)
 (from ?prev to ?any)
 =>
 (assert (traveling ?prev ?any)))


 ;;Found our origin from our destination
(defrule reached-origin
 (travel ?origin ?dest)
 ?a <- (traveling ?origin ?next)
 =>
 (assert (first ?origin ?next))
 (printout t crlf)
 (printout t "--------------------------------------------" crlf)
 (printout t "       WELCOME TO ROAD-MAPPING-CLIPS      " crlf)
 (printout t crlf)
 (printout t "   Route from " ?origin " to " ?dest " is: "crlf)
 (printout t crlf)
 (retract ?a))


 ;;Cycle back from origin to destination
 ;; using our traveling facts to avoid
 ;; branching. Print as we go since we
 ;; can only take the correct path.
(defrule route-printing
 ?a <- (first ?origin ?next)
 ?b <- (traveling ?next ?nextnext)
 ?c <- (steps ?num)
 =>
 (assert (first ?next ?nextnext))
 (printout t "   " ?num ") Drive from " ?origin " to " ?next "." crlf)
 (assert (steps (+ ?num 1)))
 (retract ?a)
 (retract ?b)
 (retract ?c))


 ;;Print the last step and remove some
 ;; leftover facts
(defrule print-last-step
 ?a <- (first ?laststep ?finish)
 ?b <- (steps ?num)
 =>
 (assert (done))
 (printout t "   " ?num ") Drive from " ?laststep " to " ?finish "." crlf)
 (printout t crlf)
 (printout t "   You've arrived!!!" crlf)
 (printout t "--------------------------------------------" crlf)
 (printout t crlf)
 (retract ?a)
 (retract ?b))
 
 ;;-----------------------------------------------------
 ;; Not essential rules used to
 ;;  clean up left over useless facts
 
(defrule clean-up-1
 (done)
 ?a <- (traveling ? ?)
 =>
 (retract ?a))

 
(defrule clean-up-2
 ?a <- (initial-fact)
 ?b <- (done)
 =>
 (retract ?a)
 (retract ?b))
 ;;-----------------------------------------------------