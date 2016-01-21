;;Part 2 - Expert System
;;Javier Garrido and Nati Tessema
;;CSC 180
;;03/04/2015


(deffacts mortgage
  ;;None required by our system
 )

 ;;Is the user over 18
(defrule over-18
 ?a <- (initial-fact)
 =>
 (printout t crlf)
 (printout t crlf)
 (printout t crlf)
 (printout t "           o--------------------------------------o" crlf)
 (printout t "           |                                      |" crlf)
 (printout t "           |     Mortgage Pre-Aproval System      |" crlf)
 (printout t "           |        by Nati Tessema and           |" crlf)
 (printout t "           |          Javier Garrido              |" crlf)
 (printout t "           |                                      |" crlf)
 (printout t "           o--------------------------------------o" crlf)
 (printout t crlf)
 (printout t "        Please complete a few questions about yourself: " crlf)
 (printout t crlf)
 (printout t "        |--Are you over 18? (yes/no) ")
 (assert (over18 (read)))
 (retract ?a)
 (printout t "        |" crlf))

 ;;Does user have a job?
(defrule has-job
 ?a <- (over18 yes)
 =>
 (printout t "        |--Do you have a job? (yes/no) ")
 (assert (job (read)))
 (retract ?a)
 (printout t "        |"crlf))


(defrule job-over-6-months
  ?a <- (job yes)
 =>
 (retract ?a)
 (printout t "        |--Have you been at your current job for " crlf)
 (printout t "        |  more than 6 months? (yes/no) ")
 (assert (sixMonths (read)))
 (printout t "        |"crlf))

 
;;-----------Simple Rejection---------------------------------------------
 ;;No job
(defrule no-job
 ?a <- (job no)
 =>
 (retract ?a)
 (printout t "    ----------------------------------------------------------------" crlf)
 (printout t crlf)
 (printout t "                             Sorry," crlf)
 (printout t "           You cannot get a mortgage without a job." crlf)
 (printout t crlf)
 (printout t "         Thank you for using the Morgage Pre-Aproval System" crlf)
 (printout t crlf)
 (printout t "    ----------------------------------------------------------------" crlf))


 ;;Underage
(defrule underage
 ?a <- (over18 no)
 =>
 (retract ?a)
 (printout t "    ----------------------------------------------------------------" crlf)
 (printout t crlf)
 (printout t "                             Sorry," crlf)
 (printout t "          You cannot get a mortgage if you're 18 years old." crlf)
 (printout t crlf)
 (printout t "         Thank you for using the Morgage Pre-Aproval System" crlf)
 (printout t crlf)
 (printout t "    ----------------------------------------------------------------" crlf))


 ;;Hasn't had his current job for longer than 6 months
(defrule not-6-months
 ?a <- (sixMonths no)
 =>
 (retract ?a)
 (printout t "    ----------------------------------------------------------------" crlf)
 (printout t crlf)
 (printout t "                             Sorry," crlf)
 (printout t "          You cannot get a mortgage if you haven't been at" crlf)
 (printout t "            your current job for more than 6 months." crlf)
 (printout t crlf)
 (printout t "         Thank you for using the Morgage Pre-Aproval System" crlf)
 (printout t crlf)
 (printout t "    ----------------------------------------------------------------" crlf))



 ;;Can't afford any downpayment amount
(defrule cant-afford-downpayment
 ?a <- (afford 20%downpayment no)
 ?b <- (afford 3%downpayment no)
 =>
 (retract ?a)
 (retract ?b)
 (printout t "    ----------------------------------------------------------------" crlf)
 (printout t crlf)
 (printout t "                             Sorry," crlf)
 (printout t "      You cannot get a mortgage if you can't afford the downpayment." crlf)
 (printout t crlf)
 (printout t "         Thank you for using the Morgage Pre-Aproval System" crlf)
 (printout t crlf)
 (printout t "    ----------------------------------------------------------------" crlf))


 ;;----------------------------------------------------------------------
 
 
 ;;Get desired mortgage amount
(defrule desired-mortgage
 ?a <- (sixMonths yes)
 =>
 (retract ?a)
 (printout t "        |--Enter the amount you want for the " crlf)
 (printout t "        |   mortgage: (ex 400000) ")
 (assert (mortgage (read)))
 (printout t "        |"crlf))
 
 
 ;;Get if the user can afford a 20% down-payment
(defrule afford-20%-down
 (mortgage ?amount)
 =>
 (printout t "        |--Can you afford a downpayment of " (* 0.20 ?amount) crlf)
 (printout t "        |   (20% of  your desired mortgage)? (yes/no) ")
 (assert (afford 20%downpayment (read)))
 (printout t "        |"crlf))

 
 ;;Get if the user can afford a 3% down-payment
(defrule afford-3%-down
 (mortgage ?amount)
 (afford 20%downpayment no)
 =>
 (printout t "        |--Can you afford a downpayment of " (* 0.035 ?amount) crlf)
 (printout t "        |   (3.5% of  your desired mortgage)? (yes/no) ")
 (assert (afford 3%downpayment (read)))
 (printout t "        |"crlf))
 
 
 ;;--------Get Type of Loan---------------------------
(defrule fha-loan
 (mortgage ?P)
 ?a <- (afford 20%downpayment no)
 ?b <- (afford 3%downpayment yes)
 =>
 (retract ?a)
 (retract ?b)
 (assert (downPayment (* 0.035 ?P)))
 (assert (loan fha)))

(defrule conventional-loan
  (mortgage ?P)
 ?a <- (afford 20%downpayment yes)
 =>
 (retract ?a)
 (assert (downPayment (* 0.20 ?P)))
 (assert (loan conventional)))
 ;;---------------------------------------------------
 
 ;;-------Set up mortgage - downpayment depending on type of loan-----

(defrule mortgage-minus-down-conventional
  (loan conventional)
  (downPayment ?D)
  (mortgage ?P)
  =>
  (assert (mortgageMinusDown (- ?P ?D))))


(defrule mortgage-minus-down-fha
  (loan fha)
  (downPayment ?D)
  (mortgage ?P)
  =>
  (assert (mortgageMinusDown (- ?P ?D))))
 ;;---------------------------------------------------------------------


 ;;Credit Score---------------------------------------------------------
(defrule credit-score
 (loan ?)
 =>
 (printout t "        |--What is your credit score? (620-850) ")
 (assert (crispCredit (read)))
 (printout t "        |"crlf))
 ;;--------------------
 
 ;;Get Interest Rate from Credit


 ;; FHA----------------------------------------------
 ;; Score ranges-------------------------------------
(defrule fha-interest-rate-high
 (loan fha)
 (crispCredit ?cs)
 (test (>= ?cs 760))
 =>
 (assert (interestRate 3.424)))
 
(defrule fha-interest-rate-mid-high
 (loan fha)
 (crispCredit ?cs)
 (test (and (<= ?cs 759)(>= ?cs 700)))
 =>
 (assert (interestRate 3.646)))

(defrule fha-interest-rate-mid
 (loan fha)
 (crispCredit ?cs)
 (test (and (<= ?cs 699)(>= ?cs 680)))
 =>
 (assert (interestRate 3.822)))

(defrule fha-interest-rate-mid-low
 (loan fha)
 (crispCredit ?cs)
 (test (and (<= ?cs 679)(>= ?cs 660)))
 =>
 (assert (interestRate 4.036)))
 
(defrule fha-interest-rate-low
 (loan fha)
 (crispCredit ?cs)
 (test (and (<= ?cs 659)(>= ?cs 640)))
 =>
 (assert (interestRate 4.466)))
 
(defrule fha-interest-rate-very-low
 (loan fha)
 (crispCredit ?cs)
 (test (and (<= ?cs 639)(>= ?cs 620)))
 =>
 (assert (interestRate 5.012)))
 ;;------------End of FHA CS---------------------------

 ;; Conventional---------------------------------------
(defrule conv-interest-rate-high
 (loan conventional)
 (crispCredit ?cs)
 (test (>= ?cs 760))
 =>
 (assert (interestRate 4.0)))
 
(defrule conv-interest-rate-mid-high
 (loan conventional)
 (crispCredit ?cs)
 (test (and (<= ?cs 759)(>= ?cs 700)))
 =>
 (assert (interestRate 4.25)))

(defrule conv-interest-rate-mid
 (loan conventional)
 (crispCredit ?cs)
 (test (and (<= ?cs 699)(>= ?cs 680)))
 =>
 (assert (interestRate 4.55)))

(defrule conv-interest-rate-mid-low
 (loan conventional)
 (crispCredit ?cs)
 (test (and (<= ?cs 679)(>= ?cs 660)))
 =>
 (assert (interestRate 4.825)))
 
(defrule conv-interest-rate-low
 (loan conventional)
 (crispCredit ?cs)
 (test (and (<= ?cs 659)(>= ?cs 640)))
 =>
 (assert (interestRate 5.25)))
 
(defrule conv-interest-rate-very-low
 (loan conventional)
 (crispCredit ?cs)
 (test (and (<= ?cs 639)(>= ?cs 620)))
 =>
 (assert (interestRate 5.5)))
 ;;------------End of Conventional CS------------------


 ;;------------End of Credit Score---------------------
 
 ;;------------Taxes-----------------------------------
 ;; Get gross monthly income
(defrule taxes-income
 (interestRate ?)
 (loan ?)
 =>
 (printout t "        |--What is your gross income per month? (ex 3000) ")
 (assert (income (read)))
 (printout t "        |"crlf))
 
 
(defrule taxes-debt
 (interestRate ?)
 (income ?)
 =>
 (printout t "        |--What is your debt per month?  (ex 300) ")
 (assert (debt (read)))
 (printout t "        |"crlf))


 ;; Find monthly interest rate
(defrule monthly-interest
 (interestRate ?I)
 =>
 (assert (monthlyInterest (/ ?I (* 12 100)))))


 ;; Adding 1% of mortgage to the mortgage for taxes
(defrule Mortgage-+-Taxes
 (mortgageMinusDown ?P)
 (monthlyInterest ?)
 (loan ?)
 =>
 (assert (mortgageReady (+ ?P (* 0.01 ?P)))))
 ;;---------------------End of Taxes--------------------------------

 ;;----------------------MDI----------------------------------------
 ;; Conventional loan calculation for monthly mortgage
(defrule Conventional-Mortgage-per-month
 (mortgageReady ?P)
 (monthlyInterest ?J)
 (loan conventional)
 =>
 (assert (monthMortgage (* ?P (/ ?J (- 1 (** (+ 1 ?J)(* -12 30))))))))
 

 ;; FHA loan calculation for monthly mortgage
(defrule FHA-Mortgage-per-month
  (mortgageReady ?P)
  (monthlyInterest ?J)
  (loan fha)
  =>
  (assert (monthMortgage (+ (/ (* 0.00125 ?P) 12) (* ?P (/ ?J (- 1 (** (+ 1 ?J)(* -12 30)))))))))


(defrule MDI
 (monthMortgage ?M)
 (income ?I)
 (debt ?D)
 =>
 (assert (crispMDI (* (/ (+ ?M ?D) ?I) 100))))
 ;;----------------------End of MDI----------------------------------------


 ;;---------------------Responsible---------------------------------
(defrule numberOfJobs
 (crispMDI ?)
 =>
 (printout t "        |--How many jobs have you had in the last 10 years? ")
 (assert (crispNumJobs (read)))
 (printout t "        |"crlf))
 
 
(defrule maximumTimeAtJob
 (crispNumJobs ?)
 =>
 (printout t "        |--How long did you hold your longest job in the last 10 years? ")
 (assert (crispMaxTimeJob (read)))
 (printout t "        |"crlf))
 
 
(defrule amountSaved
 (crispMaxTimeJob ?)
 =>
 (printout t "        |--How much money do you have saved up? (ex 30000) ")
 (assert (moneySaved (read)))
 (printout t crlf))
 
 
(defrule monthlySaved
 (moneySaved ?money)
 (monthMortgage ?monthMortg)
 =>
 (assert (crispCash (/ ?money ?monthMortg))))
 ;;;;---------------------End of Responsible------------------------
 
 
 
 
 ;;-----------------------FUZZY LOGIC-------------------------------
 
 ;;First Fuzzy System------------
 ;;Fuzzy sets--
(deftemplate numOfJobs
 0 15 amount
 ((G (1 1) (3 0))
  (F (1 0) (3 1) (5 0))
  (P (3 0) (5 1))))
  
  
(deftemplate maxTimeAtJob
 0 10 years
 ((P (2 1) (4 0))
  (F (2 0) (4 1) (6 0))
  (G (4 0) (6 1))))
  
  
(deftemplate savedCash
 0 100 months
 ((P (6 1) (12 0))
  (F (6 0) (12 1) (24 0))
  (G (12 0) (24 1))))
 
 
(deftemplate responsible
 0 100 percent
 ((VI (0 1) (25 0))
  (I (0 0) (25 1) (50 0))
  (N (25 0) (50 1) (75 0))
  (R (50 0) (75 1) (100 0))
  (VR (75 0) (100 1))))
 ;;End of Fuzzy Sets--
 
 
 ;;Fuzzify System 1--
(defrule fuzzifySystem1
 (crispNumJobs ?n)
 (crispMaxTimeJob ?t)
 (crispCash ?c)
 =>
 (assert (numOfJobs (?n 0) (?n 1) (?n 0)))
 (assert (maxTimeAtJob (?t 0) (?t 1) (?t 0)))
 (assert (savedCash (?c 0) (?c 1) (?c 0))))
 
 ;;FAM Rules System1------------------------------
 ;;-------------Cash Good-------------------------
 (defrule GGG
  (savedCash G)
  (maxTimeAtJob G)
  (numOfJobs G)
  =>
  (assert (responsible VR)))
  
  
 (defrule GGF
  (savedCash G)
  (maxTimeAtJob G)
  (numOfJobs F)
  =>
  (assert (responsible VR)))
  
  
 (defrule GGP
  (savedCash G)
  (maxTimeAtJob G)
  (numOfJobs P)
  =>
  (assert (responsible R)))
 
 
 (defrule GFG
  (savedCash G)
  (maxTimeAtJob F)
  (numOfJobs G)
  =>
  (assert (responsible VR)))
  
  
 (defrule GFF
  (savedCash G)
  (maxTimeAtJob F)
  (numOfJobs F)
  =>
  (assert (responsible R)))
  
  
 (defrule GFP
  (savedCash G)
  (maxTimeAtJob F)
  (numOfJobs P)
  =>
  (assert (responsible N)))
  
  
 (defrule GPG
  (savedCash G)
  (maxTimeAtJob P)
  (numOfJobs G)
  =>
  (assert (responsible N)))
  
  
 (defrule GPF
  (savedCash G)
  (maxTimeAtJob P)
  (numOfJobs F)
  =>
  (assert (responsible I)))
  
  
 (defrule GPP
  (savedCash G)
  (maxTimeAtJob P)
  (numOfJobs P)
  =>
  (assert (responsible I)))
 ;;---------End Cash Good-------------------------
 
 ;;-------------Cash Fair-------------------------
 (defrule FGG
  (savedCash F)
  (maxTimeAtJob G)
  (numOfJobs G)
  =>
  (assert (responsible VR)))
  
  
 (defrule FGF
  (savedCash F)
  (maxTimeAtJob G)
  (numOfJobs F)
  =>
  (assert (responsible R)))
  
  
 (defrule FGP
  (savedCash F)
  (maxTimeAtJob G)
  (numOfJobs P)
  =>
  (assert (responsible R)))
 
 
 (defrule FFG
  (savedCash F)
  (maxTimeAtJob F)
  (numOfJobs G)
  =>
  (assert (responsible N)))
  
  
 (defrule FFF
  (savedCash F)
  (maxTimeAtJob F)
  (numOfJobs F)
  =>
  (assert (responsible R)))
  
  
 (defrule FFP
  (savedCash F)
  (maxTimeAtJob F)
  (numOfJobs P)
  =>
  (assert (responsible N)))
  
  
 (defrule FPG
  (savedCash F)
  (maxTimeAtJob P)
  (numOfJobs G)
  =>
  (assert (responsible I)))
  
  
 (defrule FPF
  (savedCash F)
  (maxTimeAtJob P)
  (numOfJobs F)
  =>
  (assert (responsible I)))
  
  
 (defrule FPP
  (savedCash F)
  (maxTimeAtJob P)
  (numOfJobs P)
  =>
  (assert (responsible VI)))
 ;;---------End Cash Fair-------------------------
 
 ;;-------------Cash Poor-------------------------
 (defrule PGG
  (savedCash P)
  (maxTimeAtJob G)
  (numOfJobs G)
  =>
  (assert (responsible R)))
  
  
 (defrule PGF
  (savedCash P)
  (maxTimeAtJob G)
  (numOfJobs F)
  =>
  (assert (responsible R)))
  
  
 (defrule PGP
  (savedCash P)
  (maxTimeAtJob G)
  (numOfJobs P)
  =>
  (assert (responsible N)))
 
 
 (defrule PFG
  (savedCash P)
  (maxTimeAtJob F)
  (numOfJobs G)
  =>
  (assert (responsible N)))
  
  
 (defrule PFF
  (savedCash P)
  (maxTimeAtJob F)
  (numOfJobs F)
  =>
  (assert (responsible N)))
  
  
 (defrule PFP
  (savedCash P)
  (maxTimeAtJob F)
  (numOfJobs P)
  =>
  (assert (responsible I)))
  
  
 (defrule PPG
  (savedCash P)
  (maxTimeAtJob P)
  (numOfJobs G)
  =>
  (assert (responsible I)))
  
  
 (defrule PPF
  (savedCash P)
  (maxTimeAtJob P)
  (numOfJobs F)
  =>
  (assert (responsible VI)))
  
  
 (defrule PPP
  (savedCash P)
  (maxTimeAtJob P)
  (numOfJobs P)
  =>
  (assert (responsible VI)))
 ;;---------End Cash Poor-------------------------
 ;;End of FAM Rules System 1--
 
 ;;Defuzzify System 1--
(defrule defuzzify1
 (declare (salience -1))
 ?f <- (responsible ?)
 =>
 (bind ?t (moment-defuzzify ?f))
 (assert (crispResponsible ?t))
 )

 ;(printout t crlf)
 ;(printout t "------------------------------------------------" crlf)
 ;(printout t "User is responsible " ?t "%" crlf)
 ;(printout t "------------------------------------------------" crlf)
 ;(printout t crlf))

 ;;End Defuzzify System 1--
 
 ;;Second Fuzzy System------------
 ;;Fuzzy sets--
(deftemplate MDI
 0 100 number
 ((VP (45 0) (47 1))
  (P (35 0) (45 1) (47 0))
  (A (25 0) (35 1) (45 0))
  (G (15 0) (25 1) (35 0))
  (E (15 1) (25 0))))
  

(deftemplate Resp
 0 100 percent
 ((VI (0 1) (25 0))
  (I (0 0) (25 1))
  (N (25 0) (50 1) (75 0))
  (R  (50 0) (75 1) (100 0))
  (VR  (75 0) (100 1))))
   


(deftemplate Possibility
 0 120 percent
 ((N (40 1) (50 0))
  (M (40 0) (50 1) (60 0))
  (Y (50 0) (60 1) (75 0))
  (DY (60 0) (75 1) (87 0))
  (HY (75 0) (87 1) (100 0))
  (I (87 0) (100 1) )))
 ;;End of Fuzzy sets--
 

 ;;Fuzzify System 2--
(defrule fuzzifySystem2
  (crispMDI ?i)
  (crispResponsible ?c)
  =>
  (assert (MDI (?i 0) (?i 1) (?i 0)))
  (assert (Resp (?c 0) (?c 1) (?c 0))))
 
 ;;FAM Rules System 2--
 (defrule EVR
  (MDI E)
  (Resp VR)
  =>
  (assert (Possibility I)))

 (defrule ER
  (MDI E)
  (Resp R)
  =>
  (assert (Possibility I)))
  
 (defrule EN
  (MDI E)
  (Resp N)
  =>
  (assert (Possibility I)))
  
 (defrule EI
  (MDI E)
  (Resp I)
  =>
  (assert (Possibility HY)))

 (defrule EVI
  (MDI E)
  (Resp VI)
  =>
  (assert (Possibility HY)))
  
 (defrule GVR
  (MDI G)
  (Resp VR)
  =>
  (assert (Possibility I)))

 (defrule GR
  (MDI G)
  (Resp R)
  =>
  (assert (Possibility DY)))
  
 (defrule GN
  (MDI G)
  (Resp N)
  =>
  (assert (Possibility DY)))
  
 (defrule GI
  (MDI G)
  (Resp I)
  =>
  (assert (Possibility Y)))

 (defrule GVI
  (MDI G)
  (Resp VI)
  =>
  (assert (Possibility Y)))
  
 (defrule AVR
  (MDI A)
  (Resp VR)
  =>
  (assert (Possibility HY)))

 (defrule AR
  (MDI A)
  (Resp R)
  =>
  (assert (Possibility DY)))
  
 (defrule AN
  (MDI A)
  (Resp N)
  =>
  (assert (Possibility Y)))
  
 (defrule AI
  (MDI A)
  (Resp I)
  =>
  (assert (Possibility Y)))

 (defrule AVI
  (MDI A)
  (Resp VI)
  =>
  (assert (Possibility Y)))

 (defrule PVR
  (MDI P)
  (Resp VR)
  =>
  (assert (Possibility M)))
  
 (defrule PR
  (MDI P)
  (Resp R)
  =>
  (assert (Possibility M)))
  
 (defrule PN
  (MDI P)
  (Resp N)
  =>
  (assert (Possibility M)))
  
 (defrule PI
  (MDI P)
  (Resp I)
  =>
  (assert (Possibility M)))

 (defrule PVI
  (MDI P)
  (Resp VI)
  =>
  (assert (Possibility N)))

 (defrule VPVR
  (MDI VP)
  (Resp VR)
  =>
  (assert (Possibility N)))
  
 (defrule VPR
  (MDI VP)
  (Resp R)
  =>
  (assert (Possibility N)))
  
 (defrule VPN
  (MDI VP)
  (Resp N)
  =>
  (assert (Possibility N)))
  
 (defrule VPI
  (MDI VP)
  (Resp I)
  =>
  (assert (Possibility N)))

 (defrule VPVI
  (MDI VP)
  (Resp VI)
  =>
  (assert (Possibility N)))

 ;;End of FAM Rules System 2--

 ;;Defuzzify System 2--
 (defrule deffuzzify2
  (declare (salience -1000))
 ?f <- (Possibility ?)
 =>
 (bind ?t (moment-defuzzify ?f))
 (assert (probability ?t)))
 ;;End of Defuzzify System 2--

 ;;Output to the user
 (defrule display
  (probability ?final)
  (crispResponsible ?responsible)
  (loan ?type)
  =>
 (printout t crlf)
 (printout t "        ------------------------------------------------------------" crlf)
 (printout t crlf)
 (printout t "              The loan type you qualify for is: " ?type "." crlf)
 (printout t crlf)
 (printout t "              You are considered " ?responsible " % responsible " crlf)
 (printout t crlf)
 (printout t "              There is a " ?final " % probability that " crlf)
 (printout t "                       you get the mortgage." crlf)
 (printout t crlf)
 (printout t "             Thank you for using the Mortgage Pre-Approval System" crlf)
 (printout t crlf)
 (printout t "        ------------------------------------------------------------" crlf)
 (printout t crlf)
 (printout t crlf)
 (printout t crlf))
 
