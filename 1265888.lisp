
	;Name:Kieran Boyle
	;Student Number: 1265888
	;Course: CMPUT 325
	;Lecture Section: B1
	;Lab Section: HO1
	;Assignment Number: 1



; Question 1.

;	xmember takes in a single list and another element. If the element is within the 
;	list then the value of "T" is returned. If the value is not in the list then the 
;	value "NIL" is returned. The argument may be: an atom, list, list of lists, or nil.
;
;	My function checks if the list is empty, if not it recursively checks if the element
;	is contained witin the list. 
;
;	Examples:
;
;		(xmember '1 '( 1 2 3))
;			=> T
;
;		(xmember '1 '( (1) 2 3))
;			=> NIL



(defun xmember (a L)
	(cond ((eq NIL L)
		NIL)
	((equal a (car L))
		T)
	(t (xmember a (cdr L)))
	)
)


;Question 2.
;
;	The function takes in an argument x is a list possibly containing sublists. If 
;	there is sublists the function will remove any sublists such that it will return 
;	a list of atoms keeping with the same order as they appeared in the original 
;	inputed list. As per the assignment specification NIL and () will not appear
;	within x. 
;
;	First the function checks if the list is empty, if it is not the list verifies whether
;	the elemnt is an atom. If it is a atom the value is added to the output list. Otherwise,
;	the fucntion appends the value at eh lowest depth of the sublist. 
;
;
;	Examples:
;		(flatten '(a (b c) d))
;			=>(a b c d)



(defun flatten (L)
	(if (eq nil L) nil
		(if (atom L) (list L)
			(append (flatten (car L)) (flatten (cdr L)))
			)
		)

	)

; Question 3.
;
;	This function takes in two lists of various sizes and combines their contents into a single list. Alternating elements are
;	are placed in the final list outputs.
;
;	My function works by frist determining if either list is empty. If the lists are not empty then the first element of both the
;	first and second list are taken and appended to the list until both lists are empty. Whenever a list is empty the contents of the 
;	other list are appended to the end.
;
;	Examples:
;
;		(mix '(a b c) '(d e f))
;			=>(a d b e c f)
;
;		(mix '(1 2 3) '(a))
;			=>(1 a 2 3)

(defun mix (L1 L2)
	(cond
		((null L1) 
			L2)
		((null L2) 
			L1)
		(t (append(cons (car L1) (cons (car L2) nil)) 
			(mix(cdr L1) (cdr L2))))
	)
)

;Question 4 
;
;	The funtion split takes in a single list and divides its elaments 
;	among 2 lists. The mechanism for division is choosing alternating elemnets 
;	from the original list.
;
;	For my program I run the same subfunction twice once with the originl list and the next
;	time with the cdr f that list. it gives me the alternating values for the sublists 
;	returned to the user. 
;
;	Examples:
;		(split '(1 2 3 4 5 6))
;			=>((1 3 5) (2 4 6))
;		(split '((a) (b c) (d e f) g h))
;			=>(((a) (d e f) h) ((b c) g))
		


(defun split (l)
	(list (splitr l) (splitr (cdr l)))
	)

;This is the function that I use to get the alternating elements from the list
(defun splitr (l)

		(cond
			((null l) 
				nil)
			(t ( cons (car l)  
				(splitr (cdr (cdr l)))))
			)

	)






;Question 5.
;
;	Question 5.1:
;
;		No it is not always true that (split (mix L1 L2)) returns the list (L1 L2). There are certain casses such as when
;		the two lists of are unequal lengths that will result in the in a different return list. LArgely this has to do with
;		the fact that the tow fucntions mix and split with every other elemet while appending the rest of the ohter list on the
;		end. So instances where the first list is odd (with length n) and the second list is even (with length n+1) will result in incorrectly 
;		combined sublists. If the first list is even (with length n) and the second list (with length n-1) will also produce an incorrect result.
;		If the list adds up to an equal number and the lists are different lengths then different sublists will be produced. Any example that will 
;		result in the the rest of a list being appended on the end could result in such a change. 
;
;		Please see below examples for any other cases

;
;		Examples:
;
;		 	(split (mix '(a b c) '(d e f g)))
;				=>((A B C G) (D E F))
;
;			(split (mix '(a b c h s) '(d e f)))
;				=>((A B C H) (D E F S))
;
;			(split (mix '(a b  h s) '( e f)))
;				=>((A B H) (E F S))
;	Question 5.2:
;
;		It is always true that (mix (car (split L)) (cadr (split L))) returns L. It works as follows: the list is
;;		split twice, during the first split the first sublist is made into a single list element. During the second
;		split the second sublist is taken in, using the same process the previous list. After the two splits have been performed
;		the lists are recombined with elements in alternating fashion. The reason that this works is because the split and mix
;		rely on the same mechanism in which they remove and insert elemnts in alternating order. Since they use the same original
;		nothing is being added or changes that could change the way in which the list is recombined. The list does not change
;		because you are noit introducing eny elemnts or adding any so the alternating order is preserved.
;
;		The process:
;
;			L = (a b c)
;			 (mix (car (split (a b c))) (cadr (split (a b c)))) 
;			 => (mix (car ((a c) (b))) (cadr ((a c) (b))))
;			 => (mix (a c) (b))
;			 => (a b c)
;
;		As you can see from the above example the list order does not change due to the alternating way in which the elements
;		are taken from the list and the alternating way in which they are combined. By alternating I mean one element is put in
;		each suublist by odd index number and the other numbers are put in the list by even index number. They are recombind in
;		the same order by puutin the first element of each sublist into the final list until the sublists are empty.  
;



;Question 6.
;
;	This function takes in two paramaters a list of integers and an integer value with the goal of determining if there is a subset 
;	of the listed numbers that adds up to the integer value. If there is such a subset that adds up to this number then that subset
;	is returned, NIL is returned otherwise. 
;
;	My function works by first summing the elements of the given list and then adding up the contents within the list. I use the lper function
;	"summate" which takes in a list of elemnets and adds them up or returns 0. If l is a single element then that element is returned. Once the
;	sum has been determined I do a number of checks on the list and the s value. First I check to see if the list is empty because if it is
;	then nil should be reeturned, I check if the valuie of s is equal to the first element in the list just in case there is a single mathcing 
;	element that is equal to s. I check if s is equal to or less than zero which will become clearer when I explain I explain the recursion.
;	I then check if the sum of the list is greaer than s so that I can return nil (greater than is an undesirable situation) and finally I check
;	if the summed total is equal to s, if so then I  return the current subset.
;
;	Otherwise: first I detrmine if the r the rest of the list adds up tot he subset sum fulfilling the "when the value is not in the list" case. 
;	If this case is true then you combine the rest of the elments from the first list and check if there are any other numbers that add up to s.
;	If this is not the case then you run subset sum on the rest of the list.  
;
;


(defun subsetsum (l s)
		
	(let ((total (summate l)))
		(cond 
			((null l) 
				nil)
			((= s (car l))
				(list (car l)))
			((= s 0) 
				l)
			((< s 0) 
				nil)
			((< total s) 
				nil)
			((= total s) 
				l)
			(t(cond 
				((subsetsum (cdr l) (- s (car l)))
					(cons (car l) (subsetsum (cdr l) (- s (car l)))))
				(t(subsetsum (cdr l) s ))))
			)
		)
		
	)	
	
;The function I made to recursively add numbers in a list together.
(defun summate (l)
	(cond
		((null l) 0)
		((atom l) l)
		(t (+ (car l) (summate (cdr l))))
		)
	)



;Resources

;https://www.gnu.org/software/emacs/manual/html_node/eintr/Sorting.html
;http://www.lispworks.com/documentation/HyperSpec/Body/s_let_l.htm
;http://rosettacode.org/wiki/Flatten_a_list
;http://www.afralisp.net/autolisp/tutorials/cond-vs-if.php
;http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#symbol
;http://www.lispworks.com/documentation/HyperSpec/Body/s_let_l.htm#let
;http://www.afralisp.net/autolisp/tutorials/cond-vs-if.php
