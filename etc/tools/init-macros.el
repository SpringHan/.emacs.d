(fset 'replace-next-placeholder
   (kmacro-lambda-form [?, ?f ?E ?\( ?s ?e ?a ?r ?c ?h ?- ?f ?o ?r ?w ?a ?r ?d ?  ?\" ?< ?+ ?+ ?+ ?> return return ?  ?c] 0 "%d"))
(fset 'insert-placeholder
   (kmacro-lambda-form [?h ?< ?+ ?+ ?+ ?> C-tab] 0 "%d"))
(fset 'format-current-indentation
   (kmacro-lambda-form [tab ?j ?d ?h return C-tab ?b] 0 "%d"))

(provide 'init-macros)
