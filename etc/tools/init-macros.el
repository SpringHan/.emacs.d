(fset 'replace-next-placeholder
      (kmacro-lambda-form [?, ?f ?E ?\( ?s ?e ?a ?r ?c ?h ?- ?f ?o ?r ?w ?a ?r ?d ?  ?\" ?< ?+ ?+ ?+ ?> return return ?  ?c] 0 "%d"))
(fset 'insert-placeholder
      (kmacro-lambda-form [?h ?< ?+ ?+ ?+ ?> C-tab] 0 "%d"))
(fset 'format-current-indentation
      (kmacro-lambda-form [tab ?j ?d ?h return C-tab ?b] 0 "%d"))
(fset 'init-solution
      (kmacro "<tab> o C-o s t r u c t SPC S o l u t i o n ( C-e ; C-<tab> b <tab> o # [ a l l o w ( u n u s e d C-<tab> b y y e e e p p"))

(provide 'init-macros)
