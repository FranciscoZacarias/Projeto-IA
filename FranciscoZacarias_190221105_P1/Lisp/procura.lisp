;;; 1. Projeto 
;;; Disciplina de IA - 2020 / 2021
;;; Autor: Francisco Zacarias n.190221105

;;; procura.lisp Deve conter a implementacao do BFS/DFS/A* e (opcional) SMA*/IDA*/RBFS

(defun abertos-bfs (abertos sucessores)
"Devolve a juncao da lista abertos e sucessores, conforme o algoritmo de pesuqisa em largura"
	(append abertos sucessores)
)

(defun abertos-dfs (abertos sucessores)
"Devolve a juncao da lista abertos e sucessores, conforme o algoritmo de pesuqisa em profundidade"
	(append sucessores abertos)
)

(defun abertos-a-estrela (abertos sucessores)
"Insere, de forma ordenada, os sucessores em abertos.
Devolve a lista resultante."
	(cond
		((null sucessores) abertos)
		(t (abertos-a-estrela (insere-ordenado (car sucessores) abertos 'no-custo) (cdr sucessores)))
	)
)

(defun penetrancia (profundidade gerados)
"Calcula a penetrancia de um grafo"
	(/ (float profundidade) (float gerados))
)

; (B / B - 1) (B^L - 1) - T = 0
(defun fator-ramificacao-media-funcao (profundidade nos-gerados ponto-medio)
"Metodo da bisseccao"
	(-
		(float (* (/ ponto-medio (- ponto-medio 1)) (- (expt ponto-medio profundidade) 1)))
		nos-gerados
	)
)

(defun fator-ramificacao-media (nos-gerados profundidade-maxima &optional (a 0) (b 50))
"Calcula a ramificacao de um grafo"
	(let* (
		(ponto-medio (/ (+ a b) 2))
		(erro-atual (/ (- b a) 2))
		(fator-ramificacao (funcall 'fator-ramificacao-media-funcao profundidade-maxima nos-gerados ponto-medio))
	)
		(cond
			; Quanto a margem erro atual for menor que a margem definida 0.01, devolve o zero
			((< erro-atual 0.01) (float ponto-medio))
			; Se estiver a esquerda de zero (positivo), calcula-se o zero com a e ponto-medio
			((> fator-ramificacao 0) (fator-ramificacao-media nos-gerados profundidade-maxima a ponto-medio))
			; Se estiver a direita do zero (negativo), calcula-se o zero com ponto-medio e b
			(t (fator-ramificacao-media nos-gerados profundidade-maxima ponto-medio b))
		)
	)
)

(defun procura-nao-informada (algoritmo solucao sucessores operador profundidade abertos fechados)
"Executa o algoritmo de procura nao informada bfs ou dfs (passado por parametro)"
	(let 	((abertos-algoritmo
				(ecase algoritmo 
					(bfs 'abertos-bfs) 
					(dfs 'abertos-dfs)
				))
			(elemento (car abertos)))
		(cond
			((null abertos) nil)
			((no-existep elemento fechados) (procura-nao-informada algoritmo solucao sucessores operador profundidade (cdr abertos) fechados))
			(t
				(let* (
						(nos-sucessores (funcall sucessores elemento operador algoritmo profundidade))
					 	(solucoes (remove nil (mapcar #'(lambda (sucessor)
								(if (equal t (funcall solucao (tabuleiro sucessor))) sucessor nil) 
							)nos-sucessores)))
					)
					(if (null solucoes)
						(procura-nao-informada algoritmo solucao sucessores operador profundidade
									(funcall abertos-algoritmo (cdr abertos) nos-sucessores) (cons elemento fechados))
						(list 
							(car solucoes) 
							(+ (length fechados) 1) ; expandidos, fechados + 1 porque o no elemento nao foi adicionado
							(+ (+ (- (length abertos) 1) (length nos-sucessores)) (+ (length fechados) 1)) ; gerados, abertos + sucessores porque  os sucessores gerados nesta iteracao nao foram contados
						)
					)
				)
			)
		)
	)
)

(defun a-estrela (algoritmo solucao sucessores operador profundidade heuristica abertos fechados)
"Executa o algoritmo de procura informada a-estrela"
	(let ((elemento (car abertos)))
		(cond
			((null abertos) nil)
			((no-existep elemento fechados) (a-estrela algoritmo solucao sucessores operador profundidade heuristica (cdr abertos) fechados))
			(t
				(let* (
						(nos-sucessores (funcall sucessores elemento operador algoritmo profundidade heuristica))
					 	(solucoes (remove nil (mapcar #'(lambda (sucessor)
								(if (equal t (funcall solucao (tabuleiro sucessor))) sucessor nil) 
							)nos-sucessores)))
					)
					(if (null solucoes)
						(a-estrela algoritmo solucao sucessores operador profundidade heuristica 
									(abertos-a-estrela (cdr abertos) nos-sucessores) (cons elemento fechados))
						(list 
							(car solucoes) 
							(+ (length fechados) 1) ; expandidos, fechados + 1 porque o no elemento desta iteracao nao foi adicionado mas foi expandido
							(+ (+ (- (length abertos) 1) (length nos-sucessores)) (+ (length fechados) 1)) ; gerados, abertos + sucessores porque  os sucessores gerados nesta iteracao nao foram contados
						)
					)
				)
			)
		)
	)
)

(defun procura-espaco-estados (algoritmo estado &optional (profundidade 99999) (heuristica 'base))
"Executa uma funcao de procura em espaco de estados para o estado do problema passado por argumento."
	(let* (
			(profundidade-maxima (if (equal profundidade -1) 99999 profundidade))
			(heuristica-funcao
				(cond 
					((equal heuristica 'nova) 'heuristica-nova)
					(t 'heuristica-base)))
			(tempo-inicial (get-internal-run-time))
			(resultado (if (or (equal algoritmo 'bfs) (equal algoritmo 'dfs))
							(funcall 'procura-nao-informada algoritmo 'tabuleiro-solucao 'sucessores 'operador profundidade-maxima (list estado) '())
							(funcall 'a-estrela algoritmo 'tabuleiro-solucao 'sucessores 'operador profundidade-maxima heuristica-funcao (list estado) '())
						))
			(tempo-total (- (get-internal-run-time) tempo-inicial))
			(expandidos (second resultado))
			(gerados (third resultado))
		)
		(if (null resultado) nil
			(list 
				(first resultado)  ;no solucao
				expandidos ;nos expandidos
				gerados  ;nos gerados
				(funcall 'penetrancia (no-profundidade (first resultado)) gerados) ; penetrancia
				(funcall 'fator-ramificacao-media gerados (no-profundidade (first resultado))) ; fator de ramificacao
				tempo-total ;tempo de execução do algoritmo de procura
			)
		)
	)
)
