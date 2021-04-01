;;; Projeto 2
;;; Disciplina de IA - 2020 / 2021
;;; Autor: Francisco Zacarias nº190221105

(defvar *plus-infinity* most-positive-fixnum)
(defvar *minus-infinity* most-negative-fixnum)

; Closure com as estatisticas do algoritmo
(let (
        (no-solucao nil) ; Estrutura de dados tipo (utilidade nó)
        (nos-analisados 0)
        (corte-max 0)
        (corte-min 0)
    )
    
    ; Restaura os valores iniciais
    (defun restaura-valores ()
    "Restaura os valores deste ambiente léxico"
        (funcall 'adiciona-solucao nil)
        (setf nos-analisados 0)
        (setf corte-max 0)
        (setf corte-min 0)
    )

    (defun algoritmo-resultado ()
    "Retorna o resultado e estatisticas do algoritmo"
        (list
            nos-analisados
            corte-max
            corte-min
            (funcall 'no-solucao)
        )
    )

    ; Seletores do nó solução
    (defun adiciona-solucao (no)
    "Define o estado da solucao final"
        (setf no-solucao no)
    )

    (defun no-solucao ()
    "Devolve o nó solução"
        no-solucao
    )

    ; Nós analisados
    (defun adicionar-analisado ()
    "Adiciona um nó analisado"
        (incf nos-analisados)
    )

    (defun nos-analisados ()
    "Retorna quantos nós o algoritmo analisou"
        nos-analisados
    )

    ; Cortes
    (defun adiciona-corte (maximizing-player)
    "Adiciona um corte ao respetivo jogador"
        (ecase maximizing-player
            (1 (incf corte-max))
            (-1 (incf corte-min))
        )
    )
)
 
(defun ordena-nos-helper (no avaliacao lista-ordenada)
    (if (null lista-ordenada) 
        (list no)
        (cond
            ((<= (funcall avaliacao no) (funcall avaliacao (car lista-ordenada))) (cons no lista-ordenada))
            (t (cons (car lista-ordenada) (ordena-nos-helper no avaliacao (cdr lista-ordenada))))
        )
    )
)

(defun ordena-nos (nos avaliacao &optional final-list)
"Ordena a lista de 'nos' com base na funcao de 'avaliacao'"
    (cond
        ((null nos) final-list)
        (t (ordena-nos (cdr nos) avaliacao (funcall 'ordena-nos-helper (car nos) avaliacao final-list)))
    )
)

(defun negamax (estado profundidade-maxima avaliacao tempo-limite)
"Executa o algoritmo negamax"
    (funcall 'restaura-valores)
    (let*(
            (tempo-inicial (get-internal-run-time))
            (resultado (funcall 'negamax-algorithm estado profundidade-maxima avaliacao 'sucessores 0))
            (tempo-total (- (get-internal-run-time) tempo-inicial))
        )
        
        ; Se excedeu o limite de tempo, não retorna nada
        (if (> tempo-total tempo-limite)
            (append (list -1 -1 -1 -1 -1 estado))
            (append  
                (list tempo-total)
                (list resultado)
                (funcall 'algoritmo-resultado)
            )
        )
    )
)

; maximizing-player é 1 ou -1, respetivamente para o max e o min
(defun negamax-algorithm (estado profundidade-maxima avaliacao gera-sucessores profundidade-atual &optional (alpha *minus-infinity*) (beta *plus-infinity*) (maximizing-player 1))
    ; Incrementa um no nos analisados
    (funcall 'adicionar-analisado)
    
    (if
        ; Condições de paragem
        (or 
            (equal profundidade-maxima 0) 
            (tabuleiro-solucao (tabuleiro estado))
        )
        (* (funcall avaliacao estado) maximizing-player)
        (let* (
                (sucessores (funcall gera-sucessores estado))
                ;(sucessores (funcall 'ordena-nos sucessores avaliacao))
            )
            
            (cond
                ; Se não houver sucessores retorna a utilidade deste nó
                ((null sucessores) (* maximizing-player (funcall avaliacao estado)))
                ; Senão devolve a utilidade dos nós sucessores
                (t (funcall 'negamax-sucessores sucessores profundidade-maxima avaliacao gera-sucessores alpha beta maximizing-player (1+ profundidade-atual)))
            )
        )
    )
)

(defun negamax-sucessores (sucessores profundidade-maxima avaliacao gera-sucessores alpha beta maximizing-player profundidade-atual &optional (utilidade *minus-infinity*))
    (if
        ; Quando todos os nós forem avaliados, retorna a utilidade 
        (null sucessores)
        utilidade
        (let* (
                (sucessor (car sucessores))
                (utilidade (max utilidade (- (funcall 'negamax-algorithm sucessor (1- profundidade-maxima) avaliacao gera-sucessores profundidade-atual (- 0 beta) (- 0 alpha) (- 0 maximizing-player)))))
                (alpha (max utilidade alpha))
                (corte (if (>= alpha beta) t nil))
            )

            ; Verifica novas soluções
            (cond 
                ;Se não existe solução e for profundidade 1, adiciona o atual
                ((and (equal profundidade-atual 1) (equal (funcall 'no-solucao) nil))
                        (funcall 'adiciona-solucao sucessor)
                )
                ; Se a profundidade for 1 e a utilidade for maior que a do atual, altera a solucao
                ((and (equal profundidade-atual 1) (> (funcall avaliacao sucessor) (funcall avaliacao (funcall 'no-solucao))))
                        (funcall 'adiciona-solucao sucessor)
                )
            )
            
            ; Adiciona estatisticas do corte
            (if (not (null corte))
                (funcall 'adiciona-corte maximizing-player)
            )

            ; Se houve corte, retorna utilidade do sucessor, se não segue para os restantes sucessores
            (if (not (null corte))
                utilidade
                (negamax-sucessores (cdr sucessores) profundidade-maxima avaliacao gera-sucessores alpha beta maximizing-player profundidade-atual utilidade)
            )
        )
    )
)