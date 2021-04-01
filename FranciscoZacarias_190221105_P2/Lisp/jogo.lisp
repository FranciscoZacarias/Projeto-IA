;;; Projeto 2
;;; Disciplina de IA - 2020 / 2021
;;; Autor: Francisco Zacarias nº190221105

;;; jogo.lisp Operadores do jogo

(defun no-teste ()
"Função que representa um tabuleiro de teste"
  '(
		(
			((branca quadrada alta oca)   (preta redonda baixa oca)  0                             (branca quadrada alta cheia))
			(0    0                          0   (preta quadrada alta oca))
			(0 (preta redonda alta cheia) 0   0)
			(0   0                          (branca quadrada baixa cheia) (branca redonda baixa oca))
	
		)
		(
			(branca quadrada baixa oca)
			(branca redonda alta cheia)
			(branca redonda alta oca)
			(preta quadrada baixa cheia)
			(preta quadrada baixa oca)
			(preta redonda baixa cheia)
			(preta redonda alta oca)
			(preta quadrada alta cheia)
			(branca redonda baixa cheia)
		)
	)
)

(defun teste2 ()
"Função que representa um tabuleiro de teste"
	'(
		(
			((branca redonda alta oca)     0 0 0)
			((preta quadrada alta cheia)   0 0 0) 
			((preta redonda baixa oca)     0 0 0) 
			((branca quadrada baixa cheia) 0 0 0)
		)
		(
			(preta redonda alta oca)
			(branca quadrada alta oca)
			(preta quadrada alta oca)
			(branca quadrada baixa oca)
			(preta quadrada baixa oca)
			(branca redonda alta cheia)
			(preta redonda alta cheia)
			(branca redonda baixa cheia)
			(preta redonda baixa cheia)
			(branca quadrada alta cheia)
			(branca quadrada baixa cheia)
			(preta quadrada baixa cheia)
		)
	)
)


;;; #REGION: Seletores 

(defun tabuleiro (jogo)
"Retorna o estado atual do tabuleiro do jogo"
	(first jogo)
)

(defun reserva (jogo)
"Retorna o estado atual da reserva do jogo"
  	(second jogo)
)

(defun no-profundidade (jogo)
"Retorna a profundidade do no jogo"
	(third jogo)
)

(defun linha (n tabuleiro)
"Retorna a linha n de um tabuleiro. Retorna nil se n for invalido"
  	(or (< n 0) (> n 3) nil)
	(nth n tabuleiro)
)

(defun coluna (n tabuleiro)
"Retorna a coluna n de um tabuleiro. Retorna nil se n for invalido"
	(or (< n 0) (> n 3) nil)
	(mapcar #'(lambda (linha)
		(nth n linha)
		) tabuleiro
	)
)

(defun celula (x y tabuleiro)
"Devolve o conteudo da celula (x,y)"
	(funcall 'linha y (funcall 'coluna x tabuleiro))
)

(defun celula-x (celula)
"Retorna a coordenada x de uma celula"
	(first celula)
)

(defun celula-y (celula)
"Retorna a coordenada y de uma celula"
	(second celula)
)

(defun diagonal-1-helper (posicao tabuleiro)
"Funcao de auxilio para a funcao diagonal-1"
	(if (> posicao 3) nil
	(cons (celula posicao posicao tabuleiro) (diagonal-1-helper (1+ posicao) tabuleiro)))
)

(defun diagonal-1 (tabuleiro)
"Devolve a diagonal que comeca na celula (0,0) ate a celula (3,3)"
	(funcall 'diagonal-1-helper 0 tabuleiro)
)

(defun diagonal-2-helper (x y tabuleiro)
"Funcao de auxilio para a funcao diagonal-2"
	(if (or (< y 0) (> x 3)) nil
	(cons (celula x y tabuleiro) (diagonal-2-helper (1+ x) (1- y) tabuleiro)))
)

(defun diagonal-2 (tabuleiro)
"Devolve a diagonal que comeca na celula (0,3) ate a celula (3,0)"
	(funcall 'diagonal-2-helper 0 3 tabuleiro)
)

(defun peca-cor (peca)
"Devolve a cor de uma peca, preta ou branca"
	(first peca)
)

(defun peca-forma (peca)
"Devolve a forma de uma peca, redonda ou quadrada"
	(second peca)
)

(defun peca-altura (peca)
"Devolve a altura de uma peca, alta ou baixa"
	(third peca)
)

(defun peca-densidade (peca)
"Devolve a densidade de uma peca"
	(fourth peca)
)

(defun peca-funcoes ()
"Devolve lista de funcoes seletoras das caracteristicas de uma peca"
	'(peca-cor peca-forma peca-altura peca-densidade)
)

;;; #ENDREGION: Seletores

;;; #REGION: Funcoes Auxiliares

(defun casa-vaziap (x y tabuleiro)
"Retorna T ou NIL consoante uma cela esta vazia ou nao, respetivamente."
	(if (equal 0 (funcall 'celula x y tabuleiro)) T nil)
)

(defun peca-na-reserva (peca reserva)
"Verifica se uma peca esta na reserva"
	(cond
		((equal reserva nil) nil)
		((equal peca (car reserva)) t)
		(t (peca-na-reserva peca (cdr reserva)))
	)
)

(defun remover-peca (peca reserva)
"Remove uma peca da reserva"
	(remove nil
		(mapcar #'(lambda (elemento)
				(if (equal peca elemento) nil elemento)
			)reserva
		)
	)
)

(defun substituir-posicao (indice peca linha &optional (posicao 0))
"Substitui o indice de uma linha pela peca dada"
	(if (> posicao 3) nil    
	(cond
		((equal indice posicao) (cons peca (substituir-posicao indice peca (cdr linha) (1+ posicao))))
		(t (cons (car linha) (substituir-posicao indice peca (cdr linha) (1+ posicao))))
	))
)

(defun substituir (x y peca tabuleiro &optional (posicao 0))
"Substitui uma peca no tabuleiro, na celula (x,y). 
Retorna o tabuleiro. 
Não faz validações se a casa esta vazia"
	(if (> posicao 3) nil
	(cond
		((equal x posicao) (cons (substituir-posicao y peca (linha x tabuleiro))
					 (substituir x y peca tabuleiro (1+ posicao))))
		(t (cons (linha posicao tabuleiro) (substituir x y peca tabuleiro (1+ posicao))))
	))
)

(defun lista-solucao-caracteristica (lista seletor)
"Funcao auxiliar lista-solucao.
Verifica se uma lista contém uma solução com base na caracteristica avaliada pela funcao seletor, 
que seleciona que caracteristica das pecas é que vai ser avaliada.
Retorna nil se nao houver solucao. 
Se houver solucao, Retorna a caracteristica avaliada como solucao."
	(cond
		((equal (length lista) 2) (if (equal (funcall seletor (first lista)) (funcall seletor (second lista))) (funcall seletor (first lista)) nil))
		(t 
			(if (equal (funcall seletor (car lista)) (lista-solucao-caracteristica (cdr lista) seletor)) 
				(funcall seletor (car lista)) 
				nil
			)
		)
	)
)

(defun lista-solucao (lista seletores)
"Funcao auxiliar de tabuleiro-solucao. Verifica se uma lista de 4 elementos tem uma solucao com base nas funcoes que selecionam as suas caracteristicas (cor forma altura densidade).
Assume que a lista tem 4 pecas. Retorna t ou nil."
	(cond 
		((equal 0 (find 0 lista)) nil) ;; Ignora listas com lugares vazios
		((null (remove nil (mapcar #'(lambda (seletor)
					(funcall 'lista-solucao-caracteristica lista seletor)
				) (funcall seletores)
		))) nil)
		(t t)
	)
)

(defun sequencia-4-listas-helper (tabuleiro funcao posicao)
"Funcao auxiliar para a funcao sequencia-4-listas. Ler documentacao da mesma."
	(cond 
		((> posicao 3) nil)
		(t 
			(cons
				(funcall funcao posicao tabuleiro)
				(sequencia-4-listas-helper tabuleiro funcao (1+ posicao))
			)
		)
	)
)

(defun sequencia-4-listas (tabuleiro funcao)
"Funcao auxiliar para tabuleiro listas.
Retorna uma lista com todas as 4 linhas ou 4 colunas, com base na funcao. Funcao deve ser linha ou coluna."
	(sequencia-4-listas-helper tabuleiro funcao 0)
)

(defun tabuleiro-listas (tabuleiro)
"Funcao auxiliar para tabuleiro-solucao.
Retorna uma lista contem as 10 linhas do tabuleiro que devem ser verificadas pela condicao da solucao"
	(append 
			(funcall 'sequencia-4-listas tabuleiro 'linha)
			(funcall 'sequencia-4-listas tabuleiro 'coluna)
			(list (funcall 'diagonal-1 tabuleiro))
			(list (funcall 'diagonal-2 tabuleiro))
	)
)

(defun sem-mais-jogadas (estado)
	(null (reserva estado))
)

(defun tabuleiro-solucao (tabuleiro)
"Verifica se o estado do tabuleiro tem solucao"
	(if (null (remove nil 
			(mapcar #'(lambda (lista)
				(lista-solucao lista 'peca-funcoes)
			)(funcall 'tabuleiro-listas tabuleiro)
		))) nil T
	)
)

(defun celulas-vazias-linha (tabuleiro linha x y)
"Funcao auxiliar para tabuleiro-celulas-vazias.
Retorna a lista de coordenadas de celulas vazias de uma linha.
O argumento y serve para identificar a celula no tabuleiro"
	(cond
		((null linha) nil)
		(t (remove nil
				(cons 
					(if (equal (car linha) 0) (list x y) nil) 
					(celulas-vazias-linha tabuleiro (cdr linha) (1+ x) y)
				)
			)
		)
	)
)

(defun tabuleiro-celulas-vazias-helper (tabuleiro linhas)
"Funcao auxiliar para tabuleiro-celulas-vazias.
Retorna uma lista de todas celulas vazias no tabuleiro"
	(cond 
		((null linhas) nil)
		(t (append 
			(funcall 'celulas-vazias-linha tabuleiro (car linhas) 0 (- 4 (length linhas)))
			(tabuleiro-celulas-vazias-helper tabuleiro (cdr linhas))
			)
		)
	)
)

(defun tabuleiro-celulas-vazias (tabuleiro)
"Funcao auxiliar para sucessores.
Retorna uma lista de celulas que representam todas as celulas vazias do tabuleiro."
	(funcall 'tabuleiro-celulas-vazias-helper tabuleiro (sequencia-4-listas tabuleiro 'linha)) 
)

(defun insere-ordenado (jogo lista operacao)
"Insere o no jogo na lista de forma ordenada com base na operacao e devolve a lista."
	(cond
		((null lista) (list jogo))
		((<= (funcall operacao jogo) (funcall operacao (car lista))) (cons jogo lista))
		(t (cons (car lista) (insere-ordenado jogo (cdr lista) operacao)))
	)
)

;;; Funcoes Auxiliares Procura de Estados

(defun novo-sucessor (operador celula peca jogo)
"Funcao auxiliar para sucessores.
Retorna o sucessor do no com base no operador."
	(let ((sucessor (funcall operador celula peca jogo)))
		(cond
			((equal sucessor nil) nil)
			(t
				(append
					sucessor
				)
			)
		)
	)
)

(defun sucessores-help (jogo operador celulas)
"Funcao auxiliar de sucessores. Coloca todos os novos sucessores na mesma lista"
	(cond
		((null celulas) nil)
		(t (append
			(mapcar #'(lambda (peca-reserva)
						(funcall 'novo-sucessor operador (car celulas) peca-reserva jogo) 
				)(reserva jogo)
			)
			(sucessores-help jogo operador (cdr celulas))
		))
	)
)

(defun sucessores (jogo)
"Retorna uma lista com todos os sucessores de um estado com base no operador."
	(funcall 'sucessores-help jogo 'operador (funcall 'tabuleiro-celulas-vazias (tabuleiro jogo)))
)

(defun no-existep (no lista)
	(cond
	  	((null lista) nil)
		((equal no (car lista)) t)
		(t (no-existep no (cdr lista)))
	)
)


(defun conta-ocorrencia (elemento lista)
	(cond
		((null lista) 0)
		((equal elemento (car lista)) (+ 1 (conta-ocorrencia elemento (cdr lista))))
		(t (conta-ocorrencia elemento (cdr lista)))
	)
)

(defun caracteristica-ocorrencia (lista caracteristicas)
"Retorna quantas ocorrencias de uma certa caracteristica de uma peca existem numa lista.
O argumento opcional, total, nao deve ser passado durante a chamada da funcao."
	(let*
		((lista-caracteristicas
			(mapcar #'(lambda (caracteristica)
			(remove nil (mapcar #'(lambda (peca)
					(if (equal peca 0) nil (funcall caracteristica peca))
				)lista)
			))caracteristicas)))
		(max
			(max (conta-ocorrencia 'branca (first lista-caracteristicas)) (conta-ocorrencia 'preta (first lista-caracteristicas)))
			(max (conta-ocorrencia 'quadrada (second lista-caracteristicas)) (conta-ocorrencia 'redonda (second lista-caracteristicas)))
			(max (conta-ocorrencia 'alta (third lista-caracteristicas)) (conta-ocorrencia 'baixa (third lista-caracteristicas)))
			(max (conta-ocorrencia 'cheia (fourth lista-caracteristicas)) (conta-ocorrencia 'oca (fourth lista-caracteristicas)))
		)
	)
)

(defun lista-max (lista &optional (maior 0))
"Retorna o valor mais alto de uma lista"
	(cond
		((null lista) maior)
		((> (car lista) maior) (lista-max (cdr lista) (car lista)))
		(t (lista-max (cdr lista) maior))
	)
)

(defun heuristica-base-p (jogo)
"Calcula o numero maximo de pecas com caracteristicas comuns alinhadas em qualquer direcao valida."
	(let ((listas-tabuleiro (funcall 'tabuleiro-listas (funcall 'tabuleiro jogo))))
		(lista-max (mapcar #'(lambda (lista)
				(funcall 'caracteristica-ocorrencia lista (peca-funcoes))
			)listas-tabuleiro
		))
	)
)

(defun funcao-avaliacao (jogo)
"Calcula o valor heuristico para o no jogo.
4 = 1000, 3 = 100, 2 = 10, 1 = 1, 0 = 0"
	(let ((pecas-seguidas (heuristica-base-p jogo)))
		(ecase pecas-seguidas
			(0 0)
			(1 10)
			(2 100)
			(3 1000)
			(4 10000)
		)
	)
)

;;; #ENDREGION: Funcoes Auxiliares

;;; #REGION: Operadores

(defun operador (celula peca jogo)
"Move uma peca da reserva para o tabuleiro. Retorna nil se a casa nao estiver vazia ou a peca nao estiver na reserva"
	(cond
		((not (funcall 'casa-vaziap (celula-x celula) (celula-y celula) (funcall 'tabuleiro jogo))) nil)
		((not (funcall 'peca-na-reserva peca (funcall 'reserva jogo))) nil)
		(t (append
				(list (funcall 'substituir (celula-y celula) (celula-x celula) peca (funcall 'tabuleiro jogo)))
				(list (funcall 'remover-peca peca (funcall 'reserva jogo)))
			)
		)
	)
)

;;; #ENDREGION: Operadores
