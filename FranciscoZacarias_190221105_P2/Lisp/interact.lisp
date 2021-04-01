;;; 1. Projeto 2
;;; Disciplina de IA - 2020 / 2021
;;; Autor: Francisco Zacarias n.190221105

;;; interact.lisp Trata da interacao com o utilizador

(defun estado-inicial ()
  '(
		(
			(0 0 0 0) 
			(0 0 0 0) 
			(0 0 0 0) 
			(0 0 0 0)
		)
		(
			(branca redonda alta oca)
			(preta redonda alta oca)
			(branca redonda baixa oca)
			(preta redonda baixa oca)
			(branca quadrada alta oca)
			(preta quadrada alta oca)
			(branca quadrada baixa oca)
			(preta quadrada baixa oca)
			(branca redonda alta cheia)
			(preta redonda alta cheia)
			(branca redonda baixa cheia)
			(preta redonda baixa cheia)
			(branca quadrada alta cheia)
			(preta quadrada alta cheia)
			(branca quadrada baixa cheia)
			(preta quadrada baixa cheia)
		)
	)
)

(defun nome-ficheiros ()
"Devolve uma lista dos ficheiros que devem ser compilados para a execucao do projeto.
Os ficheiros devem estar no mesmo diretorio deste projeto.lisp"
	'("algoritmo.lisp" "jogo.lisp")
)

(defun diretorio-projeto ()
"Devolve o diretorio do projeto"
	"D:\\IA\\FranciscoZacarias_190221105_P2\\Lisp\\"  
)

(defun logfile-dat()
	"logs.dat"
)

(defun carregar-ficheiros-lisp (ficheiros) 
  "Carrega e compila uma lista de ficheiros"
	(mapcar #'(lambda (ficheiro) 
				(load (compile-file (concatenate 'string (funcall 'diretorio-projeto) ficheiro)))
				)ficheiros
			)
)

(defun imprime-lista (lista &optional (index 0))
"Imprime o tabuleiro de um estado"
	(cond
		((null lista) "==========================")
		(t 
			(let (
					(peca (car lista))
				)
				(format t "~A ~&" (concatenate 'string (write-to-string index) ". " (write-to-string peca)))
				(imprime-lista (cdr lista) (1+ index))
			)
		)
	)
)

(defun imprime-tabuleiro (tabuleiro)
	(format t " ~&     0 1 2 3 ~&")
	(funcall 'imprime-lista tabuleiro)
	(format t " ~&")
)

"Compilar ficheiros .lisp"
(funcall 'carregar-ficheiros-lisp (funcall 'nome-ficheiros))

(defun ler-input (&optional prompt)
	prompt
	(format t "~&Input>")
	(read )
)

(defun escrever-para-ficheiros (ficheiro texto)
  "Escreve no ficheiro ficheiro que existe no diretorio do projeto"
(with-open-file (strm (concatenate 'string (funcall 'diretorio-projeto) ficheiro) :direction :output :if-exists :append :if-does-not-exist :create)
		(format strm (concatenate 'string " ~&" texto))
	)
)

;; Devolve o tabuleiro onde a próxima jogada será feita
(defun jogar (tempo profundidade estado)
	(negamax estado profundidade 'funcao-avaliacao tempo)
)

(defun jogada-pc(tempo profundidade modo-jogo estado turno)
	(let* (
			(resultado			(funcall 'jogar tempo profundidade estado))
			(tempo 				(first resultado))
			(utilidade 			(second resultado))
			(nos-analisados 	(third resultado))
			(cortes-max 		(fourth resultado))
			(cortes-min 		(fifth resultado))
			(estado-devolvido 	(sixth resultado))
			(estatistica-jogada (concatenate 'string "Jogada PC" (write-to-string turno)
									" || Nós analisados:" (write-to-string nos-analisados)
									" || Cortes MAX: "		(write-to-string cortes-max)
									" || Cortes MIN: " 		(write-to-string cortes-min)
									" || Tempo Gasto: " 	(write-to-string tempo) "ms"
									" || Utilidade:"		(write-to-string utilidade)
									" || Tabuleiro:" 		(write-to-string (funcall 'tabuleiro estado-devolvido))
								)
			)
		)

		; Escreve para o ficheiro
		(funcall 'escrever-para-ficheiros (funcall 'logfile-dat) estatistica-jogada)
		(format t "~A ~& ~&" estatistica-jogada)

		(cond
			; Alguém venceu
			((funcall 'tabuleiro-solucao (tabuleiro estado-devolvido)) (format t "~&PC~A venceu!" turno))
			; Jogo acabou sem vencedores
			((funcall 'sem-mais-jogadas estado-devolvido) (format t "Empate"))
			; Próxima jogada
			(t (iniciar-jogo tempo profundidade modo-jogo estado-devolvido (if (equal turno 1) 2 1)))
		)
	)
)

(defun jogada-humano (tempo profundidade modo-jogo estado turno)
	(funcall 'imprime-tabuleiro (tabuleiro estado))
	(funcall 'imprime-lista (reserva estado))
	(let* (
			(tabuleiro-x (funcall 'ler-input (format t "~&Escolha uma peca do tabuleiro (COORDENADA X):")))
			(tabuleiro-y (funcall 'ler-input (format t "~&Escolha uma peca do tabuleiro (COORDENADA Y):")))
			(peca-da-reserva (funcall 'ler-input (format t "~&Escolha uma peca da reserva (numero):")))
			(novo-estado (funcall 'operador (list tabuleiro-x tabuleiro-y) (nth peca-da-reserva (reserva estado)) estado))
			(jogada (concatenate 'string "Jogada HUMANO "
						" || Peca: " (write-to-string (nth peca-da-reserva (reserva estado)))
						" || Posicao: (x= " (write-to-string tabuleiro-x) ",y=" (write-to-string tabuleiro-y) ")"
						" || Tabuleiro: " (write-to-string (funcall 'tabuleiro novo-estado))
					)
			)
		)

		; Escreve para o ficheiro
		(funcall 'escrever-para-ficheiros (funcall 'logfile-dat) jogada)
		(format t "~A ~& ~&" jogada)

		(cond
			; Alguém venceu
			((funcall 'tabuleiro-solucao (tabuleiro novo-estado)) (format t "~&~A venceu!" (if (equal turno 1) "HUMANO" "PC")))
			; Jogo acabou sem vencedores
			((funcall 'sem-mais-jogadas novo-estado) (format t "Empate"))
			; Próxima jogada
			(t (iniciar-jogo tempo profundidade modo-jogo novo-estado (if (equal turno 1) 2 1)))
		)
	)
)

; turno 1 é do humano, turno 2 é do computador
(defun iniciar-jogo (tempo profundidade modo-jogo &optional estado (turno 1))
	(ecase modo-jogo
		; Humano vs PC
		(1 
			; Turno 1 - Humano, 2 - PC
			(if (equal turno 1)
				(funcall 'jogada-humano tempo profundidade modo-jogo estado turno)
				(funcall 'jogada-pc tempo profundidade modo-jogo estado turno)
			)
		)
		; PC1 vs PC2
		(2
			(funcall 'jogada-pc tempo profundidade modo-jogo estado turno)
		)
	)
)

(defun iniciar ()
	"Funcao auxiliar de iniciar. Permite iniciar o programa."
	(setf *print-level* nil) ;Uso excepcional de setf, para o LispWorks mostrar o conteudo de listas profundas

	(format t " ~& ~& ~& ~&")
	(format t "************************************* ~&")
	(format t "* PROJETO 2 IA - FRANCISCO ZACARIAS * ~&")
	(format t "************************************* ~&")
	
	(let* (
			; Modo de jogo 1: Humano vs PC :: Modo de jogo 2: PC1 vs PC2
			(modo-jogo (funcall 'ler-input (format t " ~&Modo de jogo:~&1 - Humano vs PC~&2 - PC1 vs PC2")))
			(primeira-jogada (if (equal modo-jogo 2) 1 (funcall 'ler-input (format t "~&Quem joga primeiro? (1 - Humano, 2 - PC):"))))
			(tempo-limite (funcall 'ler-input (format t "~&Tempo limite para o PC jogar (1000 - 5000):")))
			(profundidade-maxima (funcall 'ler-input (format t "~&Profundidade limite de procura do algoritmo?:")))
		)
	
		(format t " ~&Modo de jogo: ~A" (if (equal modo-jogo 1) "Humano vs PC" "PC1 vs PC2"))
		(if (not (null primeira-jogada)) (format t "~&Primeiro a jogar: ~A" (if (equal primeira-jogada 1) "Humano" "PC")))
		(format t "~&Tempo limite do PC para jogar: ~Ams" tempo-limite)
		(format t "~&Profundidade limite do algoritmo: ~A" profundidade-maxima)

		(funcall 'escrever-para-ficheiros (funcall 'logfile-dat) "===== NOVO JOGO ===== ~&")
		(funcall 'iniciar-jogo tempo-limite profundidade-maxima modo-jogo (funcall 'teste2) primeira-jogada)
		(funcall 'escrever-para-ficheiros (funcall 'logfile-dat) "===== FIM DO JOGO ===== ~& ~&")
	)

	(case (funcall 'ler-input (format t " ~& ~&Continuar? (Y/N)"))
        (y (iniciar))
        (otherwise "Bye!")
    )
)


