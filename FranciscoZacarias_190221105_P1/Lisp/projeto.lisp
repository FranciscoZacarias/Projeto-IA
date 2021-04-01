;;; 1. Projeto 
;;; Disciplina de IA - 2020 / 2021
;;; Autor: Francisco Zacarias n.190221105

;;; projeto.lisp Carrega os outros ficheiros de codigo, escreve e le ficheiros, e trata da interacao com o utilizador

;;; Inicializacaoo do programa
;; iniciar

(defun nome-ficheiros ()
  "Devolve uma lista dos ficheiros que devem ser compilados para a execucao do projeto.
Os ficheiros devem estar no mesmo diretorio deste projeto.lisp"
  '("procura.lisp" "puzzle.lisp")
  )

(defun diretorio-projeto ()
  "Devolve o diretorio do projeto"
  "D:\\IA\\FranciscoZacarias_190221105_P1\\Lisp\\"  
  )

(defun problemas-dat()
  "problemas.dat"
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

"Compilar ficheiros .lisp"
(funcall 'carregar-ficheiros-lisp (funcall 'nome-ficheiros))

(defun ler-input (&optional prompt)
  prompt
  (format t "~&Input>")
  (read )
  )

(defun carregar-helper (strm)
  "Funcao auxiliar para carregar-problemas.
Carrega recursivamente todos os estados que se encontram na stream passada por argumento."
  (let ((linha (read strm nil 'eof)))
    (cond
     ((eq linha 'eof) nil)
     (t (cons (list (first linha) (second linha) 0 nil) (carregar-helper strm)))
     )
    )
  )

(defun carregar-problemas ()
  "Carrega os problemas do ficheiro que se encontra na funcao problemas-dat"
  (with-open-file (strm (concatenate 'string (funcall 'diretorio-projeto) (funcall 'problemas-dat)) :direction :input)
    (funcall 'carregar-helper strm)
    )
  )

(defun escrever-para-ficheiros (ficheiro texto)
  "Escreve no ficheiro ficheiro que existe no diretorio do projeto"
  (with-open-file (strm (concatenate 'string (funcall 'diretorio-projeto) ficheiro) :direction :output :if-exists :append :if-does-not-exist :create)
    (format strm (concatenate 'string " ~&" texto))
    )
  )

(defun iniciar ()
  "Funcao auxiliar de iniciar. Permite iniciar o programa."
  (setf *print-level* nil) ;Uso excepcional de setf, para o LispWorks mostrar o conteudo de listas profundas

  (let ((problemas (funcall 'carregar-problemas)))
    (format t " ~& ~& ~& ~&")
    (format t "*********************************** ~&")
    (format t "* PROJETO IA - FRANCISCO ZACARIAS * ~&")
    (format t "*********************************** ~&")
    (format t "*     Problemas carregados: ~A     * ~&" (length problemas))
    (format t "*********************************** ~&")
		
    (let* ( (problema (funcall 'ler-input (format t " ~&Escolha um problema (1 ate ~A):" (length problemas))))
                      (algoritmo (funcall 'ler-input (format t "~&Escolha um algoritmo (BFS DFS A-STAR):")))
                      (profundidade (funcall 'ler-input (format t "~&Profundidade (DFS) (-1 para ignorar):")))
                      (heuristica (funcall 'ler-input (format t "~&Heuristica (A*) (Base, Nova)")))
                      (solucao (funcall 'procura-espaco-estados algoritmo (nth (1- (coerce problema 'number)) problemas) profundidade heuristica))

                      (no-solucao (first solucao))
                      (nos-expandidos (second solucao))
                      (nos-gerados (third solucao))
                      (penetrancia (fourth solucao))
                      (ramificacao (fifth solucao))
                      (tempo-total (sixth solucao))
                      )

      (funcall 'escrever-para-ficheiros (funcall 'logfile-dat) 
               (concatenate 'string "Problema: " (write-to-string problema) 
                            " || Algoritmo: " (write-to-string algoritmo )
                            " || Heuristica: " (if (equal algoritmo 'a-star) (write-to-string heuristica) "NIL")
                            " || Nos-Expandidos: " (write-to-string nos-expandidos)
                            " || Nos-Gerados: " (write-to-string nos-gerados)
                            " || Penetrancia: " (write-to-string penetrancia)
                            " || Ramificação: " (write-to-string ramificacao)
                            " || Tempo de execução:" (write-to-string tempo-total) "ms"
                            " ~&Solução do problema: " (write-to-string no-solucao) "  ~& "
                            )
               )

      (format t "~&Problema escolhido: ~A~&" problema)
      (format t "~&Algoritmo escolhido: ~A~&" algoritmo)
      (format t "~&Profundidade maxima: ~A~&" profundidade)
      (format t "~&Heuristica escolhida: ~A~&" heuristica)
      (format t " ~&No solucao: ~& ~A ~&" no-solucao) 
			
      (case (funcall 'ler-input (format t " ~& ~&Continuar? (Y/N)"))
        (y (iniciar))
        (otherwise "Bye!")
        )
      )
    )
  )