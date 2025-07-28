;;;; recomendacao.lisp
;;;; Sistema de recomendação de filmes em Common Lisp para o terminal.

;; --- Definição das Estruturas de Dados ---
;; Usamos 'defstruct' para criar estruturas de dados simples,
;; similar a classes básicas ou dicionários em Python.

(defstruct user
  id
  name
  email)

(defstruct movie
  id
  title
  year
  genre)

(defstruct favorite
  user-id
  movie-id)

;; --- Armazenamento em Memória (Banco de Dados Global) ---
;; 'defparameter' cria variáveis globais. A convenção é usar *earmuffs*
;; (asteriscos) para nomes de variáveis globais.

(defparameter *users* '())
(defparameter *movies* '())
(defparameter *favorites* '())

(defparameter *next-user-id* 1)
(defparameter *next-movie-id* 1)

;; --- Funções para Gerenciar Usuários ---

(defun create-user ()
  "Solicita os dados e cria um novo usuário."
  (princ "--- Criar Novo Usuário ---~%")
  (princ "Digite o nome do usuário: ")
  (finish-output) ; Garante que o prompt seja exibido antes de ler
  (let ((name (read-line)))
    (princ "Digite o email do usuário: ")
    (finish-output)
    (let ((email (read-line)))
      ;; Verifica se o email já existe
      (if (find email *users* :key #'user-email :test #'string=)
          (format t "~%>> Erro: Já existe um usuário com este email. <<~%")
          (progn
            (let ((new-user (make-user :id *next-user-id* :name name :email email)))
              (push new-user *users*)
              (incf *next-user-id*)
              (format t "~%>> Usuário '~a' criado com sucesso! ID: ~a <<~%" name (user-id new-user))))))))

(defun list-users ()
  "Exibe todos os usuários cadastrados."
  (format t "~%--- Lista de Usuários ---~%")
  (if (null *users*)
      (princ "Nenhum usuário cadastrado.~%")
      (dolist (user *users*)
        (format t "  ID: ~a, Nome: ~a, Email: ~a~%"
                (user-id user) (user-name user) (user-email user))))
  (princ "------------------------~%"))


;; --- Funções para Gerenciar Filmes ---

(defun create-movie ()
  "Solicita os dados e adiciona um novo filme."
  (princ "--- Adicionar Novo Filme ---~%")
  (princ "Digite o título do filme: ")
  (finish-output)
  (let ((title (read-line)))
    (princ "Digite o ano do filme: ")
    (finish-output)
    (let ((year (parse-integer (read-line) :junk-allowed t)))
      (princ "Digite o gênero do filme: ")
      (finish-output)
      (let ((genre (read-line)))
        (let ((new-movie (make-movie :id *next-movie-id* :title title :year year :genre genre)))
          (push new-movie *movies*)
          (incf *next-movie-id*)
          (format t "~%>> Filme '~a' adicionado com sucesso! ID: ~a <<~%" title (movie-id new-movie)))))))

(defun list-movies ()
  "Exibe todos os filmes cadastrados."
  (format t "~%--- Catálogo de Filmes ---~%")
  (if (null *movies*)
      (princ "Nenhum filme cadastrado.~%")
      (dolist (movie *movies*)
        (format t "  ID: ~a, Título: ~a, Ano: ~a, Gênero: ~a~%"
                (movie-id movie) (movie-title movie) (movie-year movie) (movie-genre movie))))
  (princ "-------------------------~%"))


;; --- Funções para Favoritos e Recomendações ---

(defun add-favorite ()
  "Associa um filme como favorito de um usuário."
  (princ "--- Adicionar Filme aos Favoritos ---~%")
  (princ "Digite o ID do usuário: ")
  (finish-output)
  (let ((user-id (parse-integer (read-line) :junk-allowed t)))
    (princ "Digite o ID do filme para favoritar: ")
    (finish-output)
    (let ((movie-id (parse-integer (read-line) :junk-allowed t)))
      (unless (and user-id movie-id)
        (format t "~%>> Erro: ID deve ser um número. <<~%")
        (return-from add-favorite))

      (let ((user (find user-id *users* :key #'user-id))
            (movie (find movie-id *movies* :key #'movie-id)))
        (cond
          ((not user) (format t "~%>> Erro: Usuário não encontrado. <<~%"))
          ((not movie) (format t "~%>> Erro: Filme não encontrado. <<~%"))
          ((some #'(lambda (fav)
                     (and (= (favorite-user-id fav) user-id)
                          (= (favorite-movie-id fav) movie-id)))
                 *favorites*)
           (format t "~%>> Erro: Este filme já está na sua lista de favoritos. <<~%"))
          (t (push (make-favorite :user-id user-id :movie-id movie-id) *favorites*)
             (format t "~%>> Filme '~a' adicionado aos favoritos de '~a'. <<~%"
                     (movie-title movie) (user-name user))))))))

(defun get-recommendations ()
  "Recomenda filmes com base nos gêneros favoritos de um usuário."
  (princ "--- Obter Recomendações ---~%")
  (princ "Digite o ID do usuário para obter recomendações: ")
  (finish-output)
  (let ((user-id (parse-integer (read-line) :junk-allowed t)))
    (unless user-id
      (format t "~%>> Erro: ID deve ser um número. <<~%")
      (return-from get-recommendations))
    
    (unless (find user-id *users* :key #'user-id)
        (format t "~%>> Erro: Usuário não encontrado. <<~%")
        (return-from get-recommendations))

    ;; 1. Coleta os IDs dos filmes favoritos do usuário
    (let ((favorite-movie-ids
           (loop for fav in *favorites*
                 when (= (favorite-user-id fav) user-id)
                 collect (favorite-movie-id fav))))

      (if (null favorite-movie-ids)
          (format t "~%>> O usuário não possui filmes favoritos. <<~%")
          ;; Lógica principal de recomendação
          (progn
            ;; 2. Coleta os gêneros desses filmes
            (let* ((favorite-movies (loop for movie in *movies*
                                         when (member (movie-id movie) favorite-movie-ids)
                                         collect movie))
                   (favorite-genres (remove-duplicates (mapcar #'movie-genre favorite-movies) :test #'string=)))
            
              ;; 3. Encontra filmes com gêneros correspondentes que não são favoritos
              (let ((recommendations
                     (loop for movie in *movies*
                           when (and (member (movie-genre movie) favorite-genres :test #'string=)
                                     (not (member (movie-id movie) favorite-movie-ids)))
                           collect movie)))
                
                (format t "~%--- Filmes Recomendados Para Você ---~%")
                (if (null recommendations)
                    (princ "Não encontramos novas recomendações no momento.~%")
                    (dolist (rec recommendations)
                      (format t "  - Título: ~a (Gênero: ~a)~%" (movie-title rec) (movie-genre rec))))
                (princ "-----------------------------------~%"))))))))

;; --- Interface Principal do Terminal (Menu) ---

(defun main-menu ()
  "Exibe o menu principal em um loop."
  (loop
     (format t "~%========= Sistema de Filmes - Menu Principal =========~%")
     (format t "1. Criar Usuário~%")
     (format t "2. Listar Usuários~%")
     (format t "3. Adicionar Filme~%")
     (format t "4. Listar Filmes~%")
     (format t "5. Adicionar Filme aos Favoritos~%")
     (format t "6. Obter Recomendações de Filmes~%")
     (format t "0. Sair~%")
     (format t "====================================================~%")
     (princ "Escolha uma opção: ")
     (finish-output)
     (let ((choice (read-line)))
       (cond
         ((string= choice "1") (create-user))
         ((string= choice "2") (list-users))
         ((string= choice "3") (create-movie))
         ((string= choice "4") (list-movies))
         ((string= choice "5") (add-favorite))
         ((string= choice "6") (get-recommendations))
         ((string= choice "0")
          (format t "Saindo do sistema. Até logo!~%")
          (return)) ; 'return' sai do loop
         (t (format t "~%>> Opção inválida. Por favor, tente novamente. <<~%"))))))

;; Ponto de entrada do programa
(defun start-app ()
  (main-menu))

;; Inicia a aplicação
(start-app)