;;;; demo_sistema.lisp
;;;; Demonstração do Sistema de Recomendação de Filmes em Common Lisp

;; --- Definição das Estruturas de Dados ---
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

;; --- Armazenamento em Memória ---
(defparameter *users* '())
(defparameter *movies* '())
(defparameter *favorites* '())
(defparameter *next-user-id* 1)
(defparameter *next-movie-id* 1)

;; --- Funções de Demonstração ---

(defun criar-usuario-demo (nome email)
  (let ((new-user (make-user :id *next-user-id* :name nome :email email)))
    (push new-user *users*)
    (incf *next-user-id*)
    (format t "  ✓ Usuário '~a' criado (ID: ~a)~%" nome (user-id new-user))))

(defun criar-filme-demo (titulo ano genero)
  (let ((new-movie (make-movie :id *next-movie-id* :title titulo :year ano :genre genero)))
    (push new-movie *movies*)
    (incf *next-movie-id*)
    (format t "  ✓ Filme '~a' adicionado (ID: ~a)~%" titulo (movie-id new-movie))))

(defun adicionar-favorito-demo (id-usuario id-filme)
  (let ((usuario (find id-usuario *users* :key #'user-id))
        (filme (find id-filme *movies* :key #'movie-id)))
    (when (and usuario filme)
      (push (make-favorite :user-id id-usuario :movie-id id-filme) *favorites*)
      (format t "  ✓ ~a adicionou '~a' aos favoritos~%" 
              (user-name usuario) (movie-title filme)))))

(defun listar-usuarios-demo ()
  (format t "~%--- Lista de Usuários ---~%")
  (if (null *users*)
      (princ "Nenhum usuário cadastrado.~%")
      (dolist (user *users*)
        (format t "  ID: ~a, Nome: ~a, Email: ~a~%"
                (user-id user) (user-name user) (user-email user))))
  (princ "------------------------~%"))

(defun listar-filmes-demo ()
  (format t "~%--- Catálogo de Filmes ---~%")
  (if (null *movies*)
      (princ "Nenhum filme cadastrado.~%")
      (dolist (movie *movies*)
        (format t "  ID: ~a, Título: ~a, Ano: ~a, Gênero: ~a~%"
                (movie-id movie) (movie-title movie) (movie-year movie) (movie-genre movie))))
  (princ "-------------------------~%"))

(defun gerar-recomendacoes-demo (id-usuario nome-usuario)
  (format t "~%Recomendações para ~a:~%" nome-usuario)
  (let ((ids-filmes-favoritos
         (loop for fav in *favorites*
               when (= (favorite-user-id fav) id-usuario)
               collect (favorite-movie-id fav))))
    (if (null ids-filmes-favoritos)
        (format t "  Nenhum filme favorito para basear recomendações.~%")
        (let* ((filmes-favoritos 
                (loop for movie in *movies*
                      when (member (movie-id movie) ids-filmes-favoritos)
                      collect movie))
               (generos-favoritos 
                (remove-duplicates (mapcar #'movie-genre filmes-favoritos) :test #'string=))
               (recomendacoes
                (loop for movie in *movies*
                      when (and (member (movie-genre movie) generos-favoritos :test #'string=)
                               (not (member (movie-id movie) ids-filmes-favoritos)))
                      collect movie)))
          (if (null recomendacoes)
              (format t "  Não encontramos novas recomendações.~%")
              (dolist (rec recomendacoes)
                (format t "  • ~a (~a)~%" (movie-title rec) (movie-genre rec))))))
  (format t "~%"))

(defun mostrar-estatisticas ()
  (format t "~%6. ESTATÍSTICAS FINAIS~%")
  (format t "----------------------~%")
  (format t "  • Total de usuários: ~a~%" (length *users*))
  (format t "  • Total de filmes: ~a~%" (length *movies*))
  (format t "  • Total de favoritos: ~a~%" (length *favorites*))
  (let ((generos (remove-duplicates (mapcar #'movie-genre *movies*) :test #'string=)))
    (format t "  • Gêneros disponíveis: ~a~%" generos))
  (format t "~%"))

(defun demo-sistema ()
  (format t "=== SISTEMA DE RECOMENDAÇÃO DE FILMES - COMMON LISP ===~%")
  (format t "~%")
  
  ;; Limpar dados
  (setf *users* '())
  (setf *movies* '())
  (setf *favorites* '())
  (setf *next-user-id* 1)
  (setf *next-movie-id* 1)
  
  ;; 1. Criar usuários
  (format t "1. CRIANDO USUÁRIOS~%")
  (format t "-------------------~%")
  (criar-usuario-demo "João Silva" "joao@email.com")
  (criar-usuario-demo "Maria Santos" "maria@email.com")
  (criar-usuario-demo "Pedro Costa" "pedro@email.com")
  (format t "~%")
  
  ;; 2. Adicionar filmes
  (format t "2. ADICIONANDO FILMES~%")
  (format t "---------------------~%")
  (criar-filme-demo "O Senhor dos Anéis" 2001 "Fantasia")
  (criar-filme-demo "Matrix" 1999 "Ficção Científica")
  (criar-filme-demo "Titanic" 1997 "Romance")
  (criar-filme-demo "Star Wars" 1977 "Ficção Científica")
  (criar-filme-demo "Harry Potter" 2001 "Fantasia")
  (criar-filme-demo "Notebook" 2004 "Romance")
  (criar-filme-demo "Interestelar" 2014 "Ficção Científica")
  (criar-filme-demo "Avatar" 2009 "Ficção Científica")
  (criar-filme-demo "Romeu e Julieta" 1996 "Romance")
  (criar-filme-demo "O Hobbit" 2012 "Fantasia")
  (format t "~%")
  
  ;; 3. Mostrar dados
  (format t "3. DADOS DO SISTEMA~%")
  (format t "------------------~%")
  (listar-usuarios-demo)
  (format t "~%")
  (listar-filmes-demo)
  (format t "~%")
  
  ;; 4. Adicionar favoritos
  (format t "4. ADICIONANDO FAVORITOS~%")
  (format t "-----------------------~%")
  (adicionar-favorito-demo 1 1)
  (adicionar-favorito-demo 1 5)
  (adicionar-favorito-demo 2 2)
  (adicionar-favorito-demo 2 4)
  (adicionar-favorito-demo 3 3)
  (adicionar-favorito-demo 3 6)
  (format t "~%")
  
  ;; 5. Gerar recomendações
  (format t "5. SISTEMA DE RECOMENDAÇÕES~%")
  (format t "---------------------------~%")
  (gerar-recomendacoes-demo 1 "João Silva")
  (gerar-recomendacoes-demo 2 "Maria Santos")
  (gerar-recomendacoes-demo 3 "Pedro Costa")
  (format t "~%")
  
  (format t "=== DEMONSTRAÇÃO CONCLUÍDA ===~%")
  (format t "O sistema Common Lisp está funcionando corretamente!~%")
  (format t "~%")
  
  ;; 6. Mostrar estatísticas
  (mostrar-estatisticas))

;; Executa a demonstração
(demo-sistema) 