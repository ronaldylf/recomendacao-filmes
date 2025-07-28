% Demonstração final do sistema de recomendação de filmes
:- [sistema_recomendacao_filmes].

% Demonstração completa do sistema
demonstracao_final :-
    write('=== SISTEMA DE RECOMENDAÇÃO DE FILMES ==='), nl, nl,
    
    % Limpar dados anteriores
    retractall(user(_, _, _)),
    retractall(movie(_, _, _, _)),
    retractall(favorite(_, _)),
    retractall(id_counter(_, _)),
    assertz(id_counter(user, 1)),
    assertz(id_counter(movie, 1)),
    
    % 1. Criar usuários
    write('1. CRIANDO USUÁRIOS'), nl,
    write('-------------------'), nl,
    criar_usuario('João Silva', 'joao@email.com'),
    criar_usuario('Maria Santos', 'maria@email.com'),
    criar_usuario('Pedro Costa', 'pedro@email.com'),
    nl,
    
    % 2. Adicionar filmes
    write('2. ADICIONANDO FILMES'), nl,
    write('---------------------'), nl,
    criar_filme('O Senhor dos Anéis', 2001, 'Fantasia'),
    criar_filme('Matrix', 1999, 'Ficção Científica'),
    criar_filme('Titanic', 1997, 'Romance'),
    criar_filme('Star Wars', 1977, 'Ficção Científica'),
    criar_filme('Harry Potter', 2001, 'Fantasia'),
    criar_filme('Notebook', 2004, 'Romance'),
    criar_filme('Interestelar', 2014, 'Ficção Científica'),
    criar_filme('Avatar', 2009, 'Ficção Científica'),
    criar_filme('Romeu e Julieta', 1996, 'Romance'),
    criar_filme('O Hobbit', 2012, 'Fantasia'),
    nl,
    
    % 3. Mostrar dados
    write('3. DADOS DO SISTEMA'), nl,
    write('------------------'), nl,
    list_users,
    nl,
    list_movies,
    nl,
    
    % 4. Adicionar favoritos
    write('4. ADICIONANDO FAVORITOS'), nl,
    write('-----------------------'), nl,
    adicionar_favorito(1, 1), % João gosta de O Senhor dos Anéis
    adicionar_favorito(1, 5), % João gosta de Harry Potter
    adicionar_favorito(2, 2), % Maria gosta de Matrix
    adicionar_favorito(2, 4), % Maria gosta de Star Wars
    adicionar_favorito(3, 3), % Pedro gosta de Titanic
    adicionar_favorito(3, 6), % Pedro gosta de Notebook
    nl,
    
    % 5. Gerar recomendações
    write('5. SISTEMA DE RECOMENDAÇÕES'), nl,
    write('---------------------------'), nl,
    gerar_recomendacoes(1, 'João Silva'),
    gerar_recomendacoes(2, 'Maria Santos'),
    gerar_recomendacoes(3, 'Pedro Costa'),
    nl,
    
    write('=== DEMONSTRAÇÃO CONCLUÍDA ==='), nl,
    write('O sistema está funcionando corretamente!'), nl.

% Funções auxiliares
criar_usuario(Nome, Email) :-
    get_next_id(user, Id),
    assertz(user(Id, Nome, Email)),
    format('  ✓ Usuário "~w" criado (ID: ~w)~n', [Nome, Id]).

criar_filme(Titulo, Ano, Genero) :-
    get_next_id(movie, Id),
    assertz(movie(Id, Titulo, Ano, Genero)),
    format('  ✓ Filme "~w" adicionado (ID: ~w)~n', [Titulo, Id]).

adicionar_favorito(UserId, MovieId) :-
    user(UserId, UserName, _),
    movie(MovieId, MovieTitle, _, _),
    assertz(favorite(UserId, MovieId)),
    format('  ✓ ~w adicionou "~w" aos favoritos~n', [UserName, MovieTitle]).

gerar_recomendacoes(UserId, UserName) :-
    format('Recomendações para ~w:~n', [UserName]),
    (   \+ favorite(UserId, _) ->
        write('  Nenhum filme favorito para basear recomendações.'), nl
    ;
        setof(MovieTitle-MovieGenre,
              MovieId^is_recommendation_for(UserId, MovieId, MovieTitle, MovieGenre),
              Recommendations),
        (   Recommendations == [] ->
            write('  Não encontramos novas recomendações.'), nl
        ;
            forall(member(Title-Genre, Recommendations),
                   format('  • ~w (~w)~n', [Title, Genre]))
        )
    ),
    nl.

% Executar demonstração
:- initialization(demonstracao_final). 