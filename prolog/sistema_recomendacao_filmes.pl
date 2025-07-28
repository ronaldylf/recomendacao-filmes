% recomendacao.pl
% Sistema de recomendação de filmes em SWI-Prolog.

% --- Declaração de Fatos Dinâmicos ---
% Precisamos declarar que os fatos podem ser adicionados (assertz)
% ou removidos (retract) durante a execução.
:- dynamic user/3.       % user(Id, Name, Email)
:- dynamic movie/4.      % movie(Id, Title, Year, Genre)
:- dynamic favorite/2.   % favorite(UserId, MovieId)
:- dynamic id_counter/2. % id_counter(Type, NextId)


% --- Predicados para Gerenciar IDs ---
% Obtém o próximo ID para um tipo (user ou movie) e atualiza o contador.
get_next_id(Type, Id) :-
    id_counter(Type, Id),
    NextId is Id + 1,
    retract(id_counter(Type, Id)),
    assertz(id_counter(Type, NextId)).


% --- Predicados para Gerenciar Usuários ---

create_user :-
    write('--- Criar Novo Usuário ---'), nl,
    write('Digite o nome do usuário: '), read_line_to_string(user_input, Name),
    write('Digite o email do usuário: '), read_line_to_string(user_input, Email),
    (   user(_, _, Email) ->
        format('>> Erro: Já existe um usuário com o email ~w <<~n', [Email])
    ;
        get_next_id(user, Id),
        assertz(user(Id, Name, Email)),
        format('>> Usuário "~w" criado com sucesso! ID: ~w <<~n', [Name, Id])
    ).

list_users :-
    write('--- Lista de Usuários ---'), nl,
    (   \+ user(_, _, _) ->
        write('Nenhum usuário cadastrado.'), nl
    ;
        forall(user(Id, Name, Email),
               format('  ID: ~w, Nome: ~w, Email: ~w~n', [Id, Name, Email]))
    ),
    write('------------------------'), nl.


% --- Predicados para Gerenciar Filmes ---

create_movie :-
    write('--- Adicionar Novo Filme ---'), nl,
    write('Digite o título do filme: '), read_line_to_string(user_input, Title),
    write('Digite o ano do filme: '), read_line_to_string(user_input, YearString),
    atom_number(YearString, Year),
    write('Digite o gênero do filme: '), read_line_to_string(user_input, Genre),
    get_next_id(movie, Id),
    assertz(movie(Id, Title, Year, Genre)),
    format('>> Filme "~w" adicionado com sucesso! ID: ~w <<~n', [Title, Id]).

list_movies :-
    write('--- Catálogo de Filmes ---'), nl,
    (   \+ movie(_, _, _, _) ->
        write('Nenhum filme cadastrado.'), nl
    ;
        forall(movie(Id, Title, Year, Genre),
               format('  ID: ~w, Título: ~w, Ano: ~w, Gênero: ~w~n', [Id, Title, Year, Genre]))
    ),
    write('-------------------------'), nl.


% --- Predicados para Favoritos e Recomendações ---

add_favorite :-
    write('--- Adicionar Filme aos Favoritos ---'), nl,
    write('Digite o ID do usuário: '), read_line_to_string(user_input, UserIdString),
    atom_number(UserIdString, UserId),
    write('Digite o ID do filme para favoritar: '), read_line_to_string(user_input, MovieIdString),
    atom_number(MovieIdString, MovieId),
    % Validar a entrada
    (   \+ user(UserId, _, _) ->
        format('>> Erro: Usuário com ID ~w não encontrado. <<~n', [UserId])
    ;   \+ movie(MovieId, _, _, _) ->
        format('>> Erro: Filme com ID ~w não encontrado. <<~n', [MovieId])
    ;   favorite(UserId, MovieId) ->
        format('>> Erro: Este filme já está nos favoritos. <<~n')
    ;   % Se tudo estiver certo, adiciona o favorito
        user(UserId, UserName, _),
        movie(MovieId, MovieTitle, _, _),
        assertz(favorite(UserId, MovieId)),
        format('>> Filme "~w" adicionado aos favoritos de "~w". <<~n', [MovieTitle, UserName])
    ).


% Regra: um filme é uma recomendação para um usuário se...
is_recommendation_for(UserId, MovieId, MovieTitle, MovieGenre) :-
    % 1. O filme existe.
    movie(MovieId, MovieTitle, _, MovieGenre),

    % 2. O gênero do filme está na lista de gêneros favoritos do usuário.
    findall(Genre, (favorite(UserId, FavMovieId), movie(FavMovieId, _, _, Genre)), FavoriteGenresWithDups),
    list_to_set(FavoriteGenresWithDups, FavoriteGenres),
    member(MovieGenre, FavoriteGenres),

    % 3. O filme NÃO está já nos favoritos do usuário.
    \+ favorite(UserId, MovieId).


get_recommendations :-
    write('--- Obter Recomendações ---'), nl,
    write('Digite o ID do usuário: '), read_line_to_string(user_input, UserIdString),
    atom_number(UserIdString, UserId),
    (   \+ user(UserId, _, _) ->
        format('>> Erro: Usuário com ID ~w não encontrado. <<~n', [UserId])
    ;   \+ favorite(UserId, _) ->
        format('>> O usuário não possui filmes favoritos para basear as recomendações. <<~n')
    ;
        % Encontra todos os filmes únicos que satisfazem a regra
        setof(MovieTitle-MovieGenre,
              MovieId^is_recommendation_for(UserId, MovieId, MovieTitle, MovieGenre),
              Recommendations),

        write('--- Filmes Recomendados Para Você ---'), nl,
        (   Recommendations == [] ->
            write('Não encontramos novas recomendações no momento.'), nl
        ;
            forall(member(Title-Genre, Recommendations),
                   format('  - Título: ~w (Gênero: ~w)~n', [Title, Genre]))
        ),
        write('-----------------------------------'), nl
    ).


% --- Interface Principal do Terminal (Menu) ---

main_menu_loop :-
    repeat,
    write('\n========= Sistema de Filmes - Menu Principal =========\n'),
    write('1. Criar Usuário\n'),
    write('2. Listar Usuários\n'),
    write('3. Adicionar Filme\n'),
    write('4. Listar Filmes\n'),
    write('5. Adicionar Filme aos Favoritos\n'),
    write('6. Obter Recomendações de Filmes\n'),
    write('0. Sair\n'),
    write('====================================================\n'),
    write('Escolha uma opção: '),
    read_line_to_string(user_input, Choice),
    atom_string(ChoiceAtom, Choice),
    ( process_choice(ChoiceAtom), ChoiceAtom == '0' ->
        ! % Corta e encerra o loop
    ;
        fail % Força o backtracking para o 'repeat'
    ).

process_choice('1') :- create_user.
process_choice('2') :- list_users.
process_choice('3') :- create_movie.
process_choice('4') :- list_movies.
process_choice('5') :- add_favorite.
process_choice('6') :- get_recommendations.
process_choice('0') :- write('Saindo do sistema. Até logo!\n').
process_choice(_) :- write('>> Opção inválida. Por favor, tente novamente. <<\n').

% --- Ponto de Entrada do Programa ---

% Inicializa o banco de dados com alguns dados quando o programa carrega.
:- initialization(assertz(id_counter(user, 1))).
:- initialization(assertz(id_counter(movie, 1))).

start :-
    main_menu_loop.