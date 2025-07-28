# recomendacao_filmes.py

# --- Armazenamento em Memória (Simulando o Banco de Dados) ---
# Listas de dicionários que guardam nossos dados enquanto o programa executa.
usuarios = []
filmes = []
favoritos = []

# Contadores para garantir que cada novo item tenha um ID único.
proximo_id_usuario = 1
proximo_id_filme = 1

# --- Funções para Gerenciar Usuários ---

def criar_usuario():
    """Solicita os dados e cria um novo usuário."""
    global proximo_id_usuario
    print("\n--- Criar Novo Usuário ---")
    nome = input("Digite o nome do usuário: ")
    email = input("Digite o email do usuário: ")

    # Verifica se o email já está em uso para evitar duplicatas.
    for u in usuarios:
        if u['email'] == email:
            print("\n>> Erro: Já existe um usuário com este email. <<")
            return

    novo_usuario = {
        "id": proximo_id_usuario,
        "nome": nome,
        "email": email
    }
    usuarios.append(novo_usuario)
    proximo_id_usuario += 1
    print(f"\n>> Usuário '{nome}' criado com sucesso! ID: {novo_usuario['id']} <<")

def listar_usuarios():
    """Exibe todos os usuários cadastrados."""
    print("\n--- Lista de Usuários ---")
    if not usuarios:
        print("Nenhum usuário cadastrado.")
    else:
        for usuario in usuarios:
            print(f"  ID: {usuario['id']}, Nome: {usuario['nome']}, Email: {usuario['email']}")
    print("------------------------")

# --- Funções para Gerenciar Filmes ---

def criar_filme():
    """Solicita os dados e adiciona um novo filme."""
    global proximo_id_filme
    print("\n--- Adicionar Novo Filme ---")
    titulo = input("Digite o título do filme: ")
    ano = input("Digite o ano do filme: ")
    genero = input("Digite o gênero do filme: ")

    novo_filme = {
        "id": proximo_id_filme,
        "titulo": titulo,
        "ano": int(ano),
        "genero": genero
    }
    filmes.append(novo_filme)
    proximo_id_filme += 1
    print(f"\n>> Filme '{titulo}' adicionado com sucesso! ID: {novo_filme['id']} <<")

def listar_filmes():
    """Exibe todos os filmes cadastrados."""
    print("\n--- Catálogo de Filmes ---")
    if not filmes:
        print("Nenhum filme cadastrado.")
    else:
        for filme in filmes:
            print(f"  ID: {filme['id']}, Título: {filme['titulo']}, Ano: {filme['ano']}, Gênero: {filme['genero']}")
    print("-------------------------")

# --- Funções para Favoritos e Recomendações ---

def adicionar_favorito():
    """Associa um filme como favorito de um usuário."""
    print("\n--- Adicionar Filme aos Favoritos ---")
    try:
        id_usuario = int(input("Digite o ID do usuário: "))
        id_filme = int(input("Digite o ID do filme para favoritar: "))
    except ValueError:
        print("\n>> Erro: O ID deve ser um número. <<")
        return

    # Busca pelo usuário e filme usando list comprehension e next() para segurança.
    usuario_encontrado = next((u for u in usuarios if u['id'] == id_usuario), None)
    filme_encontrado = next((f for f in filmes if f['id'] == id_filme), None)

    if not usuario_encontrado:
        print("\n>> Erro: Usuário não encontrado. <<")
        return
    if not filme_encontrado:
        print("\n>> Erro: Filme não encontrado. <<")
        return

    # Verifica se a combinação já existe.
    if any(fav['id_usuario'] == id_usuario and fav['id_filme'] == id_filme for fav in favoritos):
        print("\n>> Erro: Este filme já está na sua lista de favoritos. <<")
        return

    favoritos.append({"id_usuario": id_usuario, "id_filme": id_filme})
    print(f"\n>> Filme '{filme_encontrado['titulo']}' adicionado aos favoritos de '{usuario_encontrado['nome']}'. <<")

def recomendar_filmes():
    """Recomenda filmes com base nos gêneros favoritos de um usuário."""
    print("\n--- Obter Recomendações ---")
    try:
        id_usuario = int(input("Digite o ID do usuário para obter recomendações: "))
    except ValueError:
        print("\n>> Erro: O ID deve ser um número. <<")
        return

    if not any(u['id'] == id_usuario for u in usuarios):
        print("\n>> Erro: Usuário não encontrado. <<")
        return

    # 1. Encontrar os IDs dos filmes favoritos do usuário.
    ids_filmes_favoritos = {fav['id_filme'] for fav in favoritos if fav['id_usuario'] == id_usuario}
    
    if not ids_filmes_favoritos:
        print("\n>> O usuário não possui filmes favoritos. Adicione alguns para obter recomendações. <<")
        return

    # 2. Identificar os gêneros desses filmes favoritos.
    generos_favoritos = {filme['genero'] for filme in filmes if filme['id'] in ids_filmes_favoritos}

    # 3. Encontrar filmes com esses gêneros que o usuário ainda não favoritou.
    recomendacoes = [
        filme for filme in filmes 
        if filme['genero'] in generos_favoritos and filme['id'] not in ids_filmes_favoritos
    ]

    print("\n--- Filmes Recomendados Para Você ---")
    if not recomendacoes:
        print("Não encontramos novas recomendações no momento.")
    else:
        for filme in recomendacoes:
            print(f"  - Título: {filme['titulo']} (Gênero: {filme['genero']})")
    print("-----------------------------------")


# --- Interface Principal do Terminal (Menu) ---

def main():
    """Exibe o menu principal e gerencia a entrada do usuário."""
    while True:
        print("\n========= Sistema de Filmes - Menu Principal =========")
        print("1. Criar Usuário")
        print("2. Listar Usuários")
        print("3. Adicionar Filme")
        print("4. Listar Filmes")
        print("5. Adicionar Filme aos Favoritos")
        print("6. Obter Recomendações de Filmes")
        print("0. Sair")
        print("====================================================")

        escolha = input("Escolha uma opção: ")

        if escolha == '1':
            criar_usuario()
        elif escolha == '2':
            listar_usuarios()
        elif escolha == '3':
            criar_filme()
        elif escolha == '4':
            listar_filmes()
        elif escolha == '5':
            adicionar_favorito()
        elif escolha == '6':
            recomendar_filmes()
        elif escolha == '0':
            print("Saindo do sistema. Até logo!")
            break
        else:
            print("\n>> Opção inválida. Por favor, tente novamente. <<")

# Ponto de entrada do programa
if __name__ == "__main__":
    main()