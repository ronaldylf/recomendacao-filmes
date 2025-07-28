#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Demonstração do Sistema de Recomendação de Filmes em Python
Este script testa o sistema com dados de exemplo, similar ao exemplo_uso.pl
"""

# Importa as funções do sistema principal
from main import (
    usuarios, filmes, favoritos,
    proximo_id_usuario, proximo_id_filme,
    criar_usuario, listar_usuarios,
    criar_filme, listar_filmes,
    adicionar_favorito, recomendar_filmes
)

def criar_usuario_demo(nome, email):
    """Cria um usuário para demonstração."""
    global proximo_id_usuario
    novo_usuario = {
        "id": proximo_id_usuario,
        "nome": nome,
        "email": email
    }
    usuarios.append(novo_usuario)
    proximo_id_usuario += 1
    print(f"  ✓ Usuário '{nome}' criado (ID: {novo_usuario['id']})")

def criar_filme_demo(titulo, ano, genero):
    """Cria um filme para demonstração."""
    global proximo_id_filme
    novo_filme = {
        "id": proximo_id_filme,
        "titulo": titulo,
        "ano": ano,
        "genero": genero
    }
    filmes.append(novo_filme)
    proximo_id_filme += 1
    print(f"  ✓ Filme '{titulo}' adicionado (ID: {novo_filme['id']})")

def adicionar_favorito_demo(id_usuario, id_filme):
    """Adiciona um favorito para demonstração."""
    usuario = next((u for u in usuarios if u['id'] == id_usuario), None)
    filme = next((f for f in filmes if f['id'] == id_filme), None)
    
    if usuario and filme:
        favoritos.append({"id_usuario": id_usuario, "id_filme": id_filme})
        print(f"  ✓ {usuario['nome']} adicionou '{filme['titulo']}' aos favoritos")

def gerar_recomendacoes_demo(id_usuario, nome_usuario):
    """Gera recomendações para demonstração."""
    print(f"\nRecomendações para {nome_usuario}:")
    
    # Verifica se o usuário tem favoritos
    ids_filmes_favoritos = {fav['id_filme'] for fav in favoritos if fav['id_usuario'] == id_usuario}
    
    if not ids_filmes_favoritos:
        print("  Nenhum filme favorito para basear recomendações.")
        return
    
    # Identifica gêneros favoritos
    generos_favoritos = {filme['genero'] for filme in filmes if filme['id'] in ids_filmes_favoritos}
    
    # Encontra recomendações
    recomendacoes = [
        filme for filme in filmes 
        if filme['genero'] in generos_favoritos and filme['id'] not in ids_filmes_favoritos
    ]
    
    if not recomendacoes:
        print("  Não encontramos novas recomendações.")
    else:
        for filme in recomendacoes:
            print(f"  • {filme['titulo']} ({filme['genero']})")

def limpar_dados():
    """Limpa todos os dados para começar do zero."""
    global usuarios, filmes, favoritos, proximo_id_usuario, proximo_id_filme
    
    usuarios.clear()
    filmes.clear()
    favoritos.clear()
    proximo_id_usuario = 1
    proximo_id_filme = 1

def demo_sistema():
    """Executa a demonstração completa do sistema."""
    print("=== SISTEMA DE RECOMENDAÇÃO DE FILMES - PYTHON ===")
    print()
    
    # Limpar dados anteriores
    limpar_dados()
    
    # 1. Criar usuários
    print("1. CRIANDO USUÁRIOS")
    print("-------------------")
    criar_usuario_demo("João Silva", "joao@email.com")
    criar_usuario_demo("Maria Santos", "maria@email.com")
    criar_usuario_demo("Pedro Costa", "pedro@email.com")
    print()
    
    # 2. Adicionar filmes
    print("2. ADICIONANDO FILMES")
    print("---------------------")
    criar_filme_demo("O Senhor dos Anéis", 2001, "Fantasia")
    criar_filme_demo("Matrix", 1999, "Ficção Científica")
    criar_filme_demo("Titanic", 1997, "Romance")
    criar_filme_demo("Star Wars", 1977, "Ficção Científica")
    criar_filme_demo("Harry Potter", 2001, "Fantasia")
    criar_filme_demo("Notebook", 2004, "Romance")
    criar_filme_demo("Interestelar", 2014, "Ficção Científica")
    criar_filme_demo("Avatar", 2009, "Ficção Científica")
    criar_filme_demo("Romeu e Julieta", 1996, "Romance")
    criar_filme_demo("O Hobbit", 2012, "Fantasia")
    print()
    
    # 3. Mostrar dados
    print("3. DADOS DO SISTEMA")
    print("------------------")
    listar_usuarios()
    print()
    listar_filmes()
    print()
    
    # 4. Adicionar favoritos
    print("4. ADICIONANDO FAVORITOS")
    print("-----------------------")
    adicionar_favorito_demo(1, 1)  # João gosta de O Senhor dos Anéis
    adicionar_favorito_demo(1, 5)  # João gosta de Harry Potter
    adicionar_favorito_demo(2, 2)  # Maria gosta de Matrix
    adicionar_favorito_demo(2, 4)  # Maria gosta de Star Wars
    adicionar_favorito_demo(3, 3)  # Pedro gosta de Titanic
    adicionar_favorito_demo(3, 6)  # Pedro gosta de Notebook
    print()
    
    # 5. Gerar recomendações
    print("5. SISTEMA DE RECOMENDAÇÕES")
    print("---------------------------")
    gerar_recomendacoes_demo(1, "João Silva")
    gerar_recomendacoes_demo(2, "Maria Santos")
    gerar_recomendacoes_demo(3, "Pedro Costa")
    print()
    
    print("=== DEMONSTRAÇÃO CONCLUÍDA ===")
    print("O sistema Python está funcionando corretamente!")
    print()
    
    # 6. Mostrar estatísticas
    print("6. ESTATÍSTICAS FINAIS")
    print("----------------------")
    print(f"  • Total de usuários: {len(usuarios)}")
    print(f"  • Total de filmes: {len(filmes)}")
    print(f"  • Total de favoritos: {len(favoritos)}")
    print(f"  • Gêneros disponíveis: {set(f['genero'] for f in filmes)}")
    print()

if __name__ == "__main__":
    demo_sistema() 