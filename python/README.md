# 🐍 Sistema de Recomendação de Filmes - Python

Implementação do sistema de recomendação de filmes em **Python**, mantendo a mesma funcionalidade da versão em Prolog mas com uma abordagem imperativa e orientada a objetos.

## 🚀 Como Executar

### Sistema Interativo
```bash
python python/main.py
```

### Executar com Python 3 (se necessário)
```bash
python3 python/main.py
```

### Ver Demonstração Automática
```bash
python3 python/demo_sistema.py
```

## ✨ Características da Implementação Python

### Estrutura de Dados
- **Listas de Dicionários**: Simula um banco de dados em memória
- **IDs Automáticos**: Sistema de contadores para IDs únicos
- **Validação de Dados**: Verificação de entradas e existência de registros

### Funcionalidades Implementadas

#### 1. Gerenciamento de Usuários
- **`criar_usuario()`**: Cria usuários com validação de email único
- **`listar_usuarios()`**: Exibe todos os usuários cadastrados

#### 2. Gerenciamento de Filmes
- **`criar_filme()`**: Adiciona filmes com título, ano e gênero
- **`listar_filmes()`**: Exibe o catálogo completo de filmes

#### 3. Sistema de Favoritos
- **`adicionar_favorito()`**: Associa filmes aos favoritos dos usuários
- Validação de existência de usuário e filme
- Prevenção de favoritos duplicados

#### 4. Sistema de Recomendação
- **`recomendar_filmes()`**: Gera recomendações baseadas em gêneros
- Algoritmo similar ao Prolog mas implementado em Python

## 🧠 Algoritmo de Recomendação

```python
# 1. Encontrar filmes favoritos do usuário
ids_filmes_favoritos = {fav['id_filme'] for fav in favoritos if fav['id_usuario'] == id_usuario}

# 2. Identificar gêneros dos filmes favoritos
generos_favoritos = {filme['genero'] for filme in filmes if filme['id'] in ids_filmes_favoritos}

# 3. Encontrar filmes similares não favoritados
recomendacoes = [
    filme for filme in filmes 
    if filme['genero'] in generos_favoritos and filme['id'] not in ids_filmes_favoritos
]
```

## 🛠️ Tecnologias

- **Linguagem**: Python 3
- **Paradigma**: Imperativo/Orientado a Objetos
- **Estruturas de Dados**: Listas, Dicionários, Sets
- **Interface**: Terminal interativo
- **Validação**: Try/except para tratamento de erros

## 📊 Estrutura de Dados

### Usuários
```python
{
    "id": 1,
    "nome": "João Silva",
    "email": "joao@email.com"
}
```

### Filmes
```python
{
    "id": 1,
    "titulo": "Matrix",
    "ano": 1999,
    "genero": "Ficção Científica"
}
```

### Favoritos
```python
{
    "id_usuario": 1,
    "id_filme": 2
}
```

## 🔍 Comparação com Prolog

| Aspecto | Python | Prolog |
|---------|--------|--------|
| **Paradigma** | Imperativo | Lógico/Declarativo |
| **Estrutura de Dados** | Listas/Dicionários | Fatos Dinâmicos |
| **Algoritmo** | Loops e Sets | Regras e Backtracking |
| **Validação** | Try/Except | Unificação |
| **Interface** | Input/Print | Read/Write |

## 🎯 Vantagens da Implementação Python

1. **Sintaxe Familiar**: Mais acessível para programadores iniciantes
2. **Estruturas de Dados Ricas**: Listas, dicionários, sets nativos
3. **Tratamento de Erros**: Try/except para validação robusta
4. **Performance**: Execução mais rápida para operações simples
5. **Extensibilidade**: Fácil adição de novas funcionalidades

## 📁 Arquivos

- `main.py`: Sistema principal em Python
- `demo_sistema.py`: Script de demonstração automática
- `README.md`: Esta documentação

## 🚀 Exemplo de Uso

### Sistema Interativo
```bash
$ python python/main.py

========= Sistema de Filmes - Menu Principal =========
1. Criar Usuário
2. Listar Usuários
3. Adicionar Filme
4. Listar Filmes
5. Adicionar Filme aos Favoritos
6. Obter Recomendações de Filmes
0. Sair
====================================================
Escolha uma opção: 1
```

### Demonstração Automática
O script `demo_sistema.py` executa uma demonstração completa do sistema:

```bash
$ python3 python/demo_sistema.py

=== SISTEMA DE RECOMENDAÇÃO DE FILMES - PYTHON ===

1. CRIANDO USUÁRIOS
-------------------
  ✓ Usuário 'João Silva' criado (ID: 1)
  ✓ Usuário 'Maria Santos' criado (ID: 2)
  ✓ Usuário 'Pedro Costa' criado (ID: 3)

2. ADICIONANDO FILMES
---------------------
  ✓ Filme 'O Senhor dos Anéis' adicionado (ID: 1)
  ✓ Filme 'Matrix' adicionado (ID: 2)
  ...

5. SISTEMA DE RECOMENDAÇÕES
---------------------------
Recomendações para João Silva:
  • O Hobbit (Fantasia)

Recomendações para Maria Santos:
  • Interestelar (Ficção Científica)
  • Avatar (Ficção Científica)
```

---

*Implementação em Python do sistema de recomendação de filmes, demonstrando a versatilidade do conceito em diferentes paradigmas de programação.* 