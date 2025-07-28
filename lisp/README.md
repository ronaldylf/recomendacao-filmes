# 🧠 Sistema de Recomendação de Filmes - Common Lisp

Implementação do sistema de recomendação de filmes em **Common Lisp**, mantendo a mesma funcionalidade das versões em Prolog e Python, mas com uma abordagem funcional e estruturas de dados avançadas.

## 🚀 Como Executar

### Sistema Interativo
```bash
# Usando SBCL (Steel Bank Common Lisp)
sbcl --load lisp/main.lisp

# Usando CLISP
clisp lisp/main.lisp

# Usando CCL (Clozure Common Lisp)
ccl --load lisp/main.lisp
```

### Ver Demonstração Automática
```bash
sbcl --load lisp/demo_sistema.lisp
```

## ✨ Características da Implementação Lisp

### Estruturas de Dados
- **`defstruct`**: Estruturas de dados tipadas (similar a classes)
- **Listas Globais**: Armazenamento em memória com convenção `*earmuffs*`
- **IDs Automáticos**: Sistema de contadores para IDs únicos
- **Validação Robusta**: Funções `find`, `member`, `some` para verificações

### Funcionalidades Implementadas

#### 1. Gerenciamento de Usuários
- **`create-user`**: Cria usuários com validação de email único
- **`list-users`**: Exibe todos os usuários cadastrados

#### 2. Gerenciamento de Filmes
- **`create-movie`**: Adiciona filmes com título, ano e gênero
- **`list-movies`**: Exibe o catálogo completo de filmes

#### 3. Sistema de Favoritos
- **`add-favorite`**: Associa filmes aos favoritos dos usuários
- Validação de existência de usuário e filme
- Prevenção de favoritos duplicados

#### 4. Sistema de Recomendação
- **`get-recommendations`**: Gera recomendações baseadas em gêneros
- Algoritmo funcional usando `loop` e `mapcar`

## 🧠 Algoritmo de Recomendação

```lisp
;; 1. Coleta filmes favoritos do usuário
(let ((favorite-movie-ids
       (loop for fav in *favorites*
             when (= (favorite-user-id fav) user-id)
             collect (favorite-movie-id fav))))

;; 2. Identifica gêneros dos filmes favoritos
(let* ((favorite-movies (loop for movie in *movies*
                              when (member (movie-id movie) favorite-movie-ids)
                              collect movie))
       (favorite-genres (remove-duplicates (mapcar #'movie-genre favorite-movies))))

;; 3. Encontra filmes similares não favoritados
(let ((recommendations
       (loop for movie in *movies*
             when (and (member (movie-genre movie) favorite-genres)
                       (not (member (movie-id movie) favorite-movie-ids)))
             collect movie)))
```

## 🛠️ Tecnologias

- **Linguagem**: Common Lisp
- **Paradigma**: Funcional/Imperativo
- **Estruturas de Dados**: defstruct, Listas, LOOP
- **Interface**: Terminal interativo
- **Validação**: FIND, MEMBER, SOME

## 📊 Estrutura de Dados

### Usuários
```lisp
(defstruct user
  id
  name
  email)
```

### Filmes
```lisp
(defstruct movie
  id
  title
  year
  genre)
```

### Favoritos
```lisp
(defstruct favorite
  user-id
  movie-id)
```

## 🔍 Comparação com Outras Implementações

| Aspecto | Python | Prolog | **Lisp** |
|---------|--------|--------|----------|
| **Paradigma** | Imperativo | Lógico | **Funcional** |
| **Estruturas** | Dicionários | Fatos | **defstruct** |
| **Algoritmo** | Sets/Listas | Regras | **LOOP/MAPCAR** |
| **Validação** | Try/Except | Unificação | **FIND/MEMBER** |
| **Interface** | Input/Print | Read/Write | **READ-LINE** |
| **Complexidade** | Baixa | Média | **Média** |
| **Performance** | Rápida | Moderada | **Rápida** |

## 🎯 Vantagens da Implementação Lisp

1. **Estruturas Tipadas**: `defstruct` fornece estruturas de dados claras
2. **Programação Funcional**: Uso de `mapcar`, `remove-duplicates`, `loop`
3. **Validação Robusta**: Funções nativas para busca e verificação
4. **Flexibilidade**: Combinação de paradigmas funcional e imperativo
5. **Expressividade**: Código conciso e legível

## 📁 Arquivos

- `main.lisp`: Sistema principal em Common Lisp
- `demo_sistema.lisp`: Script de demonstração automática
- `README.md`: Esta documentação

## 🚀 Exemplo de Uso

```bash
$ sbcl --load lisp/main.lisp

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

## 🔧 Requisitos

- **SBCL** (Steel Bank Common Lisp) - Recomendado
- **CLISP** - Alternativa
- **CCL** (Clozure Common Lisp) - Alternativa

### Instalação SBCL (Ubuntu/Debian)
```bash
sudo apt-get install sbcl
```

### Instalação SBCL (macOS)
```bash
brew install sbcl
```

---

*Implementação em Common Lisp do sistema de recomendação de filmes, demonstrando o poder da programação funcional e estruturas de dados avançadas.* 