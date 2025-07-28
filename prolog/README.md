# Sistema de Recomendação de Filmes em Prolog

Este é um sistema de recomendação de filmes implementado em SWI-Prolog que permite gerenciar usuários, filmes e gerar recomendações baseadas nos gêneros favoritos dos usuários.

## Funcionalidades

- **Gerenciamento de Usuários**: Criar e listar usuários
- **Gerenciamento de Filmes**: Adicionar e listar filmes com título, ano e gênero
- **Sistema de Favoritos**: Adicionar filmes aos favoritos dos usuários
- **Sistema de Recomendação**: Gerar recomendações baseadas nos gêneros dos filmes favoritos

## Como Executar

### 1. Sistema Interativo
```bash
swipl -s prolog/sistema_recomendacao_filmes.pl -g start
```

### 2. Exemplo de Uso
```bash
swipl -s prolog/exemplo_uso.pl -t halt
```

## Estrutura do Sistema

### Fatos Dinâmicos
- `user(Id, Name, Email)`: Armazena informações dos usuários
- `movie(Id, Title, Year, Genre)`: Armazena informações dos filmes
- `favorite(UserId, MovieId)`: Relaciona usuários com seus filmes favoritos
- `id_counter(Type, NextId)`: Gerencia IDs únicos

### Predicados Principais

#### Gerenciamento de Usuários
- `create_user/0`: Cria um novo usuário
- `list_users/0`: Lista todos os usuários

#### Gerenciamento de Filmes
- `create_movie/0`: Adiciona um novo filme
- `list_movies/0`: Lista todos os filmes

#### Sistema de Favoritos
- `add_favorite/0`: Adiciona um filme aos favoritos de um usuário

#### Sistema de Recomendação
- `is_recommendation_for/4`: Regra que define quando um filme é recomendado
- `get_recommendations/0`: Gera recomendações para um usuário

## Algoritmo de Recomendação

O sistema usa um algoritmo baseado em gêneros:

1. **Análise de Preferências**: Identifica os gêneros dos filmes favoritos do usuário
2. **Filtragem por Gênero**: Encontra filmes do mesmo gênero que não estão nos favoritos
3. **Geração de Recomendações**: Retorna filmes que atendem aos critérios

### Exemplo de Funcionamento

Se um usuário tem como favoritos:
- "Matrix" (Ficção Científica)
- "Star Wars" (Ficção Científica)

O sistema recomendará:
- "Interestelar" (Ficção Científica)
- "Avatar" (Ficção Científica)

## Arquivos do Projeto

- `prolog/sistema_recomendacao_filmes.pl`: Sistema principal
- `prolog/exemplo_uso.pl`: Exemplo de uso e demonstração
- `prolog/README.md`: Este arquivo de documentação

## Requisitos

- SWI-Prolog 8.4.2 ou superior
- Sistema Linux/Unix

## Exemplo de Uso

1. Execute o sistema: `swipl -s sistema_recomendacao_filmes.pl -g start`
2. Crie um usuário (opção 1)
3. Adicione alguns filmes (opção 3)
4. Adicione filmes aos favoritos (opção 5)
5. Obtenha recomendações (opção 6)

## Características Técnicas

- **Linguagem**: Prolog (SWI-Prolog)
- **Paradigma**: Lógico/Declarativo
- **Base de Dados**: Fatos dinâmicos em memória
- **Interface**: Terminal interativo
- **Algoritmo**: Baseado em filtragem por gênero

## Limitações

- Dados são perdidos ao encerrar o programa (não há persistência)
- Recomendações baseadas apenas em gênero
- Interface apenas via terminal
- Não há sistema de avaliações ou scores

## Possíveis Melhorias

- Persistência de dados em arquivo
- Sistema de avaliações (1-5 estrelas)
- Recomendações baseadas em múltiplos critérios
- Interface gráfica
- Sistema de tags para filmes
- Análise de similaridade mais avançada 