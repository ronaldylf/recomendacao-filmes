# 🎬 Sistema de Recomendação de Filmes

Este repositório contém um sistema de recomendação de filmes implementado em **múltiplas linguagens de programação**, demonstrando como o mesmo conceito pode ser aplicado em diferentes paradigmas.

## 📁 Estrutura do Projeto

```
recomendacao_filmes/
├── prolog/                          # Implementação em Prolog
│   ├── sistema_recomendacao_filmes.pl  # Sistema principal
│   ├── exemplo_uso.pl                  # Demonstração
│   ├── README.md                       # Documentação detalhada
│   └── .gitignore                      # Arquivos ignorados pelo Git
├── python/                           # Implementação em Python
│   ├── main.py                        # Sistema principal
│   ├── demo_sistema.py                # Demonstração automática
│   └── README.md                      # Documentação detalhada
├── lisp/                             # Implementação em Common Lisp
│   ├── main.lisp                      # Sistema principal
│   ├── demo_sistema.lisp              # Demonstração automática
│   └── README.md                      # Documentação detalhada
└── README.md                        # Este arquivo
```

## 🚀 Como Usar

### Implementação em Prolog
```bash
# Sistema interativo
swipl -s prolog/sistema_recomendacao_filmes.pl -g start

# Ver demonstração
swipl -s prolog/exemplo_uso.pl -t halt
```

### Implementação em Python
```bash
# Sistema interativo
python python/main.py

# Executar com Python 3 (se necessário)
python3 python/main.py

# Ver demonstração automática
python3 python/demo_sistema.py
```

### Implementação em Common Lisp
```bash
# Sistema interativo
sbcl --load lisp/main.lisp

# Ver demonstração automática
sbcl --load lisp/demo_sistema.lisp
```

## ✨ Características

- **Gerenciamento de Usuários**: Cadastro e listagem de usuários
- **Catálogo de Filmes**: Adição e visualização de filmes
- **Sistema de Favoritos**: Usuários podem marcar filmes como favoritos
- **Recomendações Inteligentes**: Sugere filmes baseado nos gêneros dos favoritos
- **Interface Interativa**: Menu amigável via terminal

## 🧠 Algoritmo de Recomendação

O sistema analisa os gêneros dos filmes favoritos do usuário e recomenda filmes similares que ainda não foram favoritados, criando uma experiência personalizada de descoberta de conteúdo.

## 🛠️ Implementações

### 🐍 Python
- **Paradigma**: Imperativo/Orientado a Objetos
- **Estruturas**: Listas, Dicionários, Sets
- **Validação**: Try/except para tratamento de erros
- **Vantagens**: Sintaxe familiar, estruturas de dados ricas

### 🔍 Prolog
- **Paradigma**: Lógico/Declarativo
- **Estruturas**: Fatos Dinâmicos, Regras
- **Validação**: Unificação e backtracking
- **Vantagens**: Lógica declarativa, inferência automática

### 🧠 Common Lisp
- **Paradigma**: Funcional/Imperativo
- **Estruturas**: defstruct, Listas, LOOP
- **Validação**: FIND, MEMBER, SOME
- **Vantagens**: Estruturas tipadas, programação funcional

## 📊 Comparação das Implementações

| Aspecto | Python | Prolog | **Lisp** |
|---------|--------|--------|----------|
| **Paradigma** | Imperativo | Lógico/Declarativo | **Funcional** |
| **Estrutura de Dados** | Listas/Dicionários | Fatos Dinâmicos | **defstruct** |
| **Algoritmo** | Loops e Sets | Regras e Backtracking | **LOOP/MAPCAR** |
| **Validação** | Try/Except | Unificação | **FIND/MEMBER** |
| **Interface** | Input/Print | Read/Write | **READ-LINE** |
| **Complexidade** | Baixa | Média | **Média** |
| **Performance** | Rápida | Moderada | **Rápida** |

## 🎯 Objetivos do Projeto

1. **Demonstrar Versatilidade**: Mesmo conceito em diferentes paradigmas
2. **Educacional**: Aprender diferentes abordagens de programação
3. **Comparativo**: Analisar vantagens e desvantagens de cada linguagem
4. **Prático**: Sistema funcional de recomendação

## 📖 Documentação Detalhada

- **[Implementação em Prolog](prolog/README.md)**: Documentação completa da versão Prolog
- **[Implementação em Python](python/README.md)**: Documentação completa da versão Python
- **[Implementação em Common Lisp](lisp/README.md)**: Documentação completa da versão Lisp



---

*Projeto educacional desenvolvido para demonstrar a versatilidade de conceitos de programação em diferentes linguagens e paradigmas.* 