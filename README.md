# Programação Funcional em Haskell

> Exercícios, listas, avaliações e exemplos de Haskell feitos na graduação (UFC Quixadá): listas e recursão, tipos algébricos, Maybe, mônadas e IO, pilhas e filas, QuickCheck e jogos com Gloss.

![status](https://img.shields.io/badge/status-concluído-success) ![haskell](https://img.shields.io/badge/Haskell-GHC-blueviolet)

## Sobre
Material de duas passagens pela programação funcional: as listas, revisões e avaliações (AP1 a AP3 e uma prova) de 2014, e o trabalho de Linguagens de Programação com as questões 2 a 8 (produto de lista, pares, união, intersecção, palíndromo, primo, ordenação). Reunido no GitHub em 2020 e reorganizado por tema em 2026. Os arquivos foram só movidos e renomeados (histórico preservado com `git mv`); versões antigas que eram subconjunto de outra mais completa foram removidas.

A pasta `material-pedro-vasconcelos/` guarda programas de apoio do professor Pedro Vasconcelos (FCUP, 2010-2014), usados como exemplo em aula: cifra de César, jogo da vida, cadeias de Markov, verificador de tautologias, pilha como tipo abstrato e os jogos em Gloss.

## Estrutura de pastas
```text
trabalho-linguagens-de-programacao/   questões 2 a 8 do trabalho (uma função por arquivo)
listas/                               listas 2 a 6, lista de IO, mônadas, tipos de dados, raciocínio sobre programas (com QuickCheck), ordenação
avaliacoes/                           AP1 (duas versões), revisões de AP1 a AP3, AP3 e sua questão 6, prova de 2014 (questões 1 a 3)
exercicios-basicos/                   fatorial, maior/menor de três, mdc, primos, triangular; enunciados.txt
exercicios-listas/                    binário, concatenação, inserção, inversão, length com foldl/foldr, mínimo, soma de dígitos, união; enunciados.txt
exemplos-compreensao-de-listas/       funções de uma linha com compreensão de listas (count, frequência, pertence, único...)
exemplos-io/                          putChar/putStr, leitura, do-notation, entrada e saída, chamada de funções puras
estruturas-de-dados/pilha-e-fila/     módulos Pilha, PilhaVet, Queue e QueueFresca com programas de teste
jogos/                                xadrez-rainhas.hs: posições que se atacam e permutações
material-pedro-vasconcelos/           cifra, life, markov, tautologia, stack, gloss-asteroids, gloss-balls, gloss-events
```

## Stack
- Haskell (GHC/GHCi)
- `QuickCheck` para `listas/lista-raciocinio-sobre-programas-quickcheck.hs`
- `gloss` e `random` para os exemplos gráficos em `material-pedro-vasconcelos/`

## Como executar
```bash
ghci trabalho-linguagens-de-programacao/questao-6.hs
# módulos de pilha e fila: entrar na pasta para o import funcionar
cd estruturas-de-dados/pilha-e-fila && ghci pilha2.hs
# compilar um exemplo com Gloss:
ghc material-pedro-vasconcelos/gloss-balls.hs -o balls && ./balls
```

## Status
Concluído. Material acadêmico; não recebe manutenção.

## Autor
Ronildo Silva · ronildo.comp@gmail.com
