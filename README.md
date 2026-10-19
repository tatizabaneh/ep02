EP02 — Query builder funcional em Clojure
Integrantes
Tatiana Maturano Zabaneh
Thiago Soares Lima
Motor de consultas sobre registros carregados de um CSV. O texto da consulta vira dado (uma AST de mapas, vetores e palavras-chave), a AST é conferida contra o esquema do arquivo, depois compilada em funções e essas funções são compostas numa só, aplicada aos registros. Não há estado mutável em nenhum ponto do caminho.
Como executar
Requisitos: Java (11 ou superior) e o Clojure CLI. O projeto não depende de nenhuma biblioteca além do próprio Clojure.
```bash
clojure -M:run dados/filmes.csv
```
```
> QUERY onde genero = "drama" e nota >= 8 | ordenar por nota desc | selecionar titulo, nota
Cidade de Deus; 8.60
Ainda Estou Aqui; 8.20
Central do Brasil; 8.00
> QUERY agrupar por genero com contar
acao; 1
comedia; 1
drama; 5
suspense; 1
> EXIT
```
Comandos: `QUERY <consulta>` e `EXIT`. O fim da entrada (por exemplo, num pipe) equivale a `EXIT`. Qualquer erro — comando desconhecido, sintaxe, campo inexistente, tipos incompatíveis, linha em branco — é reportado numa linha que começa com `ERRO:`, e o prompt volta.
Testes do builder (opcional):
```bash
clojure -M:test
```
A forma da AST
Uma consulta é um mapa com uma única chave, `:estagios`, cujo valor é um vetor de elementos na ordem em que aparecem no texto. O elemento final (`:selecionar`, um terminal ou `:agrupar-por`), quando existe, é o último do vetor.
```clojure
{:estagios \[elemento ...]}
```
Cada elemento é um vetor cujo primeiro item é o tipo do nó (uma palavra-chave):
Texto	Elemento
`onde <expr>`	`\[:onde expr]`
`ordenar por <campo>`	`\[:ordenar-por :campo :asc]`
`ordenar por <campo> desc`	`\[:ordenar-por :campo :desc]`
`limitar <n>`	`\[:limitar n]`
`selecionar a, b`	`\[:selecionar \[:a :b]]`
`contar`	`\[:contar]`
`soma <campo>`	`\[:soma :campo]`
`media <campo>`	`\[:media :campo]`
`agrupar por <campo> com <agregacao>`	`\[:agrupar-por :campo agregacao]`, com `agregacao` sendo `\[:contar]`, `\[:soma :campo]` ou `\[:media :campo]`
Expressões do `onde` também são vetores com o tipo do nó na frente:
Texto	Expressão
`a e b`	`\[:e a b]`
`a ou b`	`\[:ou a b]`
`nao a`	`\[:nao a]`
`campo <op> literal`	`\[op :campo literal]`, com `op` em `:=` `:!=` `:<` `:<=` `:>` `:>=`
Literais são `Long` (inteiro), `Double` (decimal) ou `String` (texto). Parênteses não aparecem na AST: eles só mudam a forma da árvore. `e` e `ou` associam à esquerda (`a e b e c` → `\[:e \[:e a b] c]`).
Exemplo — o texto
```
onde (genero = "acao" ou genero = "suspense") e nao ano < 2010 | ordenar por nota desc | limitar 2 | selecionar titulo, nota
```
vira
```clojure
{:estagios \[\[:onde \[:e \[:ou \[:= :genero "acao"] \[:= :genero "suspense"]]
                       \[:nao \[:< :ano 2010]]]]
            \[:ordenar-por :nota :desc]
            \[:limitar 2]
            \[:selecionar \[:titulo :nota]]]}
```
e `agrupar por genero com media duracao` vira `{:estagios \[\[:agrupar-por :genero \[:media :duracao]]]}`.
A AST é dado puro: pode ser impressa, comparada com `=` e escrita à mão. O builder produz exatamente o mesmo dado que o analisador:
```clojure
(require '\[consultas.core :refer :all])
(def dramas (onde (consulta) \[:= :genero "drama"]))
(= (limitar dramas 2) (analisar "onde genero = \\"drama\\" | limitar 2"))  ;; => true
```
Nó desconhecido. Todo despacho sobre o tipo de um nó é um `case` sem ramo padrão. Uma AST com um nó que não existe na linguagem, como `{:estagios \[\[:voar 3]]}`, faz `executar` falhar com `IllegalArgumentException: No matching clause: :voar` — antes de qualquer registro ser processado.
O que `executar` devolve
`(executar c dados)`, com `dados` = `{:esquema \[\[campo tipo] ...] :registros seq}`:
consulta sem elemento final ou com `selecionar`: a sequência de registros (mapas; com `selecionar`, só os campos escolhidos);
`contar`, `soma`, `media`: o número;
`agrupar por`: um mapa ordenado (`sorted-map`) chave → valor agregado;
consulta inválida: `{:erros \["motivo" ...]}`, sem processar nenhum registro.
Os namespaces
Namespace	Papel
`consultas.main`	Entrada do programa e único com I/O: lê o arquivo, e o laço do terminal (`loop`/`recur`) lê cada linha e imprime a resposta. Único namespace com `doseq`.
`consultas.sintaxe`	Texto → AST. Tokenizador e analisador recursivo descendente sem estado: cada função recebe os tokens e devolve `\[no tokens-que-sobraram]`. Erros de sintaxe com `ex-info`.
`consultas.compilador`	Conferência da AST contra o esquema (campos, tipos, soma/media numéricas, elemento final no fim) e compilação AST → funções, com `case` sem ramo padrão. Cada `onde` vira um predicado montado com `partial`; cada estágio, uma função de sequência em sequência; a consulta é a composição delas com `reduce` + `comp` a partir de `identity`. `contar`, `soma` e `media` são dobras com `reduce`, reaproveitadas dentro de cada grupo do `agrupar por`.
`consultas.core`	Leitura do CSV (texto → `{:esquema :registros}`), o query builder (`consulta`, `onde`, `selecionar`, `ordenar-por`, `limitar`, e também `contar`, `soma`, `media`, `agrupar-por`), `analisar`, `executar`, e a resposta a um comando como dado (`responder` devolve as linhas a imprimir).
Decisões
Nenhum nome de campo no código: o esquema vem da primeira linha do CSV; o motor só conhece os tipos `inteiro`, `decimal` e `texto`.
Comparação numérica: inteiro e decimal se comparam por `compare` (`(compare 8 8.0)` é `0`), então `nota = 8` encontra `8.0`.
Ordenação estável: `sort-by` com `compare` (ou o comparador invertido para `desc`) mantém a ordem de chegada dos empatados.
Formatação: decimais com duas casas e ponto, fixando `Locale/ROOT`, em qualquer máquina.
Erros: a conferência reúne todos os problemas da consulta e o terminal os imprime numa única linha `ERRO:`, separados por `; `.
