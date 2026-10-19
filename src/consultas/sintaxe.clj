(ns consultas.sintaxe
  "Texto da consulta -> AST.

  A analise e recursiva e sem estado: cada funcao de analise recebe a
  sequencia de tokens que ainda falta ler e devolve o par
  [no-reconhecido tokens-que-sobraram]. Nenhuma variavel guarda a posicao
  de leitura; ela e simplesmente o resto da sequencia.

  Erros de sintaxe sao sinalizados com ex-info."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Erros
;; ---------------------------------------------------------------------------

(defn- falhar
  "Lanca um erro de sintaxe com a mensagem dada."
  [mensagem]
  (throw (ex-info mensagem {:tipo :sintaxe})))

(defn- descrever
  "Como um token aparece numa mensagem de erro."
  [token]
  (if (nil? token) "fim da consulta" (str "'" token "'")))

;; ---------------------------------------------------------------------------
;; Tokens
;; ---------------------------------------------------------------------------

(def ^:private padrao-token
  ;; texto entre aspas (com ou sem a aspa de fechamento), operadores de dois
  ;; caracteres, simbolos de um caractere e palavras.
  #"\"[^\"]*\"?|<=|>=|!=|[=<>()|,!]|[^\s=<>!()|,\"]+")

(defn tokenizar
  "Quebra o texto da consulta numa sequencia de tokens (strings)."
  [texto]
  (let [tokens (re-seq padrao-token texto)]
    (if-let [ruim (first (filter #(and (str/starts-with? % "\"")
                                       (or (= 1 (count %))
                                           (not (str/ends-with? % "\""))))
                                 tokens))]
      (falhar (str "texto sem aspas de fechamento: " ruim))
      (vec tokens))))

;; ---------------------------------------------------------------------------
;; Pecas basicas: campo, numero, literal, palavra esperada
;; ---------------------------------------------------------------------------

(def ^:private palavras-reservadas
  #{"onde" "ordenar" "por" "desc" "limitar" "selecionar" "contar" "soma"
    "media" "agrupar" "com" "e" "ou" "nao"})

(defn- nome-de-campo? [token]
  (and (string? token)
       (some? (re-matches #"[\p{L}_][\p{L}\p{N}_\-]*" token))
       (not (contains? palavras-reservadas token))))

(defn- esperar
  "Consome a palavra `palavra` do inicio dos tokens; devolve o resto."
  [palavra mensagem tokens]
  (if (= palavra (first tokens))
    (rest tokens)
    (falhar (str mensagem ", encontrado " (descrever (first tokens))))))

(defn- analisar-campo
  "campo -> [:palavra-chave resto]"
  [tokens]
  (let [token (first tokens)]
    (if (nome-de-campo? token)
      [(keyword token) (rest tokens)]
      (falhar (str "nome de campo esperado, encontrado " (descrever token))))))

(defn- analisar-campos
  "campo [, campo]* -> [[:a :b ...] resto]"
  [tokens]
  (let [[campo resto] (analisar-campo tokens)]
    (if (= "," (first resto))
      (let [[outros resto2] (analisar-campos (rest resto))]
        [(into [campo] outros) resto2])
      [[campo] resto])))

(defn- analisar-inteiro
  "inteiro nao negativo -> [n resto]"
  [tokens]
  (let [token (first tokens)]
    (if (and (string? token) (re-matches #"\d+" token))
      [(Long/parseLong token) (rest tokens)]
      (falhar (str "numero esperado, encontrado " (descrever token))))))

(defn- literal
  "Converte um token em literal: inteiro, decimal ou texto."
  [token]
  (cond
    (nil? token)                          (falhar "falta o literal da comparacao")
    (str/starts-with? token "\"")         (subs token 1 (dec (count token)))
    (re-matches #"-?\d+" token)           (Long/parseLong token)
    (re-matches #"-?\d+\.\d+" token)      (Double/parseDouble token)
    :else (falhar (str "literal esperado (numero ou \"texto\"), encontrado "
                       (descrever token)))))

;; ---------------------------------------------------------------------------
;; Expressoes: ou < e < nao < ( ) / comparacao
;;
;;   ou     := e ("ou" e)*
;;   e      := nao ("e" nao)*
;;   nao    := "nao" nao | atomo
;;   atomo  := "(" ou ")" | campo op literal
;; ---------------------------------------------------------------------------

(def ^:private operadores
  {"=" :=, "!=" :!=, "<" :<, "<=" :<=, ">" :>, ">=" :>=})

(declare analisar-ou)

(defn- analisar-comparacao [tokens]
  (let [[campo resto] (analisar-campo tokens)
        token-op      (first resto)
        op            (get operadores token-op)]
    (cond
      (nil? token-op) (falhar "falta o operador da comparacao")
      (nil? op)       (falhar (str "operador desconhecido: " token-op))
      :else           [[op campo (literal (second resto))] (drop 2 resto)])))

(defn- analisar-atomo [tokens]
  (if (= "(" (first tokens))
    (let [[expr resto] (analisar-ou (rest tokens))]
      (if (= ")" (first resto))
        [expr (rest resto)]
        (falhar (str "parentese sem fechar: esperado ')', encontrado "
                     (descrever (first resto))))))
    (analisar-comparacao tokens)))

(defn- analisar-nao [tokens]
  (if (= "nao" (first tokens))
    (let [[expr resto] (analisar-nao (rest tokens))]
      [[:nao expr] resto])
    (analisar-atomo tokens)))

(defn- associar-a-esquerda
  "Le `operando (palavra operando)*` e monta [no esq dir], associando a
  esquerda. `proximo` e a funcao que analisa cada operando."
  [palavra no proximo tokens]
  (let [[primeiro resto] (proximo tokens)]
    (loop [esq primeiro
           resto resto]
      (if (= palavra (first resto))
        (let [[dir resto2] (proximo (rest resto))]
          (recur [no esq dir] resto2))
        [esq resto]))))

(defn- analisar-e [tokens]
  (associar-a-esquerda "e" :e analisar-nao tokens))

(defn- analisar-ou [tokens]
  (associar-a-esquerda "ou" :ou analisar-e tokens))

;; ---------------------------------------------------------------------------
;; Elementos da consulta
;; ---------------------------------------------------------------------------

(defn- analisar-agregacao
  "contar | soma campo | media campo"
  [tokens]
  (case (first tokens)
    "contar" [[:contar] (rest tokens)]
    "soma"   (let [[campo resto] (analisar-campo (rest tokens))] [[:soma campo] resto])
    "media"  (let [[campo resto] (analisar-campo (rest tokens))] [[:media campo] resto])
    (falhar (str "agregacao esperada (contar, soma ou media), encontrado "
                 (descrever (first tokens))))))

(def ^:private elementos
  "Tabela palavra inicial -> funcao que analisa o elemento a partir dos
  tokens que vem depois da palavra. A tabela e dado; a busca de uma palavra
  que nao esta nela e um estagio desconhecido."
  {"onde"
   (fn [tokens]
     (let [[expr resto] (analisar-ou tokens)]
       [[:onde expr] resto]))

   "ordenar"
   (fn [tokens]
     (let [[campo resto] (analisar-campo (esperar "por" "falta 'por' depois de 'ordenar'" tokens))]
       (if (= "desc" (first resto))
         [[:ordenar-por campo :desc] (rest resto)]
         [[:ordenar-por campo :asc] resto])))

   "limitar"
   (fn [tokens]
     (let [[n resto] (analisar-inteiro tokens)]
       [[:limitar n] resto]))

   "selecionar"
   (fn [tokens]
     (let [[campos resto] (analisar-campos tokens)]
       [[:selecionar campos] resto]))

   "contar" (fn [tokens] [[:contar] tokens])

   "soma"
   (fn [tokens]
     (let [[campo resto] (analisar-campo tokens)]
       [[:soma campo] resto]))

   "media"
   (fn [tokens]
     (let [[campo resto] (analisar-campo tokens)]
       [[:media campo] resto]))

   "agrupar"
   (fn [tokens]
     (let [[campo resto] (analisar-campo (esperar "por" "falta 'por' depois de 'agrupar'" tokens))
           [agg resto2]  (analisar-agregacao
                          (esperar "com" "falta 'com <agregacao>' depois de 'agrupar por <campo>'" resto))]
       [[:agrupar-por campo agg] resto2]))})

(def ^:private elementos-finais
  #{:selecionar :contar :soma :media :agrupar-por})

(defn- analisar-elemento [tokens]
  (let [palavra (first tokens)]
    (cond
      (or (nil? palavra) (= "|" palavra))
      (falhar "estagio vazio")

      (contains? elementos palavra)
      ((get elementos palavra) (rest tokens))

      :else
      (falhar (str "estagio desconhecido: " palavra)))))

(defn- analisar-elementos
  "elemento (| elemento)* -> [[no ...] resto]"
  [tokens]
  (let [[no resto] (analisar-elemento tokens)]
    (cond
      (empty? resto)
      [[no] resto]

      (= "|" (first resto))
      (if (contains? elementos-finais (first no))
        (falhar (str (name (first no)) " tem que ser o ultimo elemento da consulta"))
        (let [[outros resto2] (analisar-elementos (rest resto))]
          [(into [no] outros) resto2]))

      :else
      (falhar (str "esperado '|' ou fim da consulta, encontrado "
                   (descrever (first resto)))))))

(defn analisar
  "Texto de uma consulta (sem a palavra QUERY) -> AST.

  A AST e {:estagios [elemento ...]}; ver o README para a forma de cada
  elemento."
  [texto]
  (let [tokens (tokenizar texto)]
    (if (empty? tokens)
      {:estagios []}
      {:estagios (first (analisar-elementos tokens))})))
