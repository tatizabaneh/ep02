(ns consultas.compilador
  "Duas passagens sobre a AST, ambas antes de qualquer registro ser lido:

  1. `conferir`: confronta a AST com o esquema e devolve a lista de erros
     (campo inexistente, tipos incompativeis, soma/media sobre texto,
     elemento final fora do fim...).
  2. `compilar`: transforma a AST, ja conferida, em funcoes. Cada `onde`
     vira um predicado; cada estagio vira uma funcao de sequencia de
     registros em sequencia de registros; o elemento final vira a ultima
     funcao. A consulta e a composicao dessas funcoes.

  Todo despacho sobre o tipo de um no e um `case` SEM ramo padrao: um no
  que nao existe na linguagem faz o `case` falhar com
  `IllegalArgumentException: No matching clause: <no>`, nomeando o no.")

;; ===========================================================================
;; Classificacao dos nos
;; ===========================================================================

(defn final?
  "O elemento e um dos que encerram a consulta?"
  [tipo]
  (case tipo
    (:onde :ordenar-por :limitar)                    false
    (:selecionar :contar :soma :media :agrupar-por)  true))

(defn- tipo-do-literal [v]
  (cond
    (string? v)  :texto
    (integer? v) :inteiro
    (number? v)  :decimal
    :else        nil))

(def ^:private numerico? #{:inteiro :decimal})

(defn- compativeis? [tipo-campo tipo-literal]
  (or (= tipo-campo tipo-literal)
      (and (numerico? tipo-campo) (numerico? tipo-literal))))

;; ===========================================================================
;; 1. Conferencia contra o esquema
;;
;; `tipos` e o esquema como mapa campo -> tipo. Cada funcao devolve uma
;; sequencia (possivelmente vazia) de mensagens de erro.
;; ===========================================================================

(defn- conferir-campo [tipos campo]
  (cond
    (not (keyword? campo))        [(str "campo invalido: " (pr-str campo))]
    (contains? tipos campo)       []
    :else                         [(str "campo inexistente: " (name campo))]))

(defn- conferir-campo-numerico [tipos operacao campo]
  (let [erros (conferir-campo tipos campo)]
    (cond
      (seq erros)                       erros
      (numerico? (get tipos campo))     []
      :else [(str operacao " exige campo numerico, mas " (name campo)
                  " e " (name (get tipos campo)))])))

(defn- conferir-comparacao [tipos campo valor]
  (let [erros (conferir-campo tipos campo)
        tipo-lit (tipo-do-literal valor)]
    (cond
      (seq erros)     erros
      (nil? tipo-lit) [(str "literal invalido no campo " (name campo) ": " (pr-str valor))]
      (compativeis? (get tipos campo) tipo-lit) []
      :else [(str "comparacao entre " (name (get tipos campo)) " e "
                  (name tipo-lit) " no campo " (name campo))])))

(defn- conferir-expr [tipos expr]
  (if-not (and (vector? expr) (seq expr))
    [(str "expressao invalida: " (pr-str expr))]
    (let [[op & args] expr]
      (case op
        (:e :ou)  (if (= 2 (count args))
                    (mapcat (partial conferir-expr tipos) args)
                    [(str (name op) " exige duas expressoes")])
        :nao      (if (= 1 (count args))
                    (conferir-expr tipos (first args))
                    ["nao exige uma expressao"])
        (:= :!= :< :<= :> :>=)
                  (if (= 2 (count args))
                    (conferir-comparacao tipos (first args) (second args))
                    [(str "comparacao " (name op) " exige campo e literal")])))))

(defn- conferir-agregacao [tipos [tipo campo]]
  (case tipo
    :contar []
    :soma   (conferir-campo-numerico tipos "soma" campo)
    :media  (conferir-campo-numerico tipos "media" campo)))

(defn- conferir-elemento [tipos elemento]
  (if-not (and (vector? elemento) (seq elemento))
    [(str "elemento invalido: " (pr-str elemento))]
    (let [[tipo a b] elemento]
      (case tipo
        :onde        (conferir-expr tipos a)
        :ordenar-por (concat (conferir-campo tipos a)
                             (when-not (contains? #{:asc :desc} b)
                               [(str "ordem invalida: " (pr-str b) " (use :asc ou :desc)")]))
        :limitar     (when-not (and (integer? a) (not (neg? a)))
                       [(str "limitar exige inteiro nao negativo: " (pr-str a))])
        :selecionar  (if (and (vector? a) (seq a))
                       (mapcat (partial conferir-campo tipos) a)
                       ["selecionar exige ao menos um campo"])
        (:contar :soma :media) (conferir-agregacao tipos elemento)
        :agrupar-por (concat (conferir-campo tipos a)
                             (if (and (vector? b) (seq b))
                               (conferir-agregacao tipos b)
                               ["agrupar por exige uma agregacao"]))))))

(defn- conferir-ordem
  "Elemento final so pode aparecer na ultima posicao."
  [estagios]
  (->> (butlast estagios)
       (filter (comp final? first))
       (map #(str (name (first %)) " tem que ser o ultimo elemento da consulta"))))

(defn conferir
  "Confere a consulta `ast` contra o `esquema` ([[campo tipo] ...]).
  Devolve um vetor de mensagens de erro; vazio quando a consulta e valida.
  Um no desconhecido faz o `case` falhar, nomeando o no."
  [esquema ast]
  (let [estagios (:estagios ast)]
    (if-not (and (map? ast) (vector? estagios))
      [(str "consulta invalida: esperado {:estagios [...]}, recebido " (pr-str ast))]
      (let [tipos (into {} esquema)]
        (vec (concat (mapcat (partial conferir-elemento tipos) estagios)
                     (conferir-ordem estagios)))))))

;; ===========================================================================
;; 2. Compilacao: AST -> funcoes
;; ===========================================================================

;; --- predicados do `onde` --------------------------------------------------

(defn- testar
  "Compara o valor do `campo` no registro com o literal e aplica `teste`
  ao resultado de `compare` (negativo, zero ou positivo). Inteiro e
  decimal se comparam pelo valor: (compare 8 8.0) e 0."
  [teste campo literal registro]
  (teste (compare (get registro campo) literal)))

(defn- teste-do-operador [op]
  (case op
    :=  zero?
    :!= (complement zero?)
    :<  neg?
    :<= (complement pos?)
    :>  pos?
    :>= (complement neg?)))

(defn compilar-expr
  "Expressao -> predicado sobre um registro."
  [[op a b]]
  (case op
    :e   (every-pred (compilar-expr a) (compilar-expr b))
    :ou  (some-fn (compilar-expr a) (compilar-expr b))
    :nao (complement (compilar-expr a))
    (:= :!= :< :<= :> :>=)
    ;; aplicacao parcial: o predicado ja fixou teste, campo e literal
    (partial testar (teste-do-operador op) a b)))

;; --- agregacoes: dobras com reduce ----------------------------------------

(defn- contar [registros]
  (reduce (fn [n _] (inc n)) 0 registros))

(defn- somar [campo zero registros]
  (reduce (fn [total registro] (+ total (get registro campo))) zero registros))

(defn- media [campo registros]
  (let [[total n] (reduce (fn [[total n] registro]
                            [(+ total (get registro campo)) (inc n)])
                          [0 0]
                          registros)]
    (if (zero? n) 0.0 (/ (double total) n))))

(defn- zero-do-tipo [tipo]
  (case tipo
    :inteiro 0
    :decimal 0.0))

(defn compilar-agregacao
  "Agregacao -> funcao de uma sequencia de registros num valor. Usada pelos
  terminais e, sem alteracao, dentro de cada grupo do `agrupar por`."
  [tipos [tipo campo]]
  (case tipo
    :contar contar
    :soma   (partial somar campo (zero-do-tipo (get tipos campo)))
    :media  (partial media campo)))

;; --- estagios ---------------------------------------------------------------

(defn- crescente [a b] (compare a b))
(defn- decrescente [a b] (compare b a))

(defn- comparador [ordem]
  (case ordem
    :asc  crescente
    :desc decrescente))

(defn- projetar [campos registro]
  (select-keys registro campos))

(defn- agrupar [campo agregar registros]
  (into (sorted-map)
        (map (fn [[chave grupo]] [chave (agregar grupo)]))
        (group-by #(get % campo) registros)))

(defn- compilar-elemento
  "Elemento da AST -> {:f funcao, :saida tipo-de-saida, :colunas campos?}.
  O `case` roda uma vez por no, na compilacao; os registros so passam
  pelas funcoes devolvidas."
  [tipos elemento]
  (let [[tipo a b] elemento]
    (case tipo
      :onde        {:f (partial filter (compilar-expr a)) :saida :registros}
      ;; sort-by e estavel: empatados mantem a ordem em que vieram
      :ordenar-por {:f (partial sort-by #(get % a) (comparador b)) :saida :registros}
      :limitar     {:f (partial take a) :saida :registros}
      :selecionar  {:f (partial map (partial projetar a)) :saida :registros :colunas a}
      (:contar :soma :media)
                   {:f (compilar-agregacao tipos elemento) :saida :valor}
      :agrupar-por {:f (partial agrupar a (compilar-agregacao tipos b)) :saida :grupos})))

(defn- encadear
  "Compoe as funcoes na ordem de leitura (esquerda para a direita).
  `comp` aplica da direita para a esquerda; por isso (comp g f)."
  [funcoes]
  (reduce (fn [f g] (comp g f)) identity funcoes))

(defn compilar
  "AST conferida -> plano de execucao:
    {:executar  funcao de registros no resultado
     :saida     :registros, :valor ou :grupos
     :colunas   campos a imprimir, na ordem}"
  [esquema ast]
  (let [tipos     (into {} esquema)
        compilados (mapv (partial compilar-elemento tipos) (:estagios ast))]
    {:executar (encadear (map :f compilados))
     :saida    (if (seq compilados) (:saida (peek compilados)) :registros)
     :colunas  (or (some :colunas compilados) (mapv first esquema))}))
