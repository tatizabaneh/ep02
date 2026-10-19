(ns consultas.core
  "Texto do CSV -> registros; o query builder; `executar`; e a resposta a
  um comando do terminal como dado (linhas de texto), sem I/O."
  (:require [clojure.string :as str]
            [consultas.sintaxe :as sintaxe]
            [consultas.compilador :as compilador]))

;; ===========================================================================
;; CSV -> {:esquema [[campo tipo] ...] :registros [...]}
;; ===========================================================================

(def ^:private tipos-do-csv
  {"inteiro" :inteiro, "decimal" :decimal, "texto" :texto})

(defn- erro-csv [mensagem]
  (throw (ex-info mensagem {:tipo :csv})))

(defn- ler-cabecalho
  "\"a:texto;b:inteiro\" -> [[:a :texto] [:b :inteiro]]"
  [linha]
  (mapv (fn [coluna]
          (let [[nome tipo] (str/split coluna #":" 2)
                tipo-kw (get tipos-do-csv (some-> tipo str/trim))]
            (cond
              (str/blank? nome) (erro-csv (str "cabecalho sem nome de campo: " coluna))
              (nil? tipo-kw)    (erro-csv (str "tipo desconhecido no cabecalho: " coluna))
              :else             [(keyword (str/trim nome)) tipo-kw])))
        (str/split linha #";" -1)))

(defn- converter [tipo texto]
  (try
    (case tipo
      :inteiro (Long/parseLong (str/trim texto))
      :decimal (Double/parseDouble (str/trim texto))
      :texto   texto)
    (catch NumberFormatException _
      (erro-csv (str "valor '" texto "' nao e " (name tipo))))))

(defn- ler-registro [esquema numero linha]
  (let [valores (str/split linha #";" -1)]
    (if (not= (count valores) (count esquema))
      (erro-csv (str "linha " numero ": esperados " (count esquema)
                     " campos, encontrados " (count valores)))
      (into {} (map (fn [[campo tipo] valor] [campo (converter tipo valor)])
                    esquema valores)))))

(defn ler-csv
  "Texto de um CSV (separador ';', primeira linha `nome:tipo;...`) ->
  {:esquema [[campo tipo] ...] :registros [registro ...]}."
  [texto]
  (let [linhas (->> (str/split-lines (str/replace texto #"^﻿" ""))
                    (remove str/blank?))]
    (if (empty? linhas)
      (erro-csv "arquivo vazio: falta o cabecalho")
      (let [esquema (ler-cabecalho (first linhas))]
        {:esquema   esquema
         :registros (vec (map-indexed (fn [i linha] (ler-registro esquema (+ i 2) linha))
                                      (rest linhas)))}))))

;; ===========================================================================
;; O query builder: cada funcao recebe uma consulta e devolve outra.
;; A consulta e dado: {:estagios [elemento ...]}.
;; ===========================================================================

(defn consulta
  "Consulta nova, sem nenhum estagio: devolve todos os registros."
  []
  {:estagios []})

(defn- acrescentar [c elemento]
  (update c :estagios conj elemento))

(defn onde
  "Acrescenta um filtro. `expr`: [:= :campo valor], [:e a b], [:ou a b], [:nao a]."
  [c expr]
  (acrescentar c [:onde expr]))

(defn ordenar-por
  "`ordem` e :asc ou :desc."
  ([c campo] (ordenar-por c campo :asc))
  ([c campo ordem] (acrescentar c [:ordenar-por campo ordem])))

(defn limitar [c n]
  (acrescentar c [:limitar n]))

(defn selecionar
  "`campos` e um vetor de palavras-chave. Elemento final."
  [c campos]
  (acrescentar c [:selecionar campos]))

;; terminais e agrupamento (alem do contrato minimo)

(defn contar [c] (acrescentar c [:contar]))
(defn soma [c campo] (acrescentar c [:soma campo]))
(defn media [c campo] (acrescentar c [:media campo]))

(defn agrupar-por
  "`agregacao` e [:contar], [:soma :campo] ou [:media :campo]."
  [c campo agregacao]
  (acrescentar c [:agrupar-por campo agregacao]))

(defn analisar
  "Texto de uma consulta (sem QUERY) -> AST. Erro de sintaxe: ex-info."
  [texto]
  (sintaxe/analisar texto))

;; ===========================================================================
;; Execucao
;; ===========================================================================

(defn- planejar
  "Confere a consulta contra o esquema e, se valida, compila.
  Devolve {:erros [...]} ou o plano de execucao."
  [c esquema]
  (let [erros (compilador/conferir esquema c)]
    (if (seq erros)
      {:erros erros}
      (compilador/compilar esquema c))))

(defn executar
  "Executa a consulta `c` sobre `dados` ({:esquema ... :registros ...}).
  Devolve a sequencia de registros, o valor do terminal (num grupo, um
  mapa ordenado chave -> valor) ou {:erros [...]} se a consulta e invalida
  - neste caso, nenhum registro e processado."
  [c {:keys [esquema registros]}]
  (let [plano (planejar c esquema)]
    (if (:erros plano)
      plano
      ((:executar plano) registros))))

;; ===========================================================================
;; Formatacao e resposta a um comando do terminal
;; ===========================================================================

(defn formatar
  "Inteiro como inteiro; decimal com duas casas e ponto, em qualquer
  localidade; texto como esta."
  [v]
  (if (float? v)
    (String/format java.util.Locale/ROOT "%.2f" (to-array [v]))
    (str v)))

(defn- linhas-de-registros [colunas registros]
  (let [linhas (mapv (fn [registro]
                       (str/join "; " (map #(formatar (get registro %)) colunas)))
                     registros)]
    (if (empty? linhas) ["(vazio)"] linhas)))

(defn- linhas-de-grupos [grupos]
  (if (empty? grupos)
    ["(vazio)"]
    (mapv (fn [[chave valor]] (str (formatar chave) "; " (formatar valor))) grupos)))

(defn- linhas-do-resultado [plano resultado]
  (case (:saida plano)
    :registros (linhas-de-registros (:colunas plano) resultado)
    :valor     [(formatar resultado)]
    :grupos    (linhas-de-grupos resultado)))

(defn- erro [mensagem]
  [(str "ERRO: " mensagem)])

(defn consultar
  "Texto da consulta -> linhas de resposta. Nunca lanca excecao."
  [dados texto]
  (try
    (let [plano (planejar (analisar texto) (:esquema dados))]
      (if-let [erros (:erros plano)]
        (erro (str/join "; " erros))
        (linhas-do-resultado plano ((:executar plano) (:registros dados)))))
    (catch Throwable e
      (erro (or (ex-message e) (.getName (class e)))))))

(defn responder
  "Uma linha digitada no terminal -> {:fim? bool, :linhas [texto ...]}.
  Funcao pura: quem le e imprime e o namespace de entrada."
  [dados linha]
  (let [linha (str/trim linha)]
    (cond
      (= linha "EXIT")
      {:fim? true :linhas []}

      (str/blank? linha)
      {:fim? false :linhas (erro "linha em branco")}

      (or (= linha "QUERY") (str/starts-with? linha "QUERY "))
      {:fim? false :linhas (consultar dados (subs linha 5))}

      :else
      {:fim? false
       :linhas (erro (str "comando desconhecido: " (first (str/split linha #"\s+"))
                          " (comandos: QUERY <consulta>, EXIT)"))})))
