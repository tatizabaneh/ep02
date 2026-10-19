(ns consultas.main
  "Namespace de entrada: o unico com I/O. Le o arquivo CSV, e o laco do
  terminal - escrito com loop/recur - le uma linha, imprime a resposta
  calculada por `consultas.core/responder` e volta ao comeco."
  (:require [consultas.core :as core]))

(defn- imprimir-linhas [linhas]
  (doseq [linha linhas]
    (println linha)))

(defn- carregar [caminho]
  (try
    {:dados (core/ler-csv (slurp caminho :encoding "UTF-8"))}
    (catch Throwable e
      {:erro (str "nao foi possivel carregar " caminho ": "
                  (or (ex-message e) (.getName (class e))))})))

(defn- terminal [dados]
  (loop []
    (print "> ")
    (flush)
    (if-let [linha (read-line)]
      (let [{:keys [fim? linhas]} (core/responder dados linha)]
        (imprimir-linhas linhas)
        (flush)
        (when-not fim?
          (recur)))
      ;; fim da entrada (pipe) equivale a EXIT
      (println))))

(defn -main [& args]
  (if-let [caminho (first args)]
    (let [{:keys [dados erro]} (carregar caminho)]
      (if erro
        (do (println (str "ERRO: " erro))
            (System/exit 1))
        (terminal dados)))
    (do (println "ERRO: uso: clojure -M:run <arquivo.csv>")
        (System/exit 1))))
