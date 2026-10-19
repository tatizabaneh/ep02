(ns consultas.core-test
  "Testes do builder e do motor, no estilo dos que a correcao descreve.
  Rodar com: clojure -M:test"
  (:require [clojure.test :refer [deftest is testing]]
            [consultas.core :as c]))

(def dados
  (c/ler-csv (str "titulo:texto;ano:inteiro;genero:texto;nota:decimal;duracao:inteiro\n"
                  "Cidade de Deus;2002;drama;8.6;130\n"
                  "Central do Brasil;1998;drama;8.0;113\n"
                  "O Auto da Compadecida;2000;comedia;8.6;104\n"
                  "Tropa de Elite;2007;acao;8.0;115\n"
                  "Bacurau;2019;suspense;7.4;131\n"
                  "Que Horas Ela Volta;2015;drama;7.7;112\n"
                  "Aquarius;2016;drama;7.5;145\n"
                  "Ainda Estou Aqui;2024;drama;8.2;137\n")))

(def base   (c/consulta))
(def dramas (c/onde base [:= :genero "drama"]))
(def dois   (c/limitar dramas 2))
(def tres   (c/limitar dramas 3))

(deftest builder-devolve-consultas-novas
  (is (= {:estagios []} base))
  (is (= {:estagios [[:onde [:= :genero "drama"]]]} dramas) "dramas nao foi alterada")
  (is (= ["Cidade de Deus" "Central do Brasil"]
         (map :titulo (c/executar dois dados))))
  (is (= 3 (count (c/executar tres dados))))
  (is (= 8 (count (c/executar base dados)))))

(deftest texto-e-builder-produzem-a-mesma-ast
  (is (= dois (c/analisar "onde genero = \"drama\" | limitar 2")))
  (is (= (-> base
             (c/onde [:e [:= :genero "drama"] [:>= :nota 8]])
             (c/ordenar-por :nota :desc)
             (c/selecionar [:titulo :nota]))
         (c/analisar "onde genero = \"drama\" e nota >= 8 | ordenar por nota desc | selecionar titulo, nota")))
  (is (= (c/ordenar-por base :ano :asc) (c/analisar "ordenar por ano"))))

(deftest ast-e-dado-puro
  (let [ast (c/analisar "onde nao (ano < 2000 ou nota > 8) | agrupar por genero com media duracao")]
    (is (not-any? fn? (tree-seq coll? seq ast)) "nenhuma funcao dentro da AST")
    (is (= {:estagios [[:onde [:nao [:ou [:< :ano 2000] [:> :nota 8]]]]
                       [:agrupar-por :genero [:media :duracao]]]}
           ast))))

(deftest executar
  (testing "terminais"
    (is (= 8 (c/executar (c/contar base) dados)))
    (is (= 987 (c/executar (c/soma base :duracao) dados)))
    (is (= 127.4 (c/executar (-> dramas (c/media :duracao)) dados))))
  (testing "selecionar projeta os campos"
    (is (= [{:titulo "Bacurau"}]
           (c/executar (-> base (c/onde [:= :ano 2019]) (c/selecionar [:titulo])) dados))))
  (testing "agrupar"
    (is (= {"acao" 1 "comedia" 1 "drama" 5 "suspense" 1}
           (c/executar (c/agrupar-por base :genero [:contar]) dados)))))

(deftest consultas-invalidas
  (is (seq (:erros (c/executar (c/onde base [:= :diretor "x"]) dados))))
  (is (seq (:erros (c/executar (c/onde base [:= :ano "2002"]) dados))))
  (is (seq (:erros (c/executar (c/soma base :titulo) dados))))
  (is (seq (:erros (c/executar (-> base (c/selecionar [:titulo]) (c/limitar 2)) dados)))
      "estagio depois de selecionar torna a consulta invalida"))

(deftest no-desconhecido-falha-nomeando-o-no
  (let [ast {:estagios [[:onde [:= :genero "drama"]] [:voar 3]]}
        e   (try (c/executar ast dados) nil (catch Exception e e))]
    (is (some? e))
    (is (re-find #":voar" (ex-message e))))
  (let [ast {:estagios [[:onde [:xor [:= :ano 1] [:= :ano 2]]]]}
        e   (try (c/executar ast dados) nil (catch Exception e e))]
    (is (re-find #":xor" (ex-message e)))))

(deftest sintaxe-sinaliza-com-ex-info
  (is (thrown? clojure.lang.ExceptionInfo (c/analisar "voar")))
  (is (thrown? clojure.lang.ExceptionInfo (c/analisar "onde (ano > 1"))))
