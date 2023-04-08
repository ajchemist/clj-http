(ns ajchemist.clj-http.transduce)


(def ^:dynamic *page-index*)
(def ^:dynamic *current-params*)


(defn page-index-limit-xf
  [limit]
  {:pre [(int? limit)]}
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (let [result (rf result input)]
         (if (< *page-index* limit)
           result
           (ensure-reduced result)))))))


(defn page-size-xf
  [page-size]
  {:pre [(int? page-size)]}
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (let [result (rf result input)]
         (if (< (count input) page-size)
           (ensure-reduced result)
           result))))))


(defn recurring-request
  "collect paged data via recurring request"
  [request params update-params xf rf callback]
  (let [f     (xf rf)
        fetch (fn [params respond] (respond (request params)))
        start (fn process
                [idx params acc response]
                (let [acc' (binding [*page-index* idx *current-params* params] (f acc response))]
                  (if (reduced? acc')
                    (callback @acc')
                    (let [idx'    (inc idx)
                          params' (update-params idx' params response)]
                      (fetch params' (fn [response] (process idx' params' acc' response)))))))]
    (fetch params (fn [response] (start 1 params (f) response)))))
