;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.render
  "The main entry point for UI part needed by the exporter."
  (:require
   [app.common.math :as mth]
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.uuid :as uuid]
   [app.common.logging :as log]
   [app.common.spec :as us]
   [app.common.uri :as u]
   [app.common.geom.shapes :as gsh]
   [beicon.core :as rx]
   [app.common.pages.helpers :as cph]
   [app.main.data.fonts :as df]
   [app.main.repo :as repo]
   [app.config :as cf]
   [app.main.store :as st]
   [app.main.ui.shapes.filters :as filters]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.main.render :as render]
   [app.util.dom :as dom]
   [app.util.globals :as glob]
   [clojure.spec.alpha :as s]
   [rumext.alpha :as mf]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(log/initialize!)
(log/set-level! :root :warn)
(log/set-level! :app :info)

(declare ^:private render-object)

(log/info :hint "Welcome to penpot (Export)"
          :version (:full @cf/version)
          :public-uri (str cf/public-uri))

(defn- parse-params
  [loc]
  (let [href (unchecked-get loc "href")]
    (some-> href u/uri :query u/query-string->map)))

(defn init-ui
  []
  (when-let [params (parse-params glob/location)]
    (when-let [component (case (:route params)
                           "render-object" (render-object params)
                           nil)]
      (mf/mount component (dom/get-element "app")))))

(defn ^:export init
  []
  (init-ui))

(defn reinit
  []
  (mf/unmount (dom/get-element "app"))
  (init-ui))

(defn ^:dev/after-load after-load
  []
  (reinit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPONENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- calc-bounds
  [object objects]
  (let [xf-get-bounds (comp (map (d/getf objects)) (map #(calc-bounds % objects)))
        padding       (filters/calculate-padding object)
        obj-bounds    (-> (filters/get-filters-bounds object)
                          (update :x - padding)
                          (update :y - padding)
                          (update :width + (* 2 padding))
                          (update :height + (* 2 padding)))]

    (cond
      (and (= :group (:type object))
           (:masked-group? object))
      (calc-bounds (get objects (first (:shapes object))) objects)

      (= :group (:type object))
      (->> (:shapes object)
           (into [obj-bounds] xf-get-bounds)
           (gsh/join-rects))

      :else
      obj-bounds)))

(defn- adapt-root-frame
  [objects object-id]
  (if (uuid/zero? object-id)
    (let [object   (get objects object-id)
          shapes   (cph/get-immediate-children objects)
          srect    (gsh/selection-rect shapes)
          object   (merge object (select-keys srect [:x :y :width :height]))
          object   (gsh/transform-shape object)
          object   (assoc object :fill-color "#f0f0f0")]
      (assoc objects (:id object) object))
    objects))

(defn- prepare-objects
  [objects object-id]
  (let [object   (get objects object-id)
        frame-id (if (= :frame (:type object))
                   (:id object)
                   (:frame-id object))

        modifier (-> (gpt/point (:x object) (:y object))
                     (gpt/negate)
                     (gmt/translate-matrix))

        mod-ids  (cons frame-id (cph/get-children-ids objects frame-id))
        updt-fn  #(-> %1
                      (assoc-in [%2 :modifiers :displacement] modifier)
                      (update %2 gsh/transform-shape))]

    [(reduce updt-fn objects mod-ids)
     (calc-bounds object objects)
     object]))


;; (defn prepare-objects
;;   [objects object-id]
;;   (letfn [(adapt-root-frame [objects object-id]
;;             (let [object (get objects object-id)]
;;               [(if (uuid/zero? object-id)
;;                  (let [object   (get objects object-id)
;;                        shapes   (cph/get-immediate-children objects)
;;                        srect    (gsh/selection-rect shapes)
;;                        object   (merge object (select-keys srect [:x :y :width :height]))
;;                        object   (gsh/transform-shape object)
;;                        object   (assoc object :fill-color "#f0f0f0")]
;;                    (assoc objaects (:id object) object))
;;                  objects)
;;                object]))

;;           (position-children [objects object]
;;             (let [object   (get objects object-id)
;;                   frame-id (if (= :frame (:type object))
;;                              (:id object)
;;                              (:frame-id object))

;;                   modifier (-> (gpt/point (:x object) (:y object))
;;                                (gpt/negate)
;;                                (gmt/translate-matrix))

;;                   mod-ids  (cons frame-id (cph/get-children-ids objects frame-id))
;;                   updt-fn  #(-> %1
;;                                 (assoc-in [%2 :modifiers :displacement] modifier)
;;                                 (update %2 gsh/transform-shape))]
;;               (reduce updt-fn objects mod-ids)))]
;;     (let [object

;;     [(-> objects adapt-root-frame position-children)
;;      (calculate-dimensions






(mf/defc object-svg-wrapper
  [{:keys [file-id page-id object-id render-texts? embed?] :as props}]
  (let [state (mf/use-state nil)]

    (mf/with-effect [file-id page-id object-id]
      (->> (rx/zip
            (repo/query! :font-variants {:file-id file-id})
            (repo/query! :trimmed-file {:id file-id :page-id page-id :object-id object-id}))
           (rx/subs
            (fn [[fonts {:keys [data]}]]
              (when (seq fonts) (st/emit! (df/fonts-fetched fonts)))
              (let [objs (get-in data [:pages-index page-id :objects])
                    objs (adapt-root-frame objs object-id)
                    res  (prepare-objects objs object-id)]
                (reset! state res)))))
      (constantly nil))

    (let [[objects object bounds] @state]

      (mf/with-effect [bounds]
        (dom/set-page-style!
         {:size (dm/str (mth/ceil (:width bounds)) "px "
                        (mth/ceil (:height bounds)) "px")}))


      (when objects
        [:& render/object-svg
         {:objects objects
          :object object
          :bounds bounds
          :embed? embed?
          :render-texts? render-texts?
          :zoom 1}]))))

(s/def ::page-id ::us/uuid)
(s/def ::file-id ::us/uuid)
(s/def ::object-id ::us/uuid)
(s/def ::render-text ::us/boolean)
(s/def ::embed ::us/boolean)

(s/def ::render-object-params
  (s/keys :req-un [::file-id ::page-id ::object-id]
          :opt-un [::render-text ::embed]))

(defn- render-object
  [params]
  (let [{:keys [page-id file-id object-id render-texts embed]} (us/conform ::render-object-params params)]
    (mf/html
     [:& object-svg-wrapper
      {:file-id file-id
       :page-id page-id
       :object-id object-id
       :embed? embed
       :render-texts? render-texts}])))
