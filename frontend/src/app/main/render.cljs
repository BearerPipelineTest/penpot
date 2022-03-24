;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.render
  "Rendering utilities and components for penpot SVG.

  NOTE: This namespace is used from worker and from many parts of the
  workspace; we need to be careful when adding new requires because
  this can cause to import too many deps on worker bundle."

  (:require
   ["react-dom/server" :as rds]
   [app.common.colors :as clr]
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.align :as gal]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.math :as mth]
   [app.common.pages.helpers :as cph]
   [app.config :as cfg]
   [app.main.fonts :as fonts]
   [app.main.ui.context :as muc]
   [app.main.ui.shapes.bool :as bool]
   [app.main.ui.shapes.circle :as circle]
   [app.main.ui.shapes.embed :as embed]
   [app.main.ui.shapes.export :as export]
   [app.main.ui.shapes.filters :as filters]
   [app.main.ui.shapes.frame :as frame]
   [app.main.ui.shapes.group :as group]
   [app.main.ui.shapes.image :as image]
   [app.main.ui.shapes.path :as path]
   [app.main.ui.shapes.rect :as rect]
   [app.main.ui.shapes.shape :refer [shape-container]]
   [app.main.ui.shapes.svg-raw :as svg-raw]
   [app.main.ui.shapes.text :as text]
   [app.main.ui.shapes.text.fontfaces :as ff]
   [app.util.http :as http]
   [app.util.object :as obj]
   [app.util.strings :as ust]
   [app.util.timers :as ts]
   [beicon.core :as rx]
   [clojure.set :as set]
   [cuerdas.core :as str]
   [rumext.alpha :as mf]))

(def ^:const viewbox-decimal-precision 3)
(def ^:private default-color clr/canvas)

(mf/defc background
  [{:keys [vbox color]}]
  [:rect
   {:x (:x vbox)
    :y (:y vbox)
    :width (:width vbox)
    :height (:height vbox)
    :fill color}])

(defn- calculate-dimensions
  [{:keys [objects] :as data} vport]
  (let [shapes    (cph/get-immediate-children objects)
        rect      (cond->> (gsh/selection-rect shapes)
                    (some? vport)
                    (gal/adjust-to-viewport vport))]
    (-> rect
        (update :x mth/finite 0)
        (update :y mth/finite 0)
        (update :width mth/finite 100000)
        (update :height mth/finite 100000))))

(declare shape-wrapper-factory)

(defn frame-wrapper-factory
  [objects]
  (let [shape-wrapper (shape-wrapper-factory objects)
        frame-shape   (frame/frame-shape shape-wrapper)]
    (mf/fnc frame-wrapper
      [{:keys [shape] :as props}]
      (let [childs (mapv #(get objects %) (:shapes shape))
            shape  (gsh/transform-shape shape)]
        [:> shape-container {:shape shape}
         [:& frame-shape {:shape shape :childs childs}]]))))

(defn group-wrapper-factory
  [objects]
  (let [shape-wrapper (shape-wrapper-factory objects)
        group-shape   (group/group-shape shape-wrapper)]
    (mf/fnc group-wrapper
      [{:keys [shape] :as props}]
      (let [childs (mapv #(get objects %) (:shapes shape))]
        [:& group-shape {:shape shape
                         :is-child-selected? true
                         :childs childs}]))))

(defn bool-wrapper-factory
  [objects]
  (let [shape-wrapper (shape-wrapper-factory objects)
        bool-shape   (bool/bool-shape shape-wrapper)]
    (mf/fnc bool-wrapper
      [{:keys [shape] :as props}]
      (let [childs (mf/with-memo [(:id shape) objects]
                     (->> (cph/get-children-ids objects (:id shape))
                          (select-keys objects)))]
        [:& bool-shape {:shape shape :childs childs}]))))

(defn svg-raw-wrapper-factory
  [objects]
  (let [shape-wrapper (shape-wrapper-factory objects)
        svg-raw-shape   (svg-raw/svg-raw-shape shape-wrapper)]
    (mf/fnc svg-raw-wrapper
      [{:keys [shape] :as props}]
      (let [childs (mapv #(get objects %) (:shapes shape))]
        (if (and (map? (:content shape))
                 (or (= :svg (get-in shape [:content :tag]))
                     (contains? shape :svg-attrs)))
          [:> shape-container {:shape shape}
           [:& svg-raw-shape {:shape shape
                              :childs childs}]]

          [:& svg-raw-shape {:shape shape
                             :childs childs}])))))

(defn shape-wrapper-factory
  [objects]
  (mf/fnc shape-wrapper
    [{:keys [frame shape] :as props}]
    (let [group-wrapper   (mf/use-memo (mf/deps objects) #(group-wrapper-factory objects))
          svg-raw-wrapper (mf/use-memo (mf/deps objects) #(svg-raw-wrapper-factory objects))
          bool-wrapper    (mf/use-memo (mf/deps objects) #(bool-wrapper-factory objects))
          frame-wrapper   (mf/use-memo (mf/deps objects) #(frame-wrapper-factory objects))]
      (when (and shape (not (:hidden shape)))
        (let [shape (gsh/transform-shape shape)
              opts #js {:shape shape}
              svg-raw? (= :svg-raw (:type shape))]
          (if-not svg-raw?
            [:> shape-container {:shape shape}
             (case (:type shape)
               :text    [:> text/text-shape opts]
               :rect    [:> rect/rect-shape opts]
               :path    [:> path/path-shape opts]
               :image   [:> image/image-shape opts]
               :circle  [:> circle/circle-shape opts]
               :frame   [:> frame-wrapper {:shape shape}]
               :group   [:> group-wrapper {:shape shape :frame frame}]
               :bool    [:> bool-wrapper  {:shape shape :frame frame}]
               nil)]

            ;; Don't wrap svg elements inside a <g> otherwise some can break
            [:> svg-raw-wrapper {:shape shape :frame frame}]))))))

(defn format-viewbox
  "Format a viewbox given a rectangle"
  [{:keys [x y width height] :or {x 0 y 0 width 100 height 100}}]
  (str/join
   " "
   (->> [x y width height]
        (map #(ust/format-precision % viewbox-decimal-precision)))))

(mf/defc page-svg
  {::mf/wrap [mf/memo]}
  [{:keys [data width height thumbnails? embed? include-metadata?] :as props
    :or {embed? false include-metadata? false}}]
  (let [objects (:objects data)
        shapes  (cph/get-immediate-children objects)

        root-children
        (->> shapes
             (remove cph/frame-shape?)
             (mapcat #(cph/get-children-with-self objects (:id %))))

        vport   (when (and (some? width) (some? height))
                  {:width width :height height})

        dim     (calculate-dimensions data vport)
        vbox    (format-viewbox dim)
        background-color (get-in data [:options :background] default-color)

        frame-wrapper
        (mf/use-memo
         (mf/deps objects)
         #(frame-wrapper-factory objects))

        shape-wrapper
        (mf/use-memo
         (mf/deps objects)
         #(shape-wrapper-factory objects))]

    [:& (mf/provider embed/context) {:value embed?}
     [:& (mf/provider export/include-metadata-ctx) {:value include-metadata?}
      [:svg {:view-box vbox
             :version "1.1"
             :xmlns "http://www.w3.org/2000/svg"
             :xmlnsXlink "http://www.w3.org/1999/xlink"
             :xmlns:penpot (when include-metadata? "https://penpot.app/xmlns")
             :style {:width "100%"
                     :height "100%"
                     :background background-color}}

       (when include-metadata?
         [:& export/export-page {:options (:options data)}])

       [:& ff/fontfaces-style {:shapes root-children}]
       (for [item shapes]
         (let [frame? (= (:type item) :frame)]
           (cond
             (and frame? thumbnails? (some? (:thumbnail item)))
             [:> shape-container {:shape item}
              [:& frame/frame-thumbnail {:shape item}]]

             frame?
             [:& frame-wrapper {:shape item
                                :key (:id item)}]
             :else
             [:& shape-wrapper {:shape item
                                :key (:id item)}])))]]]))


;; Component that serves for render frame thumbnails, mainly used in
;; the viewer and handoff

(mf/defc frame-svg
  {::mf/wrap [mf/memo]}
  [{:keys [objects frame zoom show-thumbnails?] :or {zoom 1} :as props}]
  (let [frame-id          (:id frame)
        include-metadata? (mf/use-ctx export/include-metadata-ctx)

        modifier
        (mf/with-memo [(:x frame) (:y frame)]
          (-> (gpt/point (:x frame) (:y frame))
              (gpt/negate)
              (gmt/translate-matrix)))

        objects
        (mf/with-memo [frame-id objects modifier]
          (let [update-fn #(assoc-in %1 [%2 :modifiers :displacement] modifier)]
            (->> (cph/get-children-ids objects frame-id)
                 (into [frame-id])
                 (reduce update-fn objects))))

        frame
        (mf/with-memo [modifier]
          (assoc-in frame [:modifiers :displacement] modifier))

        wrapper
        (mf/with-memo [objects]
          (frame-wrapper-factory objects))

        width  (* (:width frame) zoom)
        height (* (:height frame) zoom)
        vbox   (format-viewbox {:width (:width frame 0) :height (:height frame 0)})]

    [:svg {:view-box vbox
           :width (ust/format-precision width viewbox-decimal-precision)
           :height (ust/format-precision height viewbox-decimal-precision)
           :version "1.1"
           :xmlns "http://www.w3.org/2000/svg"
           :xmlnsXlink "http://www.w3.org/1999/xlink"
           :xmlns:penpot (when include-metadata? "https://penpot.app/xmlns")}
     (if (or (not show-thumbnails?) (nil? (:thumbnail frame)))
       [:& wrapper {:shape frame :view-box vbox}]

       ;; Render the frame thumbnail
       (let [frame (gsh/transform-shape frame)]
         [:> shape-container {:shape frame}
          [:& frame/frame-thumbnail {:shape frame}]]))]))


;; Component for rendering a thumbnail of a single componenent. Mainly
;; used to render thumbnails on assets panel.

(mf/defc component-svg
  {::mf/wrap [mf/memo #(mf/deferred % ts/idle-then-raf)]}
  [{:keys [objects group zoom] :or {zoom 1} :as props}]
  (let [group-id (:id group)
        include-metadata? (mf/use-ctx export/include-metadata-ctx)

        modifier
        (mf/use-memo
         (mf/deps (:x group) (:y group))
         (fn []
           (-> (gpt/point (:x group) (:y group))
               (gpt/negate)
               (gmt/translate-matrix))))

        objects
        (mf/use-memo
         (mf/deps modifier objects group-id)
         (fn []
           (let [modifier-ids (cons group-id (cph/get-children-ids objects group-id))
                 update-fn    #(assoc-in %1 [%2 :modifiers :displacement] modifier)
                 modifiers    (reduce update-fn {} modifier-ids)]
             (gsh/merge-modifiers objects modifiers))))

        group  (get objects group-id)
        width  (* (:width group) zoom)
        height (* (:height group) zoom)
        vbox   (format-viewbox {:width (:width group 0)
                                :height (:height group 0)})
        group-wrapper
        (mf/use-memo
         (mf/deps objects)
         (fn [] (group-wrapper-factory objects)))]

    [:svg {:view-box vbox
           :width (ust/format-precision width viewbox-decimal-precision)
           :height (ust/format-precision height viewbox-decimal-precision)
           :version "1.1"
           :xmlns "http://www.w3.org/2000/svg"
           :xmlnsXlink "http://www.w3.org/1999/xlink"
           :xmlns:penpot (when include-metadata? "https://penpot.app/xmlns")}

     [:> shape-container {:shape group}
      [:& group-wrapper {:shape group :view-box vbox}]]]))

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

(mf/defc object-svg
  {::mf/wrap [mf/memo]}
  [{:keys [objects object bounds zoom render-texts? embed?]
    :or {zoom 1 embed? false}
    :as props}]
  (let [object (cond-> object
                   (:hide-fill-on-export object)
                   (assoc :fills []))

        obj-id (:id object)
        x      (* (:x bounds) zoom)
        y      (* (:y bounds) zoom)
        width  (* (:width bounds) zoom)
        height (* (:height bounds) zoom)

        vbox   (dm/str x " " y " " width " " height)

        frame-wrapper
        (mf/with-memo [objects]
          (frame-wrapper-factory objects))

        group-wrapper
        (mf/with-memo [objects]
          (group-wrapper-factory objects))

        shape-wrapper
        (mf/with-memo [objects]
          (shape-wrapper-factory objects))

        text-shapes   (sequence (filter cph/text-shape?) (vals objects))
        render-texts? (and render-texts? (d/seek (comp nil? :position-data) text-shapes))]

    ;; (mf/with-effect [width height]
    ;;   (dom/set-page-style!
    ;;    {:size (dm/str (mth/ceil width) "px "
    ;;                   (mth/ceil height) "px")}))

    [:& (mf/provider embed/context) {:value embed?}
     [:svg {:id (dm/str "screenshot-" obj-id)
            :view-box vbox
            :width width
            :height height
            :version "1.1"
            :xmlns "http://www.w3.org/2000/svg"
            :xmlnsXlink "http://www.w3.org/1999/xlink"
            ;; Fix Chromium bug about color of html texts
            ;; https://bugs.chromium.org/p/chromium/issues/detail?id=1244560#c5
            :style {:-webkit-print-color-adjust :exact}}

      (let [shapes (cph/get-children objects obj-id)]
        [:& ff/fontfaces-style {:shapes shapes}])

      (case (:type object)
        :frame [:& frame-wrapper {:shape object :view-box vbox}]
        :group [:> shape-container {:shape object}
                [:& group-wrapper {:shape object}]]
        [:& shape-wrapper {:shape object}])]

     ;; Auxiliary SVG for rendering text-shapes
     (when render-texts?
       (for [object text-shapes]
         [:& (mf/provider muc/text-plain-colors-ctx) {:value true}
          [:svg {:id (str "screenshot-text-" (:id object))
                 :view-box (str "0 0 " (:width object) " " (:height object))
                 :width (:width object)
                 :height (:height object)
                 :version "1.1"
                 :xmlns "http://www.w3.org/2000/svg"
                 :xmlnsXlink "http://www.w3.org/1999/xlink"}
           [:& shape-wrapper {:shape (assoc object :x 0 :y 0)}]]]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPRITES (DEBUG)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mf/defc component-symbol
  {::mf/wrap-props false}
  [props]
  (let [id      (obj/get props "id")
        data    (obj/get props "data")
        name    (:name data)
        path    (:path data)
        objects (:objects data)
        root    (get objects id)
        selrect (:selrect root)

        vbox
        (format-viewbox
         {:width (:width selrect)
          :height (:height selrect)})

        modifier
        (mf/use-memo
         (mf/deps (:x root) (:y root))
         (fn []
           (-> (gpt/point (:x root) (:y root))
               (gpt/negate)
               (gmt/translate-matrix))))

        objects
        (mf/use-memo
         (mf/deps modifier id objects)
         (fn []
           (let [modifier-ids (cons id (cph/get-children-ids objects id))
                 update-fn    #(assoc-in %1 [%2 :modifiers :displacement] modifier)]
             (reduce update-fn objects modifier-ids))))

        root
        (mf/use-memo
         (mf/deps modifier root)
         (fn [] (assoc-in root [:modifiers :displacement] modifier)))

        group-wrapper
        (mf/use-memo
         (mf/deps objects)
         (fn [] (group-wrapper-factory objects)))]

    [:> "symbol" #js {:id (str id)
                      :viewBox vbox
                      "penpot:path" path}
     [:title name]
     [:> shape-container {:shape root}
      [:& group-wrapper {:shape root :view-box vbox}]]]))

(mf/defc components-sprite-svg
  {::mf/wrap-props false}
  [props]
  (let [data              (obj/get props "data")
        children          (obj/get props "children")
        embed?            (obj/get props "embed?")
        include-metadata? (obj/get props "include-metadata?")]
    [:& (mf/provider embed/context) {:value embed?}
     [:& (mf/provider export/include-metadata-ctx) {:value include-metadata?}
      [:svg {:version "1.1"
             :xmlns "http://www.w3.org/2000/svg"
             :xmlnsXlink "http://www.w3.org/1999/xlink"
             :xmlns:penpot (when include-metadata? "https://penpot.app/xmlns")
             :style {:width "100vw"
                     :height "100vh"
                     :display (when-not (some? children) "none")}}
       [:defs
        (for [[component-id component-data] (:components data)]
          [:& component-symbol {:id component-id
                                :key (str component-id)
                                :data component-data}])]

       children]]]))

;; ;; TODO: move outside
;; (mf/defc render-sprite
;;   [{:keys [file-id component-id] :as props}]
;;   (let [file (mf/use-state nil)]

;;     (mf/with-effect [file-id]
;;       (->> (repo/query! :file {:id file-id})
;;            (rx/subs
;;             (fn [result]
;;               (reset! file result))))
;;       (constantly nil))

;;     (when @file
;;       [:*
;;        [:& components-sprite-svg {:data (:data @file) :embed true}

;;         (when (some? component-id)
;;           [:use {:x 0 :y 0
;;                  :xlinkHref (str "#" component-id)}])]

;;        (when-not (some? component-id)
;;          [:ul
;;           (for [[id data] (get-in @file [:data :components])]
;;             (let [url (str "#/render-sprite/" (:id @file) "?component-id=" id)]
;;               [:li [:a {:href url} (:name data)]]))])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RENDERING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-image-data [shape]
  (cond
    (= :image (:type shape))
    [(:metadata shape)]

    (some? (:fill-image shape))
    [(:fill-image shape)]

    :else
    []))

(defn- populate-images-cache
  [objects]
  (let [images (->> objects
                    (vals)
                    (mapcat get-image-data))]
    (->> (rx/from images)
         (rx/map #(cfg/resolve-file-media %))
         (rx/flat-map http/fetch-data-uri))))

(defn populate-fonts-cache [objects]
  (let [texts (->> objects
                   (vals)
                   (filterv #(= (:type %) :text))
                   (mapv :content)) ]

    (->> (rx/from texts)
         (rx/map fonts/get-content-fonts)
         (rx/reduce set/union #{})
         (rx/flat-map identity)
         (rx/flat-map fonts/fetch-font-css)
         (rx/flat-map fonts/extract-fontface-urls)
         (rx/flat-map http/fetch-data-uri))))

(defn render-page
  [data]
  (rx/concat
   (->> (rx/merge
         (populate-images-cache (:objects data))
         (populate-fonts-cache (:objects data)))
        (rx/ignore))

   (->> (rx/of data)
        (rx/map
         (fn [data]
           (let [elem (mf/element page-svg #js {:data data :embed? true :include-metadata? true})]
             (rds/renderToStaticMarkup elem)))))))

(defn render-components
  [data]
  (let [;; Join all components objects into a single map
        objects (->> (:components data)
                     (vals)
                     (map :objects)
                     (reduce conj))]
    (rx/concat
     (->> (rx/merge
           (populate-images-cache objects)
           (populate-fonts-cache objects))
          (rx/ignore))

     (->> (rx/of data)
          (rx/map
           (fn [data]
             (let [elem (mf/element components-sprite-svg #js {:data data :embed? true :include-metadata? true})]
               (rds/renderToStaticMarkup elem))))))))
