(in-package #:spinneret)

(defparameter *void-elements*
  '(:!doctype :area :base :br :col :command :embed :hr :img
    :input :keygen :link :meta :param :source :track :wbr))

(defun void? (element)
  (find element *void-elements*))

(defparameter *literal-elements*
  '(:pre :script :style))

(defun literal? (element)
  (find element *literal-elements*))

(defparameter *inline-elements*
  '(:a :abbr :address :bdo :small :code :samp :kbd
    :cite :strong :dfn :br :em :q :data :time :var
    :sub :sup :i :b :s :u :mark :ruby :rt :rp :bdi :span :wbr
    :ins :del :col :meter :output))

(defun inline? (element)
  (find element *inline-elements*))

(defparameter *paragraph-elements*
  '(:meta :title :button :label :li :h1 :h2 :h3 :h4 :h5 :h6 :p :legend :option
    :dt :dd :figcaption :iframe :colgroup :td :th :output :summary :command))

(defun paragraph? (element)
  (find element *paragraph-elements*))

(defparameter *end-tag-optional*
  ;; html head body
  '(:li :dt :dd :p :rt :rp :optgroup
    :option :colgroup :thead :tbody :tfoot :tr :td :th
    :meta))

(defun unmatched? (element)
  (find element *end-tag-optional*))

(defparameter *preformatted*
  '(:pre :textarea :script :style))

(defun preformatted? (element)
  (find element *preformatted*))

(defparameter *pseudotags*
  '((:doctype . make-doctype)
    (:!doctype . make-doctype)
    (:cdata . make-cdata)
    (:!-- . make-comment)
    (:comment . make-comment)
    (:html . make-html)
    (:head . make-head)
    (:raw . write-raw)))

(defun pseudotag-expander (element)
  (cdr (assoc element *pseudotags*)))

(defun pseudotag-expand (element args)
  (let ((expander (pseudotag-expander element)))
    (if expander
        (apply expander args)
        (cons element args))))

(defparameter *html5-elements*
  '(:a :abbr :address :area :article :aside :audio :b :base :bdi :bdo :blockquote
    :body :br :button :canvas :caption :cite :code :col :colgroup :command :data
    :datalist :dd :del :details :dfn :div :dl :dt :em :embed :fieldset
    :figcaption :figure :footer :form :head :h1 :h2 :h3 :h4 :h5 :h6 :header
    :hgroup :hr :html :i :iframe :img :input :ins :kbd :keygen :label :legend :li
    :link :main :map :mark :menu :meta :meter :nav :noscript :object :ol
    :optgroup :option :output :p :param :picture :pre :progress :q :rp :rt :ruby :s :samp
    :script :section :select :small :source :span :strong :style :sub :summary
    :sup :table :tbody :td :textarea :tfoot :th :thead :time :title :tr
    :track :u :ul :var :video :wbr))

(defun valid? (element)
  (find element *html5-elements*))

(defun invalid? (element)
  (not (valid? element)))

(defparameter *embedded-content*
  '(:math :svg))

(defun embedded? (element)
  (find element *embedded-content*))

(defparameter *boolean-attributes*
  '(:async :autofocus :autoplay :checked :controls
    :default :defer :disabled :download :formnovalidate :hidden
    :ismap :itemscope :loop :multiple :muted :novalidate
    :open :readonly :required :reversed :scoped
    :seamless :selected :typemustmatch))

(defun boolean? (attr)
  (find attr *boolean-attributes*))

(defparameter *core-attributes*
  '(:accesskey :class :contenteditable :contextmenu :dir :draggable
    :dropzone :hidden :id :lang :spellcheck :style :tabindex :title))

(defparameter *unvalidated-attribute-prefixes*
  '("data-" "aria-")
  "A list of prefixes for attributes that should not be validated.")

(defun unvalidated-attribute? (attribute)
  (loop for prefix in *unvalidated-attribute-prefixes*
        thereis (starts-with-subseq prefix
                                    (string attribute)
                                    :test #'char-equal)))

;; http://www.w3.org/TR/wai-aria/states_and_properties
(defparameter *aria-attributes*
  '(:role))

(defparameter *event-handler-attributes*
  '(:onabort :onblur :oncanplay :oncanplaythrough :onchange :onclick
    :oncontextmenu :ondblclick :ondrag :ondragend :ondragenter
    :ondragleave :ondragover :ondragstart :ondrop :ondurationchange
    :onemptied :onended :onerror :onfocus :oninput :oninvalid :onkeydown
    :onkeypress :onkeyup :onload :onloadeddata :onloadedmetadata
    :onloadstart :onmousedown :onmousemove :onmouseout :onmouseover
    :onmouseup :onmousewheel :onpause :onplay :onplaying :onprogress
    :onratechange :onreadystatechange :onreset :onscroll :onseeked
    :onseeking :onselect :onshow :onstalled :onsubmit :onsuspend
    :ontimeupdate :onvolumechange :onwaiting))

(defparameter *global-attributes*
  (append *core-attributes* *event-handler-attributes*))

(defparameter *space-separated-attributes*
  '(:accesskey :class :for :headers :rel :sandbox :sizes))

(defun tokenized-attribute? (attr)
  (find attr *space-separated-attributes*))

(defparameter *permitted-attributes*
  '((:a :href :target :rel :hreflang :media :type :download :ping)
    (:area :alt :href :target :rel :media :hreflang :type :shape :coords)
    (:audio :autoplay :preload :controls :loop :mediagroup :muted :src)
    (:base :href :target)
    (:blockquote :cite)
    (:body :onafterprint :onbeforeprint :onbeforeunload :onblur :onerror
     :onfocus :onhashchange :onload :onmessage :onoffline :ononline
     :onpopstate :onresize :onstorage :onunload)
    (:button :name :disabled :form :type :value
     :autofocus :formaction :formenctype :formmethod :formtarget
     :formnovalidate)
    (:canvas :height :width)
    (:col :span)
    (:colgroup :span)
    (:command :type :label :icon :disabled
     :radiogroup :checked)
    (:del :cite :datetime)
    (:details :open)
    (:embed :src :type :height :width *)
    (:fieldset :name :disabled :form)
    (:form :action :method :enctype :name :accept-charset
     :novalidate :target :autocomplete)
    (:html :manifest)
    (:iframe :src :srcdoc :name :width :height :sandbox :seamless)
    (:img :src :alt :height :width :usemap :ismap :border :crossorigin
     :srcset :sizes :crossorigin)
    (:input :name :disabled :form :type :maxlength :readonly :size :value
     :autocomplete :autofocus :list :pattern :required :placeholder
     :dirname :checked :multiple :src :height :width
     :inputmode
     :min :max :step :dirname
     :password :formaction :formenctype :formmethod :formtarget
     :formnovalidate)
    (:ins :cite :datetime)
    (:keygen :challenge :keytype :autofocus :name :disabled :form)
    (:label :for :form)
    (:link :href :rel :hreflang :media :type :sizes :integrity :crossorigin)
    (:map :name)
    (:menu :type :label)
    (:meta :name :content :http-equiv :charset)
    (:meter :value :min :low :high :max :optimum)
    (:object :data :type :height :width :usemap :name :form)
    (:ol :start :reversed :type)
    (:optgroup :label :disabled)
    (:option :disabled :selected :label :value)
    (:output :name :form :for)
    (:param :name :value)
    (:progress :value :max)
    (:q :cite)
    (:script :type :language :src :defer :async :charset :language :integrity
     :crossorigin)
    (:select :name :disabled :form :size :multiple :autofocus :required)
    (:source :src :srcset :sizes :type :media)
    (:style :type :media :scoped)
    (:table :border)
    (:td :colspan :rowspan
     :headers)
    (:textarea :name :disabled :form :readonly :maxlength :autofocus :required
     :placeholder :dirname :rows :wrap :cols)
    (:th :scope :colspan :rowspan :headers)
    (:time :datetime)
    (:track :kind :src :srclang :label :default)
    (:video :autoplay :preload :controls :loop :poster :height :width
     :mediagroup :muted :src :crossorigin))
  "Alist of (tag . attributes). These are the element-specific
attributes, beyond the global attributes.")

(defun valid-attribute? (tag name)
  (or (unvalidated-attribute? name)
      (eql name :attrs)
      (global-attribute? name)
      (aria-attribute? name)
      (let ((permitted (permitted-attributes tag)))
        (or (find name permitted :test #'string=)
            (find '* permitted)))))

(defun permitted-attributes (tag)
  (cdr (assoc tag *permitted-attributes*)))

(defun global-attribute? (name)
  (find name *global-attributes*))

(defun aria-attribute? (name)
  (find name *aria-attributes*))
