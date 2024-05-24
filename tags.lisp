(in-package #:spinneret)

;; These are the only functions that are called at run time.
(declaim (inline
          void?
          boolean?
          ;; These are only called at run time by dynamic-tag.
          inline?
          paragraph?
          preformatted?))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro keyword-set (&body body)
    (assert (every #'keywordp body))
    `(load-time-value (set-hash-table ',body :test 'eq) t)))

(define-global-parameter *void-elements*
    (keyword-set
     :!doctype :area :base :br :col :command :embed :hr :img
     :input :keygen :link :meta :param :source :track :wbr))

(defun void? (element)
  (declare (inline memq))
  (gethash element *void-elements*))

(define-global-parameter *literal-elements*
  '(:pre :script :style))

(defun literal? (element)
  (memq element *literal-elements*))

(define-global-parameter *inline-elements*
    (keyword-set
     :a :abbr :address :bdo :small :code :samp :kbd
     :cite :strong :dfn :br :em :q :data :time :var
     :sub :sup :i :b :s :u :mark :ruby :rt :rp :bdi :span :wbr
     :ins :del :col :meter :output :tt :strike :font :big))

(defun inline? (element)
  (declare (inline memq))
  (gethash element *inline-elements*))

(define-global-parameter *paragraph-elements*
    (keyword-set
      :meta :title :button :label :li :h1 :h2 :h3 :h4 :h5 :h6 :p :legend :option
      :dt :dd :figcaption :iframe :colgroup :td :th :output :summary :command))

(defun paragraph? (element)
  (declare (inline memq))
  (gethash element *paragraph-elements*))

(define-global-parameter *end-tag-optional*
    ;; html head body
    (keyword-set
      :li :dt :dd :p :rt :rp :optgroup
      :option :colgroup :thead :tbody :tfoot :tr :td :th
      :meta))

(defun unmatched? (element)
  (gethash element *end-tag-optional*))

(define-global-parameter *preformatted*
    '(:pre :textarea :script :style))

(defun preformatted? (element)
  (declare (inline memq))
  (memq element *preformatted*))

(defun needs-close? (element)
  (not (or (void? element)
           (unmatched? element))))

(defparameter *interpret* nil)

(define-global-parameter *pseudotag-expanders*
  (load-time-value
   (alist-hash-table
    '((:doctype . make-doctype)
      (:!doctype . make-doctype)
      (:cdata . make-cdata)
      (:!-- . make-comment)
      (:comment . make-comment)
      (:html . make-html)
      (:head . make-head)
      (:raw . write-raw)
      (:h* . expand-h*)
      (:tag . expand-dynamic-tag))
    :test 'eq)
   t))

(defun pseudotag-expander (element)
  (gethash element *pseudotag-expanders*))

(defun pseudotag-expand (element args)
  (let ((expander (pseudotag-expander element)))
    (if expander
        (apply expander args)
        (cons element args))))

(define-global-parameter *html5-elements*
    (keyword-set
      :a :abbr :address :area :article :aside :audio :b :base :bdi :bdo :blockquote
      :body :br :button :canvas :caption :cite :code :col :colgroup :command :data
      :datalist :dd :del :details :dfn :dialog :div :dl :dt :em :embed :fieldset
      :figcaption :figure :footer :form :head :h1 :h2 :h3 :h4 :h5 :h6 :header
      :hgroup :hr :html :i :iframe :img :input :ins :kbd :keygen :label :legend :li
      :link :main :map :mark :math :menu :meta :meter :nav :noscript :object :ol
      :optgroup :option :output :p :param :picture :pre :progress :q :rp :rt :ruby :s :samp
      :script :section :select :small :source :span :strong :style :sub :svg :summary
      :sup :table :tbody :td :template :textarea :tfoot :th :thead :time :title :tr
      :track :u :ul :var :video :wbr
      ;; SVG elements
      :animate :animatemotion :animatetransform :circle :clippath :cursor :defs
      :desc :ellipse :feblend :fecolormatrix :fecomponenttransfer :fecomposite
      :feconvolvematrix :fediffuselighting :fedisplacementmap :fedistantlight
      :fedropshadow :feflood :fefunca :fefuncb :fefuncg :fefuncr :fegaussianblur
      :feimage :femerge :femergenode :femorphology :feoffset :fepointlight
      :fespecularlighting :fespotlight :fetile :feturbulence :filter :font
      :font-face :font-face-format :font-face-name :font-face-src :font-face-uri
      :foreignobject :g :glyph :glyphref :hkern :image :line :lineargradient :marker
      :mask :metadata :missing-glyph :mpath :path :pattern :polygon :polyline
      :radialgradient :rect :set :stop :switch :symbol :text :textpath :tref :tspan
      :use :view :vkern))

(define-global-parameter *html3-elements*
    (keyword-set
      :plaintext :big :strike :tt :applet :font :basefont :isindex))

(-> valid? (keyword) (values (or keyword null) &optional))
(defun valid? (element)
  (or (gethash element *html5-elements*)
      (gethash element *html3-elements*)
      (valid-custom-element-name? element)))

(defun invalid? (element)
  (not (valid? element)))

(define-global-parameter *embedded-content*
  '(:math :svg))

(defun embedded? (element)
  (memq element *embedded-content*))

(define-global-parameter *boolean-attributes*
    (keyword-set
      :async :autofocus :autoplay :checked :controls
      :default :defer :disabled :download :formnovalidate :hidden
      :ismap :itemscope :loop :multiple :muted :novalidate
      :open :readonly :required :reversed :scoped
      :seamless :selected :typemustmatch
      :allowfullscreen :allowpaymentrequest))

(defun boolean? (attr)
  (declare (inline memq))
  (gethash attr *boolean-attributes*))

(defvar *unvalidated-attribute-prefixes* '("data-" "aria-" "hx-")
  "A list of prefixes for attributes that should not be validated.")

(defun unvalidated-attribute? (attribute)
  (some (op (string-prefix-p _ attribute))
        *unvalidated-attribute-prefixes*))

;; http://www.w3.org/TR/wai-aria/states_and_properties
(define-global-parameter *aria-attributes*
    '(:role))

(eval-always
  (define-global-parameter *core-attributes*
      '(:accesskey :autocapitalize :autofocus
        :class :contenteditable :contextmenu
        :dir :draggable :dropzone
        :enterkeyhint :exportparts
        :hidden :id :inert :inputmode :is
        :lang :nonce :part :popover
        :slot :spellcheck :style
        :tabindex :title :translate :virtualkeyboardpolicy))

  (define-global-parameter *html3-attributes*
      '(:background :bgcolor :text :link :vlink :alink ;; Decoration
        :align :valign :compact :width :height :size)) ;; Layout

  (define-global-parameter *microdata-attributes*
      '(:itemid :itemprop :itemref :itemscope :itemtype))

  (define-global-parameter *event-handler-attributes*
      '(:onabort :onblur :oncanplay :oncanplaythrough :onchange :onclick
        :oncontextmenu :ondblclick :ondrag :ondragend :ondragenter
        :ondragleave :ondragover :ondragstart :ondrop :ondurationchange
        :onemptied :onended :onerror :onfocus :onfocusout :oninput :oninvalid :onkeydown
        :onkeypress :onkeyup :onload :onloadeddata :onloadedmetadata
        :onloadstart :onmousedown :onmousemove :onmouseout :onmouseover
        :onmouseup :onmousewheel :onpause :onplay :onplaying :onprogress
        :onratechange :onreadystatechange :onreset :onscroll :onseeked
        :onseeking :onselect :onshow :onstalled :onsubmit :onsuspend
        :ontimeupdate :onvolumechange :onwaiting)))

(define-global-parameter *global-attributes*
  (load-time-value
   (set-hash-table
    (append *core-attributes*
            *html3-attributes*
            *microdata-attributes*
            *event-handler-attributes*)
    :test 'eq)
   t))

(define-global-parameter *space-separated-attributes*
  '(:accesskey :class :for :headers :rel :sandbox :sizes))

(defun tokenized-attribute? (attr)
  (memq attr *space-separated-attributes*))

(eval-always
  (defun parse-permitted-attributes-alist (alist)
    (lret ((table (alist-hash-table alist :test 'eq)))
      (serapeum:do-hash-table (k v table)
        (setf (gethash k table)
              (set-hash-table v :key #'string :test #'equal))))))

(define-global-parameter *permitted-attributes*
  (load-time-value
   (parse-permitted-attributes-alist
    '((:a :href :target :rel :hreflang :media :type :download :ping)
      (:applet :codebase :code :alt :name :hspace :vspace)
      (:area :alt :href :target :rel :media :hreflang :type :shape :coords  :nohref)
      (:audio :autoplay :preload :controls :loop :mediagroup :muted :src)
      (:base :href :target)
      (:blockquote :cite)
      (:body :onafterprint :onbeforeprint :onbeforeunload :onblur :onerror
             :onfocus :onhashchange :onload :onmessage :onoffline :ononline
             :onpopstate :onresize :onstorage :onunload)
      (:br :clear)
      (:button :name :disabled :form :type :value
               :formaction :formenctype :formmethod :formtarget
               :formnovalidate
               :popovertarget :popovertargetaction :autofocus)
      (:col :span)
      (:colgroup :span)
      (:command :type :label :icon :disabled
                :radiogroup :checked)
      (:del :cite :datetime)
      (:details :open)
      (:dialog :open)
      (:dl :compact)
      (:embed :src :type *)
      (:fieldset :name :disabled :form)
      (:font :color)
      (:form :action :method :enctype :name :accept-charset
             :novalidate :target :autocomplete)
      (:html :manifest :version :xmlns :prefix)
      (:head :prefix :profile)
      (:hr :noshade)
      (:iframe :src :srcdoc :name :sandbox :seamless :allowfullscreen
               :allowpaymentrequest :allow :frameborder :csp :fetchpriority :loading
               :referrerpolicy)
      (:img :src :alt  :loading :usemap :ismap :border :crossorigin
            :srcset :sizes :hspace :vspace)
      (:isindex :prompt)
      (:input :name :disabled :form :type :minlength :maxlength :readonly :value
              :autocomplete :autofocus :list :pattern :required :placeholder
              :checked :accept :capture :multiple :src  :alt :inputmode
              :min :max :step :dirname
              :formaction :formenctype :formmethod :formtarget
              :formnovalidate
              :onfocus :onfocusout
              :popovertarget :popovertargetaction)
      (:ins :cite :datetime)
      (:keygen :challenge :keytype :autofocus :name :disabled :form)
      (:label :for :form)
      (:li :type :value)
      (:link :href :rel :hreflang :media :type :sizes :integrity :crossorigin :referrerpolicy)
      (:map :name)
      (:menu :type :label)
      (:meta :name :content :http-equiv :charset :property :media)
      (:meter :value :min :low :high :max :optimum)
      (:object :data :type :height :width :usemap :name :form)
      (:ol :start :reversed :type)
      (:optgroup :label :disabled)
      (:option :disabled :selected :label :value)
      (:output :name :form :for)
      (:param :name :value)
      (:progress :value :max)
      (:q :cite)
      (:script :type :language :src :defer :async :charset :integrity
               :crossorigin)
      (:select :name :disabled :form :size :multiple :autofocus :required)
      (:source :src :srcset :sizes :type :media)
      (:style :type :media :scoped)
      (:table :border :cellspacing :cellpadding)
      (:td :colspan :rowspan
           :headers :nowrap)
      (:textarea :name :disabled :form :readonly :maxlength :autofocus :required
                 :placeholder :dirname :rows :wrap :cols)
      (:th :scope :colspan :rowspan :headers :nowrap)
      (:time :datetime)
      (:track :kind :src :srclang :label :default)
      (:ul :type)
      (:video :autoplay :preload :controls :loop :poster
              :mediagroup :muted :src :crossorigin)
      ;; SVG attributes
      (:circle :id :class :style :transform :cx :cy :r)
      (:ellipse :id :class :style :transform :cx :cy :rx :ry)
      (:line :id :class :style :transform :x1 :y1 :x2 :y2)
      (:path :id :class :style :transform :d)
      (:polygon :id :class :style :transform :points)
      (:polyline :id :class :style :transform :points)
      (:rect :id :class :style :transform :x :y :width :height :rx :ry)
      (:mesh :id :class :style :transform)
      (:text :id :class :style :transform :x :y :dx :dy :rotate :textLength :lengthAdjust)
      (:tspan :id :class :style :transform :x :y :dx :dy :rotate)
      (:textPath :id :class :style :transform :href :startOffset :method :spacing :side :path)
      (:defs :id :class :style :transform)
      (:g :id :class :style :transform)
      (:image :id :class :style :transform :href :x :y :width :height :preserveAspectRatio :externalResourceRequired :corssorigin )
      (:svg :id :class :style :transform :x :y :width :height :viewBox :preserveAspectRatio
            :linecap :fill :stroke :stroke-width :stroke-linecap :stroke-linejoin)
      (:symbol :id :class :style :transform :x :y :width :height :viewBox :preserveAspectRatio :refX :refY)
      (:use :id :class :style :transform :x :y :width :height)
      (:view :id :class :style :transform :viewBox :preserveAspectRatio :viewTarget)))
   t)
  "Alist of (tag . attributes). These are the element-specific
attributes, beyond the global attributes.")

(defun valid-attribute? (tag name)
  (or (null tag)                        ;A dynamic tag.
      (unvalidated-attribute? name)
      ;; Don't try to validate attributes on custom elements.
      (valid-custom-element-name? tag)
      (eql name :attrs)
      (global-attribute? name)
      (aria-attribute? name)
      (when-let ((permitted (permitted-attributes tag)))
        (or (gethash (string name) permitted)
            (gethash "*" permitted)))))

(defun permitted-attributes (tag)
  (gethash tag *permitted-attributes*))

(defun global-attribute? (name)
  (gethash name *global-attributes*))

(defun aria-attribute? (name)
  (memq name *aria-attributes*))

(define-global-parameter *invalid-custom-element-names*
    (keyword-set
      :annotation-xml
      :color-profile
      :font-face
      :font-face-src
      :font-face-uri
      :font-face-format
      :font-face-name
      :missing-glyph)
  "Names that are not allowed for custom elements.")

(-> pcen-char? (character) boolean)
(defun pcen-char? (char)
  "Is CHAR a valid character for a Potential Custom Element Name?"
  (declare (character char))
  (let ((code (char-code (char-downcase char))))
    (declare (optimize speed))
    (or (= code (char-code #\-))
        (= code (char-code #\.))
        (<= (char-code #\0) code (char-code #\9))
        (= code (char-code #\_))
        (<= (char-code #\a) code (char-code #\z))
        (= code #xB7)
        (<= #xC0 code #xD6)
        (<= #xD8 code #xF6)
        (<= #xF8 code #x37D)
        (<= #x37F code #x1FFF)
        (<= #x200C code #x200D)
        (<= #x203F code #x2040)
        (<= #x2070 code #x218F)
        (<= #x2C00 code #x2FEF)
        (<= #x3001 code #xD7FF)
        (<= #xF900 code #xFDCF)
        (<= #xFDF0 code #xFFFD)
        (<= #x10000 code #xEFFFF))))

;; <https://html.spec.whatwg.org/multipage/custom-elements.html#valid-custom-element-name>
(-> valid-custom-element-name? (keyword) (or keyword null))
(defun valid-custom-element-name? (tag)
  "Does TAG satisfy the requirements for a custom element name?"
  (declare (keyword tag)
           (optimize speed))
  (labels ((ascii-alpha? (char)
             (or (char<= #\A char #\Z)
                 (char<= #\a char #\z)))
           (valid-string? (s)
             ;; "These requirements ensure a number of goals for valid
             ;; custom element names:"
             (and
              (>= (length s) 2)
              ;; "They contain a hyphen, used for namespacing and to
              ;; ensure forward compatibility (since no elements will be
              ;; added to HTML, SVG, or MathML with hyphen-containing
              ;; local names in the future)."
              (find #\- s :start 1)
              ;; "They start with an ASCII lower alpha, ensuring that
              ;; the HTML parser will treat them as tags instead of as
              ;; text."
              (ascii-alpha? (aref s 0))
              ;; "They do not contain any ASCII upper alphas, ensuring
              ;; that the user agent can always treat HTML elements
              ;; ASCII-case-insensitively." But Spinneret is not
              ;; case-sensitive...
              t
              ;; "They can always be created with createElement() and
              ;; createElementNS(), which have restrictions that go
              ;; beyond the parser's."
              (every #'pcen-char? s))))
    (and (not (gethash tag *invalid-custom-element-names*))
         (valid-string? (symbol-name tag))
         tag)))
