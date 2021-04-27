(in-package #:spinneret)

;; These are the only functions that are called at run time.
(declaim (inline
          void?
          boolean?
          ;; These are only called at run time by dynamic-tag.
          inline?
          paragraph?
          preformatted?))

(define-global-parameter *void-elements*
    '(:!doctype :area :base :br :col :command :embed :hr :img
      :input :keygen :link :meta :param :source :track :wbr))

(defun void? (element)
  (declare (inline memq))
  (memq element *void-elements*))

(define-global-parameter *literal-elements*
  '(:pre :script :style))

(defun literal? (element)
  (memq element *literal-elements*))

(define-global-parameter *inline-elements*
  '(:a :abbr :address :bdo :small :code :samp :kbd
    :cite :strong :dfn :br :em :q :data :time :var
    :sub :sup :i :b :s :u :mark :ruby :rt :rp :bdi :span :wbr
    :ins :del :col :meter :output))

(defun inline? (element)
  (declare (inline memq))
  (memq element *inline-elements*))

(define-global-parameter *paragraph-elements*
  '(:meta :title :button :label :li :h1 :h2 :h3 :h4 :h5 :h6 :p :legend :option
    :dt :dd :figcaption :iframe :colgroup :td :th :output :summary :command))

(defun paragraph? (element)
  (declare (inline memq))
  (memq element *paragraph-elements*))

(define-global-parameter *end-tag-optional*
  ;; html head body
  '(:li :dt :dd :p :rt :rp :optgroup
    :option :colgroup :thead :tbody :tfoot :tr :td :th
    :meta))

(defun unmatched? (element)
  (memq element *end-tag-optional*))

(define-global-parameter *preformatted*
  '(:pre :textarea :script :style))

(defun preformatted? (element)
  (declare (inline memq))
  (memq element *preformatted*))

(defun needs-close? (element)
  (not (or (void? element)
           (unmatched? element))))

(define-global-parameter *pseudotags*
    '((:doctype . make-doctype)
      (:!doctype . make-doctype)
      (:cdata . make-cdata)
      (:!-- . make-comment)
      (:comment . make-comment)
      (:html . make-html)
      (:head . make-head)
      (:raw . write-raw)
      (:h* . expand-h*)
      (:tag . expand-dynamic-tag)))

(defun pseudotag-expander (element)
  (cdr (assoc element *pseudotags*)))

(defun pseudotag-expand (element args)
  (let ((expander (pseudotag-expander element)))
    (if expander
        (apply expander args)
        (cons element args))))

(define-global-parameter *html5-elements*
  '(:a :abbr :address :area :article :aside :audio :b :base :bdi :bdo :blockquote
    :body :br :button :canvas :caption :cite :code :col :colgroup :command :data
    :datalist :dd :del :details :dfn :div :dl :dt :em :embed :fieldset
    :figcaption :figure :footer :form :head :h1 :h2 :h3 :h4 :h5 :h6 :header
    :hgroup :hr :html :i :iframe :img :input :ins :kbd :keygen :label :legend :li
    :link :main :map :mark :menu :meta :meter :nav :noscript :object :ol
    :optgroup :option :output :p :param :picture :pre :progress :q :rp :rt :ruby :s :samp
    :script :section :select :small :source :span :strong :style :sub :summary
    :sup :table :tbody :td :template :textarea :tfoot :th :thead :time :title :tr
    :track :u :ul :var :video :wbr))

(-> valid? (keyword) (or keyword null))
(defun valid? (element)
  (or (car (memq element *html5-elements*))
      (valid-custom-element-name? element)))

(defun invalid? (element)
  (not (valid? element)))

(define-global-parameter *embedded-content*
  '(:math :svg))

(defun embedded? (element)
  (memq element *embedded-content*))

(define-global-parameter *boolean-attributes*
  '(:async :autofocus :autoplay :checked :controls
    :default :defer :disabled :download :formnovalidate :hidden
    :ismap :itemscope :loop :multiple :muted :novalidate
    :open :readonly :required :reversed :scoped
    :seamless :selected :typemustmatch))

(defun boolean? (attr)
  (declare (inline memq))
  (memq attr *boolean-attributes*))

(define-global-parameter *core-attributes*
  '(:accesskey :class :contenteditable :contextmenu :dir :draggable
    :dropzone :hidden :id :is :lang :spellcheck :style :tabindex :title))

(define-global-parameter *microdata-attributes*
    '(:itemid :itemprop :itemref :itemscope :itemtype))

(defvar *unvalidated-attribute-prefixes* '("data-" "aria-")
  "A list of prefixes for attributes that should not be validated.")

(defun unvalidated-attribute? (attribute)
  (some (op (string-prefix-p _ attribute))
        *unvalidated-attribute-prefixes*))

;; http://www.w3.org/TR/wai-aria/states_and_properties
(define-global-parameter *aria-attributes*
  '(:role))

(define-global-parameter *event-handler-attributes*
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

(define-global-parameter *global-attributes*
    (append *core-attributes*
            *microdata-attributes*
            *event-handler-attributes*))

(define-global-parameter *space-separated-attributes*
  '(:accesskey :class :for :headers :rel :sandbox :sizes))

(defun tokenized-attribute? (attr)
  (memq attr *space-separated-attributes*))

(define-global-parameter *permitted-attributes*
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
     :formaction :formenctype :formmethod :formtarget
     :formnovalidate)
    (:ins :cite :datetime)
    (:keygen :challenge :keytype :autofocus :name :disabled :form)
    (:label :for :form)
    (:link :href :rel :hreflang :media :type :sizes :integrity :crossorigin)
    (:map :name)
    (:menu :type :label)
    (:meta :name :content :http-equiv :charset :property)
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
  (or (null tag)                        ;A dynamic tag.
      (unvalidated-attribute? name)
      ;; Don't try to validate attributes on custom elements.
      (valid-custom-element-name? tag)
      (eql name :attrs)
      (global-attribute? name)
      (aria-attribute? name)
      (let ((permitted (permitted-attributes tag)))
        (or (find name permitted :test #'string=)
            (memq '* permitted)))))

(defun permitted-attributes (tag)
  (cdr (assoc tag *permitted-attributes*)))

(defun global-attribute? (name)
  (memq name *global-attributes*))

(defun aria-attribute? (name)
  (memq name *aria-attributes*))

(define-global-parameter *invalid-custom-element-names*
    '(:annotation-xml
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
  (declare (character char)
           (optimize speed))
  (let ((code (char-code (char-downcase char))))
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
    (and (not (memq tag *invalid-custom-element-names*))
         (valid-string? (symbol-name tag))
         tag)))
