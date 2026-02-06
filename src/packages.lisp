(defpackage #:shout.ansi
  (:use #:cl)
  (:export
   ;; Colors
   #:color
   #:indexed-color
   #:rgb-color
   #:named-color
   #:make-indexed-color
   #:make-rgb-color
   #:make-named-color
   #:lookup-color
   #:color-index
   #:register-color
   #:emit-fg
   #:emit-bg
   ;; Styles
   #:text-style
   #:make-style
   #:emit-style
   ;; Terminal operations
   #:*escape*
   #:cursor-to
   #:cursor-home
   #:cursor-hide
   #:cursor-show
   #:cursor-up
   #:cursor-down
   #:cursor-forward
   #:cursor-back
   #:clear-screen
   #:clear-line
   #:clear-to-end
   #:reset
   #:begin-sync-update
   #:end-sync-update
   ;; Convenience
   #:fg
   #:bg
   #:fg-rgb
   #:bg-rgb
   #:bold
   #:dim
   #:italic
   #:underline
   #:inverse
   #:color-code
   #:with-style))

(defpackage #:shout.terminal
  (:use #:cl #:shout.ansi)
  (:export
   #:key-event
   #:key-event-char
   #:key-event-code
   #:key-event-ctrl-p
   #:key-event-alt-p
   #:make-key-event
   #:+key-up+
   #:+key-down+
   #:+key-left+
   #:+key-right+
   #:+key-enter+
   #:+key-escape+
   #:+key-tab+
   #:+key-backspace+
   #:+key-delete+
   #:+key-home+
   #:+key-end+
   #:+key-page-up+
   #:+key-page-down+
   #:*terminal-mode*
   #:terminal-mode
   #:enable-raw-mode
   #:disable-raw-mode
   #:terminal-size
   #:enter-alternate-screen
   #:leave-alternate-screen
   #:with-raw-terminal
   #:setup-terminal
   #:restore-terminal
   #:*input-reader*
   #:read-key
   #:read-key-with-timeout
   #:close-tty-stream))

(defpackage #:shout.widgets
  (:use #:cl #:shout.ansi #:shout.terminal)
  (:export
   ;; Base widget
   #:widget
   #:widget-x
   #:widget-y
   #:widget-width
   #:widget-height
   #:widget-focused
   #:widget-visible
   #:widget-help-keys
   #:render
   #:handle-key
   #:focus
   #:blur
   ;; Panel
   #:panel
   #:panel-title
   #:panel-children
   #:panel-border-color
   #:panel-focused-color
   ;; Text area
   #:text-area
   #:text-area-content
   #:text-area-cursor-row
   #:text-area-cursor-col
   #:text-area-scroll-offset
   #:text-area-placeholder
   ;; Checkbox list
   #:checkbox-list
   #:checkbox-list-items
   #:checkbox-list-selected
   #:checkbox-list-checked
   #:toggle-checked
   ;; Tag list
   #:tag-list
   #:tag-list-tags
   #:tag-list-enabled
   #:tag-list-selected
   #:tag-list-editing
   #:tag-list-active-tags
   #:add-tag
   #:remove-tag
   #:toggle-tag
   ;; Progress bar
   #:progress-bar
   #:progress-bar-value
   #:progress-bar-max-value
   #:progress-bar-label
   ;; Status display
   #:status-display
   #:status-display-entries
   #:add-status
   #:clear-status
   ;; Help bar
   #:help-bar
   #:help-bar-bindings
   ;; Config form
   #:config-form
   #:config-form-client-type
   #:config-form-fields
   #:config-form-values
   #:config-form-selected
   #:config-form-submitted
   #:config-form-result
   #:config-form-field-value
   #:config-form-set-value
   ;; Drawing utilities
   #:clear-region
   #:next-spinner-frame
   #:draw-box
   #:draw-text))

(defpackage #:shout.layout
  (:use #:cl #:shout.ansi #:shout.terminal #:shout.widgets)
  (:export
   #:screen
   #:screen-widgets
   #:screen-focus-ring
   #:screen-focus-index
   #:screen-help
   #:add-widget
   #:focus-next
   #:focus-prev
   #:focused-widget
   #:screen-width
   #:screen-height
   #:screen-dirty
   #:update-help-bar
   #:layout-screen
   #:render-screen
   #:remove-widget
   #:dispatch-key))

(defpackage #:shout.bridge
  (:use #:cl)
  (:export
   #:load-multiposter-config
   #:get-configured-clients
   #:get-available-client-types
   #:client-info
   #:client-name
   #:client-type-name
   #:client-enabled-p
   #:client-ready-p
   #:client-setup-fields
   #:post-to-clients
   #:posting-result
   #:result-client-name
   #:result-success-p
   #:result-url
   #:result-error
   #:client-char-limit
   #:client-tags-in-body-p
   #:load-saved-tags
   #:save-tags
   #:add-client-to-config
   #:client-object))

(defpackage #:shout
  (:use #:cl #:shout.ansi #:shout.terminal #:shout.widgets #:shout.layout #:shout.bridge)
  (:export #:main #:shout))
