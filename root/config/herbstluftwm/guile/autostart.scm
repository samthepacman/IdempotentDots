#!/usr/bin/env guile
!#

(define (hc command)
  (system (string-append "herbstclient " command)))

(define (keybind modkey key command)
  (display (string-append "keybind " modkey "-" key " " command "\n"))
  (hc (string-append "keybind " modkey "-" key " " command)))

(define (mousebind modkey button command)
  (hc (string-append "mousebind " modkey "-" button " " command)))

(define (add-tag name)
  (hc (string-append "add " name)))

(define (set variable value)
  (hc (string-append "set " variable " " value)))

(define (unrule)
  (hc "unrule -F"))

(define (rule spec)
  (hc (string-append "rule " spec)))

(define modkey "Mod4")

(define (create-tag name)
  (add-tag name)
  (keybind modkey name (string-append "use " name))
  (keybind modkey
           (string-append "Shift-" name)
           (string-append "move "  name)))

;; keybindings
(display "Setting Up Keybinds\n\n")
(keybind modkey "Mod1-q"  "quit")
(keybind modkey "Shift-r" "reload")
(keybind modkey "q" "close")
(keybind modkey "Shift-Return"  "spawn alacritty")
(keybind modkey "Return"       "spawn dmenu_run -h 28")

;; tags
(hc "rename default 1")
(for-each create-tag '("1" "2" "3" "4" "5"))
;;(for-each create-tag '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
;; (for-each create-tag '("1" "2" "3" "4" "5" "6" "7" "8" "9"))

;; layout
;; (hc "move_monitor 0 1680x1050+0+0")
;; (hc "add_monitor 1280x1024+1680+0 2")

;; layouting
(keybind modkey "w"       "remove")
(keybind modkey "space"   "cycle_layout 1")
(keybind modkey "u"       "split vertical 0.5")
(keybind modkey "o"       "split horizontal 0.5")
(keybind modkey "f"       "floating toggle")
(keybind modkey "Shift-f" "fullscreen toggle")
(keybind modkey "p"       "pseudotile toggle")

;; resizing
(define resizestep "0.05")
(keybind modkey "Control-h" (string-append "resize left +"  resizestep))
(keybind modkey "Control-j" (string-append "resize down +"  resizestep))
(keybind modkey "Control-k" (string-append "resize up +"    resizestep))
(keybind modkey "Control-l" (string-append "resize right +" resizestep))

;; mouse
(mousebind modkey "Button1" "move")
(mousebind modkey "Button2" "resize")
(mousebind modkey "Button3" "zoom")

;; focus
(keybind modkey "BackSpace"  "cycle_monitor")
(keybind modkey "Tab"        "cycle_all +1")
(keybind modkey "Shift-Tab"  "cycle_all -1")
(keybind modkey "Mod1-c"     "cycle")
(keybind modkey "h"          "focus left")
(keybind modkey "j"          "focus down")
(keybind modkey "k"          "focus up")
(keybind modkey "l"          "focus right")
(keybind modkey "Shift-h"    "shift left")
(keybind modkey "Shift-j"    "shift down")
(keybind modkey "Shift-k"    "shift up")
(keybind modkey "Shift-l"    "shift right")

(keybind modkey "Left"           "focus left")
(keybind modkey "Down"           "focus down")
(keybind modkey "Up"             "focus up")
(keybind modkey "Right"          "focus right")
(keybind modkey "Shift-Left"     "shift left")
(keybind modkey "Shift-Down"     "shift down")
(keybind modkey "Shift-Up"       "shift up")
(keybind modkey "Shift-Right"    "shift right")
;; colors

(display "Setting Up Frames\n")
(set "frame_border_normal_color"  "\\#16161c")
(set "frame_border_active_color"  "\\#96e06c")
(set "frame_bg_normal_color"      "\\#96e06c")
(set "frame_bg_active_color"      "\\#61afef")
(set "frame_border_width"         "2")
(set "always_show_frame"          "0")
(set "frame_bg_transparent"       "1")
(set "frame_transparent_width"    "1")
(set "frame_gap"                  "10")
(set "frame_padding"              "0")
(set "smart_window_surroundings"  "0")
(set "smart_frame_surroundings"   "0")

(display "set window_border_active_color\n")
(set "window_border_active_color"  "\\#61afef")
(set "window_border_normal_color"  "\\#16161c")
(set "window_border_width"          "2")
(set "window_gap"          "0")

;; rules
(display "unrule\n")
(unrule)
(display "rule focus=off\n")
(rule "focus=off")                 ; normally do not focus new clients
(display "rule focus terminal\n")
(rule "class~'(.*[Rr]xvt.*|.*[Tt]erm|Konsole)' focus=on")
(rule "windowtype=_NET_WM_WINDOW_TYPE_DIALOG focus=on pseudotile=on")

;; Show panel

(system "~/.config/polybar/launch.sh &")
(system "~/.fehbg &")
