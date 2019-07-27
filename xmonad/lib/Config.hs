module Config where

myDotfiles          = "/home/craigfe/r/dotfiles"
myAccentFile        = myDotfiles ++ "/colours/out/theme"
myBarInit           = myDotfiles ++ "/polybar/start"
myLauncher          = myDotfiles ++ "/rofi/menu/run"
mySystemMenu        = myDotfiles ++ "/rofi/menu/system"

myCalendar          = "google-chrome --app=https://calendar.google.com"
myEditor            = "emacsclient --alternate-editor='emacs' --no-wait --create-frame"
myLock              = "~/craigfe/.scripts/lock"
myPdfViewer         = "zathura"
myScreensaver       = "/usr/bin/gnome-screensaver-command --lock"
myScreenshot        = "screenshot"
mySelectScreenshot  = "screenshot_clipboard"
mySink              = "alsa_output.pci-0000_00_1f.3.analog-stereo"
myTerminal          = "alacritty"
myWebBrowser        = "firefox-developer-edition"
myPrivateWebBrowser = "firefox-developer-edition --private-window"
