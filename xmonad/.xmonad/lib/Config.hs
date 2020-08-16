module Config where


-- Tilde is not always expanded by XMonad.
myHome = "/home/craigfe"

myDotfiles = myHome ++ "/t/dotfiles"

myAccentFile = myDotfiles ++ "/colours/out/theme"

myBarInit = myHome ++ "/.config/polybar/start"

myLauncher = myHome ++ "/.config/rofi/menu/run"

mySystemMenu = myHome ++ "/.config/rofi/menu/system"

myCalendar = "google-chrome --app=https://calendar.google.com"

myCI = "alacritty --command '" ++ myHome ++ "/t/citty/run.sh'"

myEditor = "emacsclient --alternate-editor='emacs' --no-wait --create-frame"

myLock = myHome ++ "/.scripts/lock"

myPdfViewer = "zathura"

myScreensaver = "/usr/bin/gnome-screensaver-command --lock"

myScreenshot = "screenshot"

mySelectScreenshot = "screenshot_clipboard"

mySink = "alsa_output.pci-0000_00_1f.3.analog-stereo"

myTerminal = "alacritty"

myWebBrowser = "firefox-developer-edition"

myPrivateWebBrowser = "firefox-developer-edition --private-window"

myUnicodePrompt = myHome ++ "/.scripts/unicode"
