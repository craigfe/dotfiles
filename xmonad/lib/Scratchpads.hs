module Scratchpads where

import XMonad.ManageHook

{-spotifyCommand = "spotify"-}
{-messengerCommand = "google-chrome --app=https://www.messenger.com/"-}
youtubeCommand = "google-chrome --app=https://www.youtube.com/"

isSpotify   = (className =? "Spotify")
{-isMessenger = (appName =? "www.messenger.com")-}
isYoutube = (appName =? "www.youtube.com")

scratchpads =
  [
  {-NS "spotify" spotifyCommand isSpotify (customFloating $ W.RationalRect (1/12) (1/12) (5/6) (5/6))-}
  {-, NS "messenger" messengerCommand isMessenger (customFloating $ W.RationalRect (1/12) (1/12) (5/6) (5/6))-}
  {-, NS "youtube" youtubeCommand isYoutube (customFloating $ W.RationalRect (31/48) (1/24) (8/24) (9/24))-}
  ]
