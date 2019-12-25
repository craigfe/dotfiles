module Projects where

import XMonad.Actions.DynamicProjects
import XMonad.Core
import XMonad.Layout
import XMonad.Operations

emailWorkspace = "E"

socialWorkspace = "A"

musicWorkspace = "S"

projectsWorkspaces = [emailWorkspace, socialWorkspace, musicWorkspace]

v :: XConfig a -> XConfig a
v =
  dynamicProjects
    [ Project
        { projectName = emailWorkspace,
          projectDirectory = "~/",
          projectStartHook = Just $ do
            spawn "firefox-developer-edition --app=https://webmail.hermes.cam.ac.uk"
        },
      Project
        { projectName = socialWorkspace,
          projectDirectory = "~/",
          projectStartHook = Just $ do
            sendMessage $ NextLayout
            spawn "firefox-developer-edition --app=https://www.messenger.com/"
            spawn "firefox-developer-edition --app=https://hangouts.google.com/?pli=1&authuser=1"
            spawn "firefox-developer-edition --app=https://ocamllabs.slack.com/"
            spawn "telegram-desktop"
        },
      Project
        { projectName = musicWorkspace,
          projectDirectory = "~/",
          projectStartHook = Just $ do
            spawn "spotify"
        }
    ]
