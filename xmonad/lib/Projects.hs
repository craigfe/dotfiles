module Projects where

import           XMonad.Actions.DynamicProjects
import           XMonad.Core
import           XMonad.Layout
import           XMonad.Operations

emailWorkspace   = "E"
socialWorkspace  = "A"
projectWorkspace = "P"
musicWorkspace   = "S"
dissWorkspace    = "D"

projectsWorkspaces = [emailWorkspace, socialWorkspace, projectWorkspace, musicWorkspace, dissWorkspace]

projects =
  dynamicProjects [ Project { projectName = emailWorkspace
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                spawn "google-chrome --app=https://webmail.hermes.cam.ac.uk"
            }

  , Project { projectName = socialWorkspace
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                sendMessage $ NextLayout
                spawn "google-chrome --app=https://www.messenger.com/"
                spawn "google-chrome --app=https://hangouts.google.com/?pli=1&authuser=1"
                spawn "google-chrome --app=https://ocamllabs.slack.com/"
            }

  , Project { projectName = musicWorkspace
            , projectDirectory = "~/"
            , projectStartHook = Just $ do
                spawn "spotify"
            }

  , Project { projectName = projectWorkspace
            , projectDirectory = "~/repos/trace-rpc"
            , projectStartHook = Just $ do
                spawn "emacs ~/repos/trace-rpc"
            }

  , Project { projectName = dissWorkspace
            , projectDirectory = "~/repos/part2-dissertation"
            , projectStartHook = Just $ do
                spawn "em ~/repos/part2-dissertation"
            }
  ]
