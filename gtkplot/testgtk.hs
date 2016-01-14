


import qualified Control.Concurrent as CC
import qualified Data.IORef as IORef
import qualified GHC.Stats
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import Text.Printf ( printf )
import Text.Read ( readMaybe )
import System.Glib.Signals ( on )


main = do

  statsEnabled <- GHC.Stats.getGCStatsEnabled

  _ <- Gtk.initGUI
  _ <- Gtk.timeoutAddFull (CC.yield >> return True) Gtk.priorityDefault 50

  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "HasKAL Plot"
                   ]

  statsLabel <- Gtk.labelNew (Nothing :: Maybe String)

  let statsWorker = do
        CC.threadDelay 500000
        msg <- if statsEnabled
               then do
                 stats <- GHC.Stats.getGCStats
                 return $ printf "The current memory usage is %.2f MB"
                   ((realToFrac (GHC.Stats.currentBytesUsed stats) :: Double) /(1024*1024))
               else return "(enable GHC statistics with +RTS -T)"
        Gtk.postGUISync $ Gtk.labelSetText statsLabel ("Welcome to HasKAL\n" ++ msg)
        statsWorker

  statsThread <- CC.forkIO statsWorker

  graphWindowsToBeKilled <- CC.newMVar []

  let killEverything = do
        CC.killThread statsThread
        gws <- CC.readMVar graphWindowsToBeKilled
        mapM_ Gtk.widgetDestroy gws
        Gtk.mainQuit

  _ <- Gtk.onDestroy win killEverything

newDataWidget :: ParamData
              -> CC.MVar [Gtk.Window]
              -> IO Gtk.VBox
newDataWidget param graphWindowsToBeKilled = do
  vbox <- Gtk.vBoxNew False 4
  nameBox' <- Gtk.hBoxNew False 4
  nameBox <- labeledWidget (dataName param) nameBox'
  buttonsBox <- Gtk.hBoxNew False 4


  -- button to clear history
  buttonAlsoDoNothing <- Gtk.buttonNewWithLabel "also do nothing"
  --  _ <- Gtk.onClicked buttonAlsoDoNothing $ do
  --    putStrLn "i promise, nothing happens"
  --    -- CC.modifyMVar_ logData (const (return S.empty))
  --    return ()
  let triggerYo action = Gtk.onClicked buttonAlsoDoNothing action >> return ()

  -- button to make a new graph
  buttonNew <- Gtk.buttonNewWithLabel "new graph"
  _ <- Gtk.onClicked buttonNew $ do
    graphWin <- newGraph
                triggerYo
                (dataName param)
                (chanSameSignalTree param)
                (chanToSignalTree param)
                (chanMsgStore param)

    -- add this window to the list to be killed on exit
    CC.modifyMVar_ graphWindowsToBeKilled (return . (graphWin:))


  -- entry to set history length
  entryAndLabel <- Gtk.hBoxNew False 4
  entryLabel <- Gtk.vBoxNew False 4 >>= labeledWidget "max history:"
  entryEntry <- Gtk.entryNew
  Gtk.set entryEntry [ Gtk.entryEditable := True
                     , Gtk.widgetSensitive := True
                       ]
  Gtk.entrySetText entryEntry "200"
  let updateMaxHistory = do
        txt <- Gtk.get entryEntry Gtk.entryText
        let reset = Gtk.entrySetText entryEntry "(max)"
        case readMaybe txt :: Maybe Int of
          Nothing ->
            putStrLn ("max history: couldn't make an Int out of \"" ++ show txt ++ "\"") >> reset
          Just 0  -> putStrLn ("max history: must be greater than 0") >> reset
          Just k  -> IORef.writeIORef (chanMaxHistory param) k

  _ <- on entryEntry Gtk.entryActivate updateMaxHistory
  updateMaxHistory


  Gtk.set entryAndLabel [ Gtk.containerChild := entryLabel
                        , Gtk.boxChildPacking entryLabel := Gtk.PackNatural
                        , Gtk.containerChild := entryEntry
                        , Gtk.boxChildPacking entryEntry := Gtk.PackNatural
                        ]


  -- put all the buttons/entries together
  Gtk.set buttonsBox [ Gtk.containerChild := buttonNew
                     , Gtk.boxChildPacking buttonNew := Gtk.PackNatural
                     , Gtk.containerChild := buttonAlsoDoNothing
                     , Gtk.boxChildPacking buttonAlsoDoNothing := Gtk.PackNatural
                     , Gtk.containerChild := entryAndLabel
                     , Gtk.boxChildPacking entryAndLabel := Gtk.PackNatural
                     ]

  Gtk.set vbox [ Gtk.containerChild := nameBox
               , Gtk.boxChildPacking   nameBox := Gtk.PackNatural
               , Gtk.containerChild := buttonsBox
               , Gtk.boxChildPacking   buttonsBox := Gtk.PackNatural
               ]

  return vbox



-- helper to make an hbox with a label
labeledWidget :: Gtk.WidgetClass a => String -> a -> IO Gtk.HBox
labeledWidget name widget = do
  label <- Gtk.labelNew (Just name)
  hbox <- Gtk.hBoxNew False 4
  Gtk.set hbox [ Gtk.containerChild := label
               , Gtk.containerChild := widget
               , Gtk.boxChildPacking label := Gtk.PackNatural
--               , Gtk.boxChildPacking widget := Gtk.PackNatural
               ]
