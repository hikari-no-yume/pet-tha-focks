import Haste (setTimer, Interval(Once))
import Haste.DOM (Elem, PropID, newElem, newTextElem, with, attr, (=:), addChild, documentBody, getAttr, setAttr)
import Haste.Events (MouseEvent(Click), onEvent)
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)

void :: IO a -> IO ()
void = (>>= (\_ -> return ()))

type HP = Double

data Effect = None | Love | Asleep | Alert Int | Bored | Sad | Cry | Dead

data State = State HP Effect

defaultState :: State
defaultState = State 0.75 Asleep

-- updates state, run every second
step :: State -> State
step (State hp effect) = State hp' effect'
    where
        hp' = case effect of
            Sad         -> hp * 0.9
            Cry         -> hp * 0.5
            Love        -> hp * 0.995
            Asleep      -> hp * 0.995
            _           -> hp * 0.99

        effect' = case effect of
            None
                | hp < 0.5  -> Bored
                | hp > 0.95 -> Love
                | otherwise -> None
            Love
                | hp < 0.9  -> None
                | otherwise -> Love
            Asleep
                | hp < 0.25 -> Sad
                | otherwise -> Asleep
            Alert 0 -> None
            Alert n -> Alert (n - 1)
            Bored
                | hp < 0.25 -> Sad
                | hp > 0.6  -> None
                | otherwise -> Bored
            Sad
                | hp < 0.1  -> Cry
                | hp > 0.75 -> None
                | otherwise -> Sad
            Cry
                | hp < 0.01 -> Dead
                | hp > 0.4  -> Sad
                | otherwise -> Cry
            Dead -> Dead

-- Updates state on click
click :: State -> State
click (State hp effect) = State hp' effect'
    where
        hp' = hp * 1.1 
        effect' = case effect of
            None        -> Alert 2
            Asleep      -> Alert 2
            Bored       -> Alert 2
            Dead        -> Dead
            _           -> effect

setIfDifferent :: Elem -> PropID -> String -> IO ()
setIfDifferent elem prop value = do
    curValue <- getAttr elem prop
    if curValue == value
        then return ()
        else setAttr elem prop value

updateDOM :: Elem -> Elem -> State -> IO ()
updateDOM img hpBar (State hp effect) = do
    setIfDifferent img "src" (case effect of
        None    -> "normal.gif"
        Love    -> "love.gif"
        Asleep  -> "sleep.gif"
        Alert _ -> "owo.gif"
        Bored   -> "bored.gif"
        Sad     -> "sad.gif"
        Cry     -> "cry.gif"
        Dead    -> "dead.gif")
    setIfDifferent img "alt" (case effect of
        None    -> "focks happy ^w^"
        Love    -> "focks super happy =w= <33"
        Asleep  -> "focks sleep .zZ"
        Alert _ -> "focks ears perk up owo"
        Bored   -> "focks bored '-'"
        Sad     -> "focks sad 'ʌ'"
        Cry     -> "focks cry ;;ʌ;;"
        Dead    -> "focks is dead, you monster, how could you")

    setAttr hpBar "value" $ show hp

process :: Elem -> Elem -> IORef State -> IO ()
process img hpBar stateRef = do
    modifyIORef' stateRef step
    updateDOM img hpBar =<< readIORef stateRef
    void $ setTimer (Once 1000) (process img hpBar stateRef)

processClick :: Elem -> Elem -> IORef State -> IO ()
processClick img hpBar stateRef = do
    modifyIORef' stateRef click
    updateDOM img hpBar =<< readIORef stateRef

main :: IO ()
main = do
    img <- newElem "img" `with` [
            attr "id" =: "game",
            attr "width" =: "64",
            attr "height" =: "64"
       ]

    hpLabel <- newElem "label"
    (flip addChild) hpLabel =<< newTextElem "happiness"
    (flip addChild) hpLabel =<< newElem "br"
            
    hpBar <- newElem "progress" `with` [
            attr "max" =: "1"
        ]
    (flip addChild) hpLabel hpBar
   
    stateRef <- newIORef defaultState

    updateDOM img hpBar =<< readIORef stateRef

    addChild img documentBody
    addChild hpLabel documentBody

    void $ img `onEvent` Click $ \_ -> processClick img hpBar stateRef
    void $ setTimer (Once 1000) (process img hpBar stateRef)

