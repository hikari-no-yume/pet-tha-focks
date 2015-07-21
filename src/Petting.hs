import Haste (setTimer, Interval(Once))
import Haste.DOM (Elem, PropID, newElem, newTextElem, with, attr, (=:), addChild, documentBody, getAttr, setAttr)
import Haste.Events (MouseEvent(Click), onEvent)
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)

void :: IO a -> IO ()
void = (>>= (\_ -> return ()))

type HP = Double

-- Make sure HP doesn't go *too* far above the "maximum"
-- It would cap at 1.0, but people liked being able to go over
-- So we'll let them go over a bit, but not too far
capHp :: Double -> Double
capHp = min 1.5

data Effect = None | Love | Asleep | Alert Int | Eating Int | Bored | Sad | Cry | Dead

data State = State HP Effect

defaultState :: State
defaultState = State 0.75 Asleep

-- updates state, run every second
step :: State -> State
step (State hp effect) = State hp' effect'
    where
        hp' = capHp $ case effect of
            Sad         -> hp * 0.9
            Cry         -> hp * 0.5
            Love        -> hp * 0.995
            -- Note that eating, unlike all other effects, *increases* HP
            Eating _    -> hp * 1.05
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
            Eating 0 -> None
            Eating n -> Eating (n - 1)
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
        hp' = capHp $ hp * 1.1
        effect' = case effect of
            None        -> Alert 2
            Asleep      -> Alert 2
            Bored       -> Alert 2
            Dead        -> Dead
            _           -> effect

-- Updates state when "feed" clicked
feed :: State -> State
feed s@(State _ Dead) = s
feed (State hp _) = State hp (Eating 3)

-- Just a container with our DOM elements, so we don't have to pass three args
data Elems = Elems Elem Elem Elem

setIfDifferent :: Elem -> PropID -> String -> IO ()
setIfDifferent elem prop value = do
    curValue <- getAttr elem prop
    if curValue == value
        then return ()
        else setAttr elem prop value

updateDOM :: Elems -> State -> IO ()
updateDOM (Elems img hpBar favicon) (State hp effect) = do
    let src = case effect of
            None        -> "normal.gif"
            Love        -> "love.gif"
            Asleep      -> "sleep.gif"
            Alert _     -> "owo.gif"
            Eating _    -> "eat.gif"
            Bored       -> "bored.gif"
            Sad         -> "sad.gif"
            Cry         -> "cry.gif"
            Dead        -> "dead.gif"
    let alt = case effect of
            None        -> "focks happy ^w^"
            Love        -> "focks super happy =w= <33"
            Asleep      -> "focks sleep .zZ"
            Alert _     -> "focks ears perk up owo"
            Eating _    -> "focks eat *nom*"
            Bored       -> "focks bored '-'"
            Sad         -> "focks sad 'ʌ'"
            Cry         -> "focks cry ;;ʌ;;"
            Dead        -> "focks is dead, you monster, how could you"

    setIfDifferent img "src" src
    setIfDifferent favicon "href" src
    setIfDifferent img "alt" alt
    setIfDifferent img "title" alt

    setAttr hpBar "value" $ show hp

process :: Elems -> IORef State -> IO ()
process elems stateRef = do
    modifyIORef' stateRef step
    updateDOM elems =<< readIORef stateRef
    void $ setTimer (Once 1000) (process elems stateRef)

processClick :: Elems -> IORef State -> (State -> State) -> IO ()
processClick elems stateRef modifier = do
    modifyIORef' stateRef modifier
    updateDOM elems =<< readIORef stateRef

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

    favicon <- newElem "link" `with` [
            attr "rel" =: "shortcut icon"
        ]

    feedBtn <- newElem "button" `with` [
            attr "title" =: "feed focks a chicken nugget"
        ]
    (flip addChild) feedBtn =<< newElem "img" `with` [
            attr "src" =: "nugget.gif",
            attr "alt" =: ""
        ]
    (flip addChild) feedBtn =<< newTextElem " feed"

    stateRef <- newIORef defaultState

    let elems = Elems img hpBar favicon

    updateDOM elems =<< readIORef stateRef

    addChild img documentBody
    addChild hpLabel documentBody
    addChild favicon documentBody
    addChild feedBtn documentBody

    void $ img `onEvent` Click $ \_ -> processClick elems stateRef click
    void $ feedBtn `onEvent` Click $ \_ -> processClick elems stateRef feed
    void $ setTimer (Once 1000) (process elems stateRef)

