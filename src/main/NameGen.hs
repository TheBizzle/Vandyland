module NameGen(generateName) where

import Bizzlelude

import Data.List((!!))
import System.Random(randomRIO)

generateName :: IO Text
generateName = (\x y -> x <> " " <> y) <$> (randomOneOf adjectives) <*> (randomOneOf animals)

randomOneOf :: [a] -> IO a
randomOneOf xs =
  do
    index <- randomRIO (0, (length xs) - 1)
    return $ xs !! index

adjectives :: [Text]
adjectives = ["adult", "amazing", "american", "angry", "basic", "best", "big", "blue", "boring", "bouncing", "brave", "bright", "brown", "busy", "calm", "careful", "civil", "classic", "clean", "clear", "clever", "cold", "common", "complete", "complex", "confident", "confused", "cool", "correct", "crazy", "creative", "crying", "curious", "cute", "damp", "dangerous", "difficult", "dizzy", "dramatic", "eastern", "elder", "electric", "english", "entire", "exact", "expert", "extreme", "famous", "fast", "federal", "final", "financial", "foolish", "former", "free", "french", "friendly", "frightened", "full", "funny", "german", "glass", "golden", "good", "great", "green", "guilty", "happy", "healthy", "heavy", "helpful", "hidden", "honest", "huge", "human", "hungry", "important", "impossible", "incapable", "intelligent", "interesting", "invisible", "jumping", "junior", "kind", "known", "large", "last", "late", "leather", "little", "living", "local", "logical", "lonely", "lost", "loud", "low", "lucky", "mad", "major", "massive", "master", "maximum", "mean", "medical", "minor", "mobile", "mother", "mountain", "narrow", "nasty", "native", "natural", "negative", "new", "nice", "normal", "northern", "odd", "old", "one", "ordinary", "original", "patient", "perfect", "plastic", "pleasant", "popular", "positive", "powerful", "pretend", "primary", "proper", "proud", "purple", "quick", "quiet", "rare", "real", "red", "regular", "relevant", "remarkable", "resident", "responsible", "rich", "round", "royal", "russian", "sad", "salty", "scared", "scary", "secret", "senior", "serious", "short", "sick", "silly", "silver", "simple", "single", "sleeping", "slow", "sly", "small", "smart", "smooth", "soft", "solid", "sorry", "southern", "spanish", "special", "spiritual", "strange", "strict", "strong", "stupid", "successful", "sudden", "super", "sweet", "talking", "tall", "terrible", "tiny", "tired", "tough", "tricky", "ugly", "unfair", "unhappy", "unique", "unusual", "upset", "useful", "valuable", "warm", "wasteful", "weird", "western", "white", "wicked", "wild", "wise", "wonderful", "wooden", "yellow", "young"]

animals :: [Text]
animals    = ["ant", "ape", "bat", "bear", "beaver", "bee", "beetle", "bird", "bison", "boar", "buffalo", "bull", "bunny", "butterfly", "camel", "cat", "cheetah", "chimp", "chipmunk", "cobra", "cow", "coyote", "crab", "crane", "crow", "deer", "dingo", "dog", "dolphin", "dove", "dragon", "duck", "dwarf", "eagle", "elephant", "elf", "elk", "emu", "falcon", "fish", "flamingo", "flea", "fly", "fox", "frog", "gator", "gecko", "goat", "goose", "gorilla", "gull", "hawk", "hen", "hippo", "insect", "jackal", "jaguar", "kangaroo", "koala", "lamb", "lark", "lemming", "lion", "lizard", "mantis", "mole", "mongoose", "monkey", "moose", "mouse", "mule", "octopus", "otter", "owl", "ox", "panda", "parrot", "pig", "peacock", "puma", "rabbit", "raptor", "rat", "raven", "rhino", "robin", "robot", "seal", "shark", "sheep", "skunk", "sloth", "slug", "snail", "spider", "stingray", "stork", "swan", "tiger", "toad", "tortoise", "trout", "turkey", "turtle", "viper", "vulture", "weasel", "whale", "wolf", "wombat", "woodchuck", "woodpecker", "worm", "yak", "zebra"]
