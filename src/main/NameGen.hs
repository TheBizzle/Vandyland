module NameGen(generateName) where

import Bizzlelude

import Data.List((!!))
import System.Random(randomRIO)

generateName :: IO Text
generateName =
  do
    adjective <- randomOneOf adjectives
    animal    <- randomOneOf animals
    return $ adjective <> animal

randomOneOf :: [a] -> IO a
randomOneOf xs =
  do
    index <- randomRIO (0, (length xs) - 1)
    return $ xs !! index

adjectives = ["Adult", "Amazing", "American", "Angry", "Basic", "Best", "Big", "Blue", "Boring", "Brave", "Bright", "Brown", "Busy", "Calm", "Careful", "Civil", "Classic", "Clean", "Clear", "Cold", "Common", "Complete", "Complex", "Confident", "Cool", "Correct", "Crazy", "Creative", "Curious", "Cute", "Dangerous", "Difficult", "Dramatic", "Eastern", "Electric", "English", "Entire", "Exact", "Expert", "Extreme", "Famous", "Fast", "Federal", "Final", "Financial", "Former", "Free", "French", "Friendly", "Full", "Funny", "German", "Glass", "Golden", "Good", "Great", "Green", "Guilty", "Happy", "Healthy", "Heavy", "Helpful", "Honest", "Huge", "Human", "Hungry", "Important", "Impossible", "Incapable", "Intelligent", "Interesting", "Invisible", "Junior", "Kind", "Known", "Large", "Last", "Late", "Leather", "Little", "Living", "Local", "Logical", "Lonely", "Lost", "Loud", "Low", "Lucky", "Mad", "Major", "Massive", "Master", "Maximum", "Mean", "Medical", "Minor", "Mobile", "Mother", "Mountain", "Narrow", "Nasty", "Native", "Natural", "Negative", "New", "Nice", "Normal", "Northern", "Odd", "Old", "One", "Ordinary", "Original", "Patient", "Perfect", "Plastic", "Pleasant", "Popular", "Positive", "Powerful", "Pretend", "Primary", "Proper", "Proud", "Purple", "Quick", "Quiet", "Rare", "Real", "Red", "Regular", "Relevant", "Remarkable", "Resident", "Responsible", "Rich", "Round", "Royal", "Russian", "Sad", "Salty", "Scared", "Secret", "Senior", "Serious", "Short", "Sick", "Silly", "Silver", "Simple", "Single", "Sleeping", "Slow", "Small", "Smart", "Smooth", "Soft", "Solid", "Sorry", "Southern", "Spanish", "Special", "Spiritual", "Strange", "Strict", "Strong", "Stupid", "Successful", "Sudden", "Super", "Sweet", "Tall", "Terrible", "Tiny", "Tough", "Tricky", "Ugly", "Unfair", "Unhappy", "Unique", "Unusual", "Upset", "Useful", "Valuable", "Warm", "Wasteful", "Weird", "Western", "White", "Wild", "Wise", "Wonderful", "Wooden", "Yellow", "Young"]
animals    = ["Ant", "Bat", "Bear", "Beaver", "Bird", "Bison", "Boar", "Buffalo", "Butterfly", "Camel", "Cat", "Chipmunk", "Cobra", "Cow", "Coyote", "Crab", "Deer", "Dingo", "Dog", "Dolphin", "Dove", "Dragon", "Duck", "Dwarf", "Eagle", "Elephant", "Elk", "Falcon", "Fish", "Flamingo", "Fly", "Fox", "Frog", "Gator", "Gecko", "Goose", "Gorilla", "Gull", "Hawk", "Hen", "Hippo", "Insect", "Jackal", "Jaguar", "Kangaroo", "Koala", "Lark", "Lemming", "Lion", "Lizard", "Mantis", "Mongoose", "Monkey", "Moose", "Mouse", "Mule", "Otter", "Owl", "Ox", "Parrot", "Pig", "Peacock", "Puma", "Rabbit", "Rat", "Raven", "Rhino", "Robin", "Seal", "Shark", "Sheep", "Skunk", "Sloth", "Spider", "Stork", "Swan", "Tiger", "Tortoise", "Turkey", "Turtle", "Viper", "Vulture", "Whale", "Wolf", "Wombat", "Woodchuck", "Woodpecker", "Worm", "Yak", "Zebra"]
