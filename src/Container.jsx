import { useEffect, useRef, useState } from "react"
import "../src/Container.css"
import TopComponents from "./TopComponents/TopComponents"
import TypingKeys from "./TypingKeys/TypingKeys"
import WordCollections from "./WordCollections/WordCollections"
import VerificationPrompt from "./verificationPrompt/verificationPrompt"
import { use } from "react"
function Container(){    
    const wordList = ["cigar", "rebut", "sissy", "humph", "awake", "blush", "focal", "evade", "naval", "serve", "heath", "dwarf", "model", "karma", "stink", "grade", "quiet", "bench", "abate", "feign", "major", "death", "fresh", "crust", "stool", "colon", "abase", "marry", "react", "batty", "pride", "floss", "helix", "croak", "staff", "paper", "unfed", "whelp", "trawl", "outdo", "adobe", "crazy", "sower", "repay", "digit", "crate", "cluck", "spike", "mimic", "pound", "maxim", "linen", "unmet", "flesh", "booby", "forth", "first", "stand", "belly", "ivory", "seedy", "print", "yearn", "drain", "bribe", "stout", "panel", "crass", "flume", "offal", "agree", "error", "swirl", "argue", "bleed", "delta", "flick", "totem", "wooer", "front", "shrub", "parry", "biome", "lapel", "start", "greet", "goner", "golem", "lusty", "loopy", "round", "audit", "lying", "gamma", "labor", "islet", "civic", "forge", "corny", "moult", "basic", "salad", "agate", "spicy", "spray", "essay", "fjord", "spend", "kebab", "guild", "aback", "motor", "alone", "hatch", "hyper", "thumb", "dowry", "ought", "belch", "dutch", "pilot", "tweed", "comet", "jaunt", "enema", "steed", "abyss", "growl", "fling", "dozen", "boozy", "erode", "world", "gouge", "click", "briar", "great", "altar", "pulpy", "blurt", "coast", "duchy", "groin", "fixer", "group", "rogue", "badly", "smart", "pithy", "gaudy", "chill", "heron", "vodka", "finer", "surer", "radio", "rouge", "perch", "retch", "wrote", "clock", "tilde", "store", "prove", "bring", "solve", "cheat", "grime", "exult", "usher", "epoch", "triad", "break", "rhino", "viral", "conic", "masse", "sonic", "vital", "trace", "using", "peach", "champ", "baton", "brake", "pluck", "craze", "gripe", "weary", "picky", "acute", "ferry", "aside", "tapir", "troll", "unify", "rebus", "boost", "truss", "siege", "tiger", "banal", "slump", "crank", "gorge", "query", "drink", "favor", "abbey", "tangy", "panic", "solar", "shire", "proxy", "point", "robot", "prick", "wince", "crimp", "knoll", "sugar", "whack", "mount", "perky", "could", "wrung", "light", "those", "moist", "shard", "pleat", "aloft", "skill", "elder", "frame", "humor", "pause", "ulcer", "ultra", "robin", "cynic", "aroma", "caulk", "shake", "dodge", "swill", "tacit", "other", "thorn", "trove", "bloke", "vivid", "spill", "chant", "choke", "rupee", "nasty", "mourn", "ahead", "brine", "cloth", "hoard", "sweet", "month", "lapse", "watch", "today", "focus", "smelt", "tease", "cater", "movie", "saute", "allow", "renew", "their", "slosh", "purge", "chest", "depot", "epoxy", "nymph", "found", "shall", "harry", "stove", "lowly", "snout", "trope", "fewer", "shawl", "natal", "comma", "foray", "scare", "stair", "black", "squad", "royal", "chunk", "mince", "shame", "cheek", "ample", "flair", "foyer", "cargo", "oxide", "plant", "olive", "inert", "askew", "heist", "shown", "zesty", "hasty", "trash", "fella", "larva", "forgo", "story", "hairy", "train", "homer", "badge", "midst", "canny", "fetus", "butch", "farce", "slung", "tipsy", "metal", "yield", "delve", "being", "scour", "glass", "gamer", "scrap", "money", "hinge", "album", "vouch", "asset", "tiara", "crept", "bayou", "atoll", "manor", "creak", "showy", "phase", "froth", "depth", "gloom", "flood", "trait", "girth", "piety", "payer", "goose", "float", "donor", "atone", "primo", "apron", "blown", "cacao", "loser", "input", "gloat", "awful", "brink", "smite", "beady", "rusty", "retro", "droll", "gawky", "hutch", "pinto", "gaily", "egret", "lilac", "sever", "field", "fluff", "hydro", "flack", "agape", "voice", "stead", "stalk", "berth", "madam", "night", "bland", "liver", "wedge", "augur", "roomy", "wacky", "flock", "angry", "bobby", "trite", "aphid", "tryst", "midge", "power", "elope", "cinch", "motto", "stomp", "upset", "bluff", "cramp", "quart", "coyly", "youth", "rhyme", "buggy", "alien", "smear", "unfit", "patty", "cling", "glean", "label", "hunky", "khaki", "poker", "gruel", "twice", "twang", "shrug", "treat", "unlit", "waste", "merit", "woven", "octal", "needy", "clown", "widow", "irony", "ruder", "gauze", "chief", "onset", "prize", "fungi", "charm", "gully", "inter", "whoop", "taunt", "leery", "class", "theme", "lofty", "tibia", "booze", "alpha", "thyme", "eclat", "doubt", "parer", "chute", "stick", "trice", "alike", "sooth", "recap", "saint", "liege", "glory", "grate", "admit", "brisk", "soggy", "usurp", "scald", "scorn", "leave", "twine", "sting", "bough", "marsh", "sloth", "dandy", "vigor", "howdy", "enjoy", "valid", "ionic", "equal", "unset", "floor", "catch", "spade", "stein", "exist", "quirk", "denim", "grove", "spiel", "mummy", "fault", "foggy", "flout", "carry", "sneak", "libel", "waltz", "aptly", "piney", "inept", "aloud", "photo", "dream", "stale", "vomit", "ombre", "fanny", "unite", "snarl", "baker", "there", "glyph", "pooch", "hippy", "spell", "folly", "louse", "gulch", "vault", "godly", "threw", "fleet", "grave", "inane", "shock", "crave", "spite", "valve", "skimp", "claim", "rainy", "musty", "pique", "daddy", "quasi", "arise", "aging", "valet", "opium", "avert", "stuck", "recut", "mulch", "genre", "plume", "rifle", "count", "incur", "total", "wrest", "mocha", "deter", "study", "lover", "safer", "rivet", "funny", "smoke", "mound", "undue", "sedan", "pagan", "swine", "guile", "gusty", "equip", "tough", "canoe", "chaos", "covet", "human", "udder", "lunch", "blast", "stray", "manga", "melee", "lefty", "quick", "paste", "given", "octet", "risen", "groan", "leaky", "grind", "carve", "loose", "sadly", "spilt", "apple", "slack", "honey", "final", "sheen", "eerie", "minty", "slick", "derby", "wharf", "spelt", "coach", "erupt", "singe", "price", "spawn", "fairy", "jiffy", "filmy", "stack", "chose", "sleep", "ardor", "nanny", "niece", "woozy", "handy", "grace", "ditto", "stank", "cream", "usual", "diode", "valor", "angle", "ninja", "muddy", "chase", "reply", "prone", "spoil", "heart", "shade", "diner", "arson", "onion", "sleet", "dowel", "couch", "palsy", "bowel", "smile", "evoke", "creek", "lance", "eagle", "idiot", "siren", "built", "embed", "award", "dross", "annul", "goody", "frown", "patio", "laden", "humid", "elite", "lymph", "edify", "might", "reset", "visit", "gusto", "purse", "vapor", "crock", "write", "sunny", "loath", "chaff", "slide", "queer", "venom", "stamp", "sorry", "still", "acorn", "aping", "pushy", "tamer", "hater", "mania", "awoke", "brawn", "swift", "exile", "birch", "lucky", "freer", "risky", "ghost", "plier", "lunar", "winch", "snare", "nurse", "house", "borax", "nicer", "lurch", "exalt", "about", "savvy", "toxin", "tunic", "pried", "inlay", "chump", "lanky", "cress", "eater", "elude", "cycle", "kitty", "boule", "moron", "tenet", "place", "lobby", "plush", "vigil", "index", "blink", "clung", "qualm", "croup", "clink", "juicy", "stage", "decay", "nerve", "flier", "shaft", "crook", "clean", "china", "ridge", "vowel", "gnome", "snuck", "icing", "spiny", "rigor", "snail", "flown", "rabid", "prose", "thank", "poppy", "budge", "fiber", "moldy", "dowdy", "kneel", "track", "caddy", "quell", "dumpy", "paler", "swore", "rebar", "scuba", "splat", "flyer", "horny", "mason", "doing", "ozone", "amply", "molar", "ovary", "beset", "queue", "cliff", "magic", "truce", "sport", "fritz", "edict", "twirl", "verse", "llama", "eaten", "range", "whisk", "hovel", "rehab", "macaw", "sigma", "spout", "verve", "sushi", "dying", "fetid", "brain", "buddy", "thump", "scion", "candy", "chord", "basin", "march", "crowd", "arbor", "gayly", "musky", "stain", "dally", "bless", "bravo", "stung", "title", "ruler", "kiosk", "blond", "ennui", "layer", "fluid", "tatty", "score", "cutie", "zebra", "barge", "matey", "bluer", "aider", "shook", "river", "privy", "betel", "frisk", "bongo", "begun", "azure", "weave", "genie", "sound", "glove", "braid", "scope", "wryly", "rover", "assay", "ocean", "bloom", "irate", "later", "woken", "silky", "wreck", "dwelt", "slate", "smack", "solid", "amaze", "hazel", "wrist", "jolly", "globe", "flint", "rouse", "civil", "vista", "relax", "cover", "alive", "beech", "jetty", "bliss", "vocal", "often", "dolly", "eight", "joker", "since", "event", "ensue", "shunt", "diver", "poser", "worst", "sweep", "alley", "creed", "anime", "leafy", "bosom", "dunce", "stare", "pudgy", "waive", "choir", "stood", "spoke", "outgo", "delay", "bilge", "ideal", "clasp", "seize", "hotly", "laugh", "sieve", "block", "meant", "grape", "noose", "hardy", "shied", "drawl", "daisy", "putty", "strut", "burnt", "tulip", "crick", "idyll", "vixen", "furor", "geeky", "cough", "naive", "shoal", "stork", "bathe", "aunty", "check", "prime", "brass", "outer", "furry", "razor", "elect", "evict", "imply", "demur", "quota", "haven", "cavil", "swear", "crump", "dough", "gavel", "wagon", "salon", "nudge", "harem", "pitch", "sworn", "pupil", "excel", "stony", "cabin", "unzip", "queen", "trout", "polyp", "earth", "storm", "until", "taper", "enter", "child", "adopt", "minor", "fatty", "husky", "brave", "filet", "slime", "glint", "tread", "steal", "regal", "guest", "every", "murky", "share", "spore", "hoist", "buxom", "inner", "otter", "dimly", "level", "sumac", "donut", "stilt", "arena", "sheet", "scrub", "fancy", "slimy", "pearl", "silly", "porch", "dingo", "sepia", "amble", "shady", "bread", "friar", "reign", "dairy", "quill", "cross", "brood", "tuber", "shear", "posit", "blank", "villa", "shank", "piggy", "freak", "which", "among", "fecal", "shell", "would", "algae", "large", "rabbi", "agony", "amuse", "bushy", "copse", "swoon", "knife", "pouch", "ascot", "plane", "crown", "urban", "snide", "relay", "abide", "viola", "rajah", "straw", "dilly", "crash", "amass", "third", "trick", "tutor", "woody", "blurb", "grief", "disco", "where", "sassy", "beach", "sauna", "comic", "clued", "creep", "caste", "graze", "snuff", "frock", "gonad", "drunk", "prong", "lurid", "steel", "halve", "buyer", "vinyl", "utile", "smell", "adage", "worry", "tasty", "local", "trade", "finch", "ashen", "modal", "gaunt", "clove", "enact", "adorn", "roast", "speck", "sheik", "missy", "grunt", "snoop", "party", "touch", "mafia", "emcee", "array", "south", "vapid", "jelly", "skulk", "angst", "tubal", "lower", "crest", "sweat", "cyber", "adore", "tardy", "swami", "notch", "groom", "roach", "hitch", "young", "align", "ready", "frond", "strap", "puree", "realm", "venue", "swarm", "offer", "seven", "dryer", "diary", "dryly", "drank", "acrid", "heady", "theta", "junto", "pixie", "quoth", "bonus", "shalt", "penne", "amend", "datum", "build", "piano", "shelf", "lodge", "suing", "rearm", "coral", "ramen", "worth", "psalm", "infer", "overt", "mayor", "ovoid", "glide", "usage", "poise", "randy", "chuck", "prank", "fishy", "tooth", "ether", "drove", "idler", "swath", "stint", "while", "begat", "apply", "slang", "tarot", "radar", "credo", "aware", "canon", "shift", "timer", "bylaw", "serum", "three", "steak", "iliac", "shirk", "blunt", "puppy", "penal", "joist", "bunny", "shape", "beget", "wheel", "adept", "stunt", "stole", "topaz", "chore", "fluke", "afoot", "bloat", "bully", "dense", "caper", "sneer", "boxer", "jumbo", "lunge", "space", "avail", "short", "slurp", "loyal", "flirt", "pizza", "conch", "tempo", "droop", "plate", "bible", "plunk", "afoul", "savoy", "steep", "agile", "stake", "dwell", "knave", "beard", "arose", "motif", "smash", "broil", "glare", "shove", "baggy", "mammy", "swamp", "along", "rugby", "wager", "quack", "squat", "snaky", "debit", "mange", "skate", "ninth", "joust", "tramp", "spurn", "medal", "micro", "rebel", "flank", "learn", "nadir", "maple", "comfy", "remit", "gruff", "ester", "least", "mogul", "fetch", "cause", "oaken", "aglow", "meaty", "gaffe", "shyly", "racer", "prowl", "thief", "stern", "poesy", "rocky", "tweet", "waist", "spire", "grope", "havoc", "patsy", "truly", "forty", "deity", "uncle", "swish", "giver", "preen", "bevel", "lemur", "draft", "slope", "annoy", "lingo", "bleak", "ditty", "curly", "cedar", "dirge", "grown", "horde", "drool", "shuck", "crypt", "cumin", "stock", "gravy", "locus", "wider", "breed", "quite", "chafe", "cache", "blimp", "deign", "fiend", "logic", "cheap", "elide", "rigid", "false", "renal", "pence", "rowdy", "shoot", "blaze", "envoy", "posse", "brief", "never", "abort", "mouse", "mucky", "sulky", "fiery", "media", "trunk", "yeast", "clear", "skunk", "scalp", "bitty", "cider", "koala", "duvet", "segue", "creme", "super", "grill", "after", "owner", "ember", "reach", "nobly", "empty", "speed", "gipsy", "recur", "smock", "dread", "merge", "burst", "kappa", "amity", "shaky", "hover", "carol", "snort", "synod", "faint", "haunt", "flour", "chair", "detox", "shrew", "tense", "plied", "quark", "burly", "novel", "waxen", "stoic", "jerky", "blitz", "beefy", "lyric", "hussy", "towel", "quilt", "below", "bingo", "wispy", "brash", "scone", "toast", "easel", "saucy", "value", "spice", "honor", "route", "sharp", "bawdy", "radii", "skull", "phony", "issue", "lager", "swell", "urine", "gassy", "trial", "flora", "upper", "latch", "wight", "brick", "retry", "holly", "decal", "grass", "shack", "dogma", "mover", "defer", "sober", "optic", "crier", "vying", "nomad", "flute", "hippo", "shark", "drier", "obese", "bugle", "tawny", "chalk", "feast", "ruddy", "pedal", "scarf", "cruel", "bleat", "tidal", "slush", "semen", "windy", "dusty", "sally", "igloo", "nerdy", "jewel", "shone", "whale", "hymen", "abuse", "fugue", "elbow", "crumb", "pansy", "welsh", "syrup", "terse", "suave", "gamut", "swung", "drake", "freed", "afire", "shirt", "grout", "oddly", "tithe", "plaid", "dummy", "broom", "blind", "torch", "enemy", "again", "tying", "pesky", "alter", "gazer", "noble", "ethos", "bride", "extol", "decor", "hobby", "beast", "idiom", "utter", "these", "sixth", "alarm", "erase", "elegy", "spunk", "piper", "scaly", "scold", "hefty", "chick", "sooty", "canal", "whiny", "slash", "quake", "joint", "swept", "prude", "heavy", "wield", "femme", "lasso", "maize", "shale", "screw", "spree", "smoky", "whiff", "scent", "glade", "spent", "prism", "stoke", "riper", "orbit", "cocoa", "guilt", "humus", "shush", "table", "smirk", "wrong", "noisy", "alert", "shiny", "elate", "resin", "whole", "hunch", "pixel", "polar", "hotel", "sword", "cleat", "mango", "rumba", "puffy", "filly", "billy", "leash", "clout", "dance", "ovate", "facet", "chili", "paint", "liner", "curio", "salty", "audio", "snake", "fable", "cloak", "navel", "spurt", "pesto", "balmy", "flash", "unwed", "early", "churn", "weedy", "stump", "lease", "witty", "wimpy", "spoof", "saner", "blend", "salsa", "thick", "warty", "manic", "blare", "squib", "spoon", "probe", "crepe", "knack", "force", "debut", "order", "haste", "teeth", "agent", "widen", "icily", "slice", "ingot", "clash", "juror", "blood", "abode", "throw", "unity", "pivot", "slept", "troop", "spare", "sewer", "parse", "morph", "cacti", "tacky", "spool", "demon", "moody", "annex", "begin", "fuzzy", "patch", "water", "lumpy", "admin", "omega", "limit", "tabby", "macho", "aisle", "skiff", "basis", "plank", "verge", "botch", "crawl", "lousy", "slain", "cubic", "raise", "wrack", "guide", "foist", "cameo", "under", "actor", "revue", "fraud", "harpy", "scoop", "climb", "refer", "olden", "clerk", "debar", "tally", "ethic", "cairn", "tulle", "ghoul", "hilly", "crude", "apart", "scale", "older", "plain", "sperm", "briny", "abbot", "rerun", "quest", "crisp", "bound", "befit", "drawn", "suite", "itchy", "cheer", "bagel", "guess", "broad", "axiom", "chard", "caput", "leant", "harsh", "curse", "proud", "swing", "opine", "taste", "lupus", "gumbo", "miner", "green", "chasm", "lipid", "topic", "armor", "brush", "crane", "mural", "abled", "habit", "bossy", "maker", "dusky", "dizzy", "lithe", "brook", "jazzy", "fifty", "sense", "giant", "surly", "legal", "fatal", "flunk", "began", "prune", "small", "slant", "scoff", "torus", "ninny", "covey", "viper", "taken", "moral", "vogue", "owing", "token", "entry", "booth", "voter", "chide", "elfin", "ebony", "neigh", "minim", "melon", "kneed", "decoy", "voila", "ankle", "arrow", "mushy", "tribe", "cease", "eager", "birth", "graph", "odder", "terra", "weird", "tried", "clack", "color", "rough", "weigh", "uncut", "ladle", "strip", "craft", "minus", "dicey", "titan", "lucid", "vicar", "dress", "ditch", "gypsy", "pasta", "taffy", "flame", "swoop", "aloof", "sight", "broke", "teary", "chart", "sixty", "wordy", "sheer", "leper", "nosey", "bulge", "savor", "clamp", "funky", "foamy", "toxic", "brand", "plumb", "dingy", "butte", "drill", "tripe", "bicep", "tenor", "krill", "worse", "drama", "hyena", "think", "ratio", "cobra", "basil", "scrum", "bused", "phone", "court", "camel", "proof", "heard", "angel", "petal", "pouty", "throb", "maybe", "fetal", "sprig", "spine", "shout", "cadet", "macro", "dodgy", "satyr", "rarer", "binge", "trend", "nutty", "leapt", "amiss", "split", "myrrh", "width", "sonar", "tower", "baron", "fever", "waver", "spark", "belie", "sloop", "expel", "smote", "baler", "above", "north", "wafer", "scant", "frill", "awash", "snack", "scowl", "frail", "drift", "limbo", "fence", "motel", "ounce", "wreak", "revel", "talon", "prior", "knelt", "cello", "flake", "debug", "anode", "crime", "salve", "scout", "imbue", "pinky", "stave", "vague", "chock", "fight", "video", "stone", "teach", "cleft", "frost", "prawn", "booty", "twist", "apnea", "stiff", "plaza", "ledge", "tweak", "board", "grant", "medic", "bacon", "cable", "brawl", "slunk", "raspy", "forum", "drone", "women", "mucus", "boast", "toddy", "coven", "tumor", "truer", "wrath", "stall", "steam", "axial", "purer", "daily", "trail", "niche", "mealy", "juice", "nylon", "plump", "merry", "flail", "papal", "wheat", "berry", "cower", "erect", "brute", "leggy", "snipe", "sinew", "skier", "penny", "jumpy", "rally", "umbra", "scary", "modem", "gross", "avian", "greed", "satin", "tonic", "parka", "sniff", "livid", "stark", "trump", "giddy", "reuse", "taboo", "avoid", "quote", "devil", "liken", "gloss", "gayer", "beret", "noise", "gland", "dealt", "sling", "rumor", "opera", "thigh", "tonga", "flare", "wound", "white", "bulky", "etude", "horse", "circa", "paddy", "inbox", "fizzy", "grain", "exert", "surge", "gleam", "belle", "salvo", "crush", "fruit", "sappy", "taker", "tract", "ovine", "spiky", "frank", "reedy", "filth", "spasm", "heave", "mambo", "right", "clank", "trust", "lumen", "borne", "spook", "sauce", "amber", "lathe", "carat", "corer", "dirty", "slyly", "affix", "alloy", "taint", "sheep", "kinky", "wooly", "mauve", "flung", "yacht", "fried", "quail", "brunt", "grimy", "curvy", "cagey", "rinse", "deuce", "state", "grasp", "milky", "bison", "graft", "sandy", "baste", "flask", "hedge", "girly", "swash", "boney", "coupe", "endow", "abhor", "welch", "blade", "tight", "geese", "miser", "mirth", "cloud", "cabal", "leech", "close", "tenth", "pecan", "droit", "grail", "clone", "guise", "ralph", "tango", "biddy", "smith", "mower", "payee", "serif", "drape", "fifth", "spank", "glaze", "allot", "truck", "kayak", "virus", "testy", "tepee", "fully", "zonal", "metro", "curry", "grand", "banjo", "axion", "bezel", "occur", "chain", "nasal", "gooey", "filer", "brace", "allay", "pubic", "raven", "plead", "gnash", "flaky", "munch", "dully", "eking", "thing", "slink", "hurry", "theft", "shorn", "pygmy", "ranch", "wring", "lemon", "shore", "mamma", "froze", "newer", "style", "moose", "antic", "drown", "vegan", "chess", "guppy", "union", "lever", "lorry", "image", "cabby", "druid", "exact", "truth", "dopey", "spear", "cried", "chime", "crony", "stunk", "timid", "batch", "gauge", "rotor", "crack", "curve", "latte", "witch", "bunch", "repel", "anvil", "soapy", "meter", "broth", "madly", "dried", "scene", "known", "magma", "roost", "woman", "thong", "punch", "pasty", "downy", "knead", "whirl", "rapid", "clang", "anger", "drive", "goofy", "email", "music", "stuff", "bleep", "rider", "mecca", "folio", "setup", "verso", "quash", "fauna", "gummy", "happy", "newly", "fussy", "relic", "guava", "ratty", "fudge", "femur", "chirp", "forte", "alibi", "whine", "petty", "golly", "plait", "fleck", "felon", "gourd", "brown", "thrum", "ficus", "stash", "decry", "wiser", "junta", "visor", "daunt", "scree", "impel", "await", "press", "whose", "turbo", "stoop", "speak", "mangy", "eying", "inlet", "crone", "pulse", "mossy", "staid", "hence", "pinch", "teddy", "sully", "snore", "ripen", "snowy", "attic", "going", "leach", "mouth", "hound", "clump", "tonal", "bigot", "peril", "piece", "blame", "haute", "spied", "undid", "intro", "basal", "shine", "gecko", "rodeo", "guard", "steer", "loamy", "scamp", "scram", "manly", "hello", "vaunt", "organ", "feral", "knock", "extra", "condo", "adapt", "willy", "polka", "rayon", "skirt", "faith", "torso", "match", "mercy", "tepid", "sleek", "riser", "twixt", "peace", "flush", "catty", "login", "eject", "roger", "rival", "untie", "refit", "aorta", "adult", "judge", "rower", "artsy", "rural", "shave"]

    const [showVerificationPrompt, setShowVerificationPrompt] = useState()
    const [takeHint, setTakeHint] = useState(false)
    const [page, setPage] = useState(1)
    const [row, setRow] = useState(null)
    const [tile, setTiles] = useState(null)
    const [gameState, setGameState] = useState(false)
    const [highScore, setHighScore] = useState(JSON.parse(localStorage.getItem("HighScore")) ? JSON.parse(localStorage.getItem("HighScore")) : 0)
    const [isCorrect, setIsCorrect] = useState(false)
    const [score, setScore] = useState(null)
    const [showAnswer, setShowAnswer] = useState(null)
    const [started, setStarted] = useState(false)
    const [gold, setGold] = useState(JSON.parse(localStorage.getItem("goldStorage")) ? JSON.parse(localStorage.getItem("goldStorage")) : 0)
    const [showCollections, setShowCollection]= useState(false)
    const [collections, setCollections] = useState(JSON.parse(localStorage.getItem("CollectedWords")) ? JSON.parse(localStorage.getItem("CollectedWords")) : [])

    const [randomNum, setRandomNum] = useState()
    const [randomIndices, setRandomIndices] = useState(["1", "2", "3", "4", "0"])
    
    const SubmitButton = useRef(null)
    const StartButton = useRef(null)

    const Keys = [
        ["q", "w", "e", 'r', "t", "y", "u", "i", "o", "p"],
        ["a", "s", "d", "f", 'g', "h", "j", "k", "l", "⌫"],
        ["z", "x", "c", "v", 'b', "n", "m"]
    ]
// Labels and DOM Manipulation

    const row1 = useRef(null)
    const row2 = useRef(null)
    const row3 = useRef(null)
    const row4 = useRef(null)
    const row5 = useRef(null)
    const row6 = useRef(null)

    const tile1 = useRef()
    const tile2 = useRef()
    const tile3 = useRef()
    const tile4 = useRef()
    const tile5 = useRef()
    const tile6 = useRef()
    const tile7 = useRef()
    const tile8 = useRef()
    const tile9 = useRef()
    const tile10 = useRef()
    const tile11 = useRef()
    const tile12 = useRef()
    const tile13 = useRef()
    const tile14 = useRef()
    const tile15 = useRef()
    const tile16 = useRef()
    const tile17 = useRef()
    const tile18 = useRef()
    const tile19 = useRef()
    const tile20 = useRef()
    const tile21 = useRef()
    const tile22 = useRef()
    const tile23 = useRef()
    const tile24 = useRef()
    const tile25 = useRef()
    const tile26 = useRef()
    const tile27 = useRef()
    const tile28 = useRef()
    const tile29 = useRef()
    const tile30 = useRef()

    const TypingKeys1 = useRef()
    const TypingKeys2 = useRef()
    const TypingKeys3 = useRef()
    const TypingKeys4 = useRef()
    const TypingKeys5 = useRef()
    const TypingKeys6 = useRef()
    const TypingKeys7 = useRef()
    const TypingKeys8 = useRef()
    const TypingKeys9 = useRef()
    const TypingKeys10 = useRef()
    const TypingKeys11 = useRef()
    const TypingKeys12 = useRef()
    const TypingKeys13 = useRef()
    const TypingKeys14 = useRef()
    const TypingKeys15 = useRef()
    const TypingKeys16 = useRef()
    const TypingKeys17 = useRef()
    const TypingKeys18 = useRef()
    const TypingKeys19 = useRef()
    const TypingKeys20 = useRef()
    const TypingKeys21 = useRef()
    const TypingKeys22 = useRef()
    const TypingKeys23 = useRef()
    const TypingKeys24 = useRef()
    const TypingKeys25 = useRef()
    const TypingKeys26 = useRef()
    const TypingKeys27 = useRef()

    const ArrayOfTypingKeys = [
        [TypingKeys1, TypingKeys2, TypingKeys3, TypingKeys4, TypingKeys5, TypingKeys6, TypingKeys7, TypingKeys8, TypingKeys9, TypingKeys10],
        [TypingKeys11, TypingKeys12, TypingKeys13, TypingKeys14, TypingKeys15, TypingKeys16, TypingKeys17, TypingKeys18, TypingKeys19, TypingKeys20],
        [TypingKeys21, TypingKeys22, TypingKeys23, TypingKeys24, TypingKeys25, TypingKeys26, TypingKeys27]
    ]
    const arrOfRowOfTiles1 = [
        tile1, tile2, tile3, tile4, tile5
    ]
    const arrOfRowOfTiles2 = [
        tile6, tile7, tile8, tile9, tile10
    ]
    const arrOfRowOfTiles3 = [
        tile11, tile12, tile13, tile14, tile15
    ]
    const arrOfRowOfTiles4 = [
        tile16, tile17, tile18, tile19, tile20
    ]
    const arrOfRowOfTiles5 = [
        tile21, tile22, tile23, tile24, tile25
    ]
    const arrOfRowOfTiles6 = [
        tile26, tile27, tile28, tile29, tile30
    ]
    const ArrayOftiles = [ tile1, tile2, tile3, tile4, tile5, tile6, tile7, tile8, tile9, tile10, tile11, tile12, tile13, tile14, tile15, tile16, tile17, tile18, tile19, tile20, tile21, tile22, tile23, tile24, tile25, tile26, tile27, tile28, tile29, tile30]
    window.onkeydown = (e)=>{if(gameState && tile1.current != null){DefineTiles(e)}}
    const KeyType = (e) => {if(tile1.current != null){DefineTiles(e)}}

    function Start(){
        if(!gameState){
            setRow(1)
            setTiles(0)
            setScore(0)
            setPage(1)
            setGameState(true)
            setIsCorrect(false)
            HandleRandomWord()
        } 
    }

    function newRound(){
        setPage(1)
        setGameState(true)
        clearBoard()
        HandleRandomWord()
        setIsCorrect(false)
    }

    const HandleRandomWord = () => {
        localStorage.setItem("Word", JSON.stringify(wordList[Math.floor(Math.random() * wordList.length) -1].toUpperCase()))
        console.log(JSON.parse(localStorage.getItem("Word")))
    }

    function DefineTiles(e){
        const letters = [
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
            's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
        ]
        let key
        e.key != null ? key = e.key : key = e

        for(let i = 0; i < letters.length; i++){
            if(row < 7){
                if((key == letters[i]) && tile != row*5){
                    for(let j = tile; j < row*5; j++){
                        if(ArrayOftiles[j].current.classList.contains("hintedTile")){
                            continue
                        } else {
                            ArrayOftiles[j].current.classList.add("highlightTile")
                            ArrayOftiles[j].current.textContent = key.toUpperCase()
                            setTiles(tile + (j-(tile - 1)))

                            for(let z = j; z != row*5; z++){
                                if(ArrayOftiles[z+1] != null){
                                    if(ArrayOftiles[z+1].current.classList.contains("hintedTile")){
                                        continue
                                    }  else {
                                        setTiles(z + 1)
                                        break
                                        
                                    }
                                } else {
                                    setTiles(row*5)
                                }
                                
                            }
                            break
                        }
                    }
                } else if((key == "⌫" || key == "Backspace") && tile != (row*5)-5) {
                    for(let j = tile; j > row*5-5; j--){
                        if(ArrayOftiles[j-1].current.classList.contains("hintedTile")){
                            continue
                        } else {
                            ArrayOftiles[j-1].current.textContent = ""
                            ArrayOftiles[j-1].current.classList.remove("highlightTile")
                            setTiles(j - 1)
                            break
                        }
                    }
                }
            }
        }
        
    }

    function CheckAnswer(e){
        var Guess = ""
        if(e.key === "Enter" || e == "Enter"){
            if(row <=6 && tile === row * 5){ //Simplified row check
                for(let i = tile-5; i < tile; i++){
                    if(ArrayOftiles[i].current != null){Guess += ArrayOftiles[i].current.textContent}
                    Guess.toString()
                }
                CheckGuess(Guess)
            }
        }
    }

    const deleteTiles = (index) => {
        setTiles(index + ((row*5)-5))
        for(let i = index + ((row*5)-5); i < row*5; i++){
            if(ArrayOftiles[i].current.classList.contains("highlightTile")){
                ArrayOftiles[i].current.classList.remove("highlightTile")
                ArrayOftiles[i].current.textContent = ""
            }
        }

    }

    function CheckGuess(guess){
        let Word = JSON.parse(localStorage.getItem("Word"))
        if(guess.length == 5 && guess.length != 0){
            
            if(guess.toUpperCase() == Word.toUpperCase()){
                for(let j = tile-5; j < tile; j++){
                    setTimeout(() => {
                        if(ArrayOftiles[j].current.classList.contains("hintedTile")){
                            ArrayOftiles[j].current.classList.replace("hintedTile", "flipGreen")
                            ChangeTypingKeyColor()
                        } else {
                            ArrayOftiles[j].current.classList.add( "flipGreen")
                            ChangeTypingKeyColor()
                        }
                    }, 100*(j-(tile-5)));

                    function ChangeTypingKeyColor(){
                        for(let row = 0; row < ArrayOfTypingKeys.length; row++){
                            for(let each = 0; each < ArrayOfTypingKeys[row].length; each++){
                                if(ArrayOftiles[j].current.textContent == ArrayOfTypingKeys[row][each].current.textContent.toUpperCase()){
                                    ArrayOfTypingKeys[row][each].current.style.backgroundColor = "green"
                                } 
                            }
                        }
                    }
                }
                if(Word != null){GameUpdate(guess, Word.toUpperCase())}
                setTimeout(() => {
                    setPage(2)
                    setIsCorrect(true)
                    setShowAnswer(true)
                }, 2000);
            } else if (guess.toUpperCase() != Word.toUpperCase()){
                for(var j = tile - 5; j < tile; j++){
                    for(let i = 0; i < 5; i++){
                        if(Word[(j+5)-tile] === ArrayOftiles[j].current.textContent ){
                            
                            if(ArrayOftiles[j].current.classList.contains("hintedTile")){
                                ArrayOftiles[j].current.classList.replace("hintedTile", "flipGreen")
                            } else {
                                ArrayOftiles[j].current.classList.add("flipGreen")
                            }

                            for(let row = 0; row < ArrayOfTypingKeys.length; row++){
                                for(let each = 0; each < ArrayOfTypingKeys[row].length; each++){
                                    if(ArrayOftiles[j].current.textContent == ArrayOfTypingKeys[row][each].current.textContent.toUpperCase()){
                                        ArrayOfTypingKeys[row][each].current.style.backgroundColor = "green"
                                        break
                                    } 
                                }
                            }
                            
                            break
                            
                        } 
                        else if(ArrayOftiles[j].current.textContent == Word[i]){
                            
                            if(ArrayOftiles[j].current.classList.contains("flipGray")){
                                ArrayOftiles[j].current.classList.replace("flipGray", "flipYellow")
                            } else {
                                ArrayOftiles[j].current.classList.add("flipYellow")
                            }

                            for(let row = 0; row < ArrayOfTypingKeys.length; row++){
                                for(let each = 0; each < ArrayOfTypingKeys[row].length; each++){
                                    if((ArrayOftiles[j].current.textContent == ArrayOfTypingKeys[row][each].current.textContent.toUpperCase()) && (ArrayOfTypingKeys[row][each].current.style.backgroundColor !== "green")){
                                        ArrayOfTypingKeys[row][each].current.style.backgroundColor = "yellow"
                                        break
                                    } 
                                }
                            }
                            break
                        } 
                        else {
                            ArrayOftiles[j].current.classList.add("flipGray")
                            for(let row = 0; row < ArrayOfTypingKeys.length; row++){
                                for(let each = 0; each < ArrayOfTypingKeys[row].length; each++){
                                    if(ArrayOftiles[j].current.textContent == ArrayOfTypingKeys[row][each].current.textContent.toUpperCase() && ArrayOfTypingKeys[row][each].current.style.backgroundColor != "green"){
                                        ArrayOfTypingKeys[row][each].current.style.backgroundColor = "gray"
                                    } 
                                }
                            }

                        }
                    }
                }
                if(guess != Word && row == 6) {
                    setRow(1)
                    setTiles(0)
                    setTimeout(() => {
                        setGameState(false)
                        setPage(2)
                        setIsCorrect(false)
                        setShowAnswer(true)
                    }, 1000);
                } else {setRow(row + 1)}
            }
            compareScoreToHighScore
            if(Word != null){GameUpdate(guess, Word.toUpperCase())}
        }
    }

    function GameUpdate(string, Word){
        if(string == Word){
            switch(row){
                case 1:
                    setScore(score + 6)
                    setGold(gold + 15)
                    break;
                case 2:
                    setScore(score + 5)
                    setGold(gold + 14)
                    break;
                case 3:
                    setScore(score + 4)
                    setGold(gold + 13)
                    break;
                case 4:
                    setScore(score + 3)
                    setGold(gold + 15)
                    break;
                case 5:
                    setScore(score + 2)
                    setGold(gold + 12)
                    break;
                case 6:
                    setScore(score + 1)
                    setGold(gold + 11)
                    break;
            } 
            setRandomIndices(["0", "1", "2", "3", "4"])
            setTiles(0)
            setRow(1)
            SetGuessedWords(Word)
            setGameState(false)
        }
    }

    function clearBoard(){
        if(tile1.current != null){
            for(let i=0; i<ArrayOftiles.length; i++){
                if(ArrayOftiles[i].current.classList.contains("highlightTile") ){
                    ArrayOftiles[i].current.classList.remove("highlightTile")
                    ArrayOftiles[i].current.classList.add("Unhighlight")
                    ArrayOftiles[i].current.textContent = ""
                } 
    
                if(ArrayOftiles[i].current.classList.contains("hintedTile") ){
                    ArrayOftiles[i].current.classList.remove("hintedTile")
                    ArrayOftiles[i].current.textContent = ""
                } 
    
                if(ArrayOftiles[i].current.classList.contains("flipGreen")){
                    ArrayOftiles[i].current.classList.remove("flipGreen")
                }
                if(ArrayOftiles[i].current.classList.contains("flipYellow")){
                    ArrayOftiles[i].current.classList.remove("flipYellow")
                }
                if(ArrayOftiles[i].current.classList.contains("flipGray")){
                    ArrayOftiles[i].current.classList.remove("flipGray")
                }
                if(ArrayOftiles[i].current.classList.contains("hintedTile")){
                    ArrayOftiles[i].current.classList.remove("hintedTile")
                }
            }
            for(let row = 0; row < ArrayOfTypingKeys.length; row++){
    
                for(let each = 0; each < ArrayOfTypingKeys[row].length; each++){
                    if(ArrayOfTypingKeys[row][each].current.style.backgroundColor != "#99949473"){
                        ArrayOfTypingKeys[row][each].current.style.backgroundColor = "#99949473"
                    }
                }
            }
            for(let i=0; i<ArrayOftiles.length; i++){
                if(ArrayOftiles[i].current.classList.contains("Unhighlight")){
                    ArrayOftiles[i].current.classList.remove("Unhighlight")
                    
                } 
            }
        }
        
        setGameState(true)
    }

    function SetGuessedWords(word){
        if(collections.length != 0){
            for(let i = 0; i <= collections.length; i++){
                if(collections[i] == word && i < collections.length){
                    break;
                }

                if(i == collections.length){
                    setCollections([...collections, word])
                    break
                }
            }
        } else if(collections.length == 0) {
            setCollections([word])
        }
    }

    function Hint(boolean){
        if(boolean == true){
            setRandomNum(Math.random() * 1)
            setGold(gold - 20)
            setShowVerificationPrompt(false)
        } else {    
            setShowVerificationPrompt(false)
        }
    }

    const useHint = useEffect(()=>{
        if(gameState && row != null && randomIndices.length != 0){
            let random = Math.floor(Math.random() * randomIndices.length).toString()
            let randomI = randomIndices
            let randomLetterIndex = randomI[random]
            console.log(randomLetterIndex)

            for(let i = (row * 5) - 5; i < row*5; i++){
                console.log(i)
                if(i-(row*5 - 5) == randomLetterIndex){
                    ArrayOftiles[i].current.classList.add("hintedTile")
                    ArrayOftiles[i].current.textContent = JSON.parse(localStorage.getItem("Word"))[randomLetterIndex]
                    randomI.splice(random, 1)
                    break
                }
            }

            setRandomIndices(randomI)
            if(randomIndices.length == 0){
                setTiles(row*5)
            }

            for(let i = (row*5) -5; i <= row*5; i++){
                if(i != row*5 && ArrayOftiles[i].current.textContent == ""){
                    break
                } 
                if(i == (row*5)) {
                    console.log(i)
                    setTiles(row*5)
                    break
                }
            }
        }


        
    },[randomNum])

    const storeInfos = useEffect(()=>{
        localStorage.setItem("goldStorage", JSON.stringify(gold))
        localStorage.setItem("CollectedWords", JSON.stringify(collections))
    },[collections, gold])

    const compareScoreToHighScore = useEffect(()=>{
        if(score >= JSON.parse(localStorage.getItem("highScore"))){
            setHighScore(score)
            localStorage.setItem("highScore", JSON.stringify(score))
        }
    },[score])
    if(page === 1){
        return(
            <>
                <div className={!started ? "Wrapper" : "HideWrapper"}>
                    <button className="StartButton" ref={StartButton} onClick={()=>{Start(); setStarted(true)}}>
                        Start
                    </button>
                </div>  
    
                <TopComponents gold={gold} score={score} setShowCollection={(e)=>setShowCollection(e)}  Hint={()=>{Hint()}}  />
    
                <div className={ started ? "Container" : "HideContainer"}>
                <h1>{window.innerWidth} , {window.innerHeight}</h1>
                    <div className="row row1" ref={row1}>
                        {arrOfRowOfTiles1.map((key, i) => {
                            return(<div className={"tile"} onClick={()=>{if(row == 1){deleteTiles(i)}}} key={key + i} ref={arrOfRowOfTiles1[i]}></div>)
                        })}
                    </div>
                    <div className="row row1" ref={row2}>
                        {arrOfRowOfTiles2.map((key, i) => {
                            return(<div className={"tile"} onClick={()=>{if(row == 2){deleteTiles(i)}}} key={key + i} ref={arrOfRowOfTiles2[i]}></div>)
                        })}
                    </div>
                    <div className="row row1" ref={row3}>
                        {arrOfRowOfTiles3.map((key, i) => {
                            return(<div className={"tile"} onClick={()=>{if(row == 3){deleteTiles(i)}}} key={key + i} ref={arrOfRowOfTiles3[i]}></div>)
                        })}
                    </div>
                    <div className="row row1" ref={row4}>
                        {arrOfRowOfTiles4.map((key, i) => {
                            return(<div className={"tile"} onClick={()=>{if(row == 4){deleteTiles(i)}}} key={key + i} ref={arrOfRowOfTiles4[i]}></div>)
                        })}
                    </div>
                    <div className="row row1" ref={row5}>
                        {arrOfRowOfTiles5.map((key, i) => {
                            return(<div className={"tile"} onClick={()=>{if(row == 5){deleteTiles(i)}}} key={key + i} ref={arrOfRowOfTiles5[i]}></div>)
                        })}
                    </div>
                    <div className="row row1" ref={row6}>
                        {arrOfRowOfTiles6.map((key, i) => {
                            return(<div className={"tile"} onClick={()=>{if(row == 6){deleteTiles(i)}}} key={key + i} ref={arrOfRowOfTiles6[i]}></div>)
                        })}
                    </div>
    
                    <button className="Submit" ref={SubmitButton} onClick={()=>CheckAnswer("Enter")}>Submit</button>
                </div>
    
                <TypingKeys Keys={Keys} KeyType={(key)=>KeyType(key)} ArrayOfTypingKeys={ArrayOfTypingKeys} CheckAnswer={(e)=>CheckAnswer(e)}/>
    
                <WordCollections showCollections={showCollections} collections={collections} setShowCollection={(e)=>setShowCollection(e)}/>

                <VerificationPrompt Hint={(e)=>Hint(e)}/>
            </>
    
            
        )
    } else if(page === 2 && showAnswer){
        return(
            <>
                <div className={ showAnswer ? "ShowAnswer" : "UnshowAnswer"}>
                    <h1>{isCorrect ? "You Win!" : "You Lose"}</h1>
                    <p className="Answer">The Word Is: <span id="answerSpan">{JSON.parse(localStorage.getItem("Word"))}</span></p>
                    <div className="Scores">
                        <h1 className="Score">High Score: {JSON.parse(localStorage.getItem("highScore"))}</h1>
                        <h1 className="Score">Your Score: {score}</h1>
                    </div>
                    <button className="TryAgain" onClick={()=>{
                        isCorrect ? newRound() : Start()
                    }}>
                        Try Again
                    </button>
                </div>
            </>
        )
    } 
    
}

export default Container