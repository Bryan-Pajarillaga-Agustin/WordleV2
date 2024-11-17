import { useEffect, useRef, useState } from "react"
import "../src/Container.css"
function Container(){
    const [row, setRow] = useState(null)
    const [currentRow, setCurrentRow] = useState(null)
    const [tile, setTiles] = useState(null)
    const [gameState, setGameState] = useState(false)
    const [answer, setAnswer] = useState("")
    const [guess, setGuess] = useState("")
    const [score, setScore] = useState(null)

    const SubmitButton = useRef(null)
    const StartButton = useRef(null)
    const wordList = ["cigar", "rebut", "sissy", "humph", "awake", "blush", "focal", "evade", "naval", "serve", "heath", "dwarf", "model", "karma", "stink", "grade", "quiet", "bench", "abate", "feign", "major", "death", "fresh", "crust", "stool", "colon", "abase", "marry", "react", "batty", "pride", "floss", "helix", "croak", "staff", "paper", "unfed", "whelp", "trawl", "outdo", "adobe", "crazy", "sower", "repay", "digit", "crate", "cluck", "spike", "mimic", "pound", "maxim", "linen", "unmet", "flesh", "booby", "forth", "first", "stand", "belly", "ivory", "seedy", "print", "yearn", "drain", "bribe", "stout", "panel", "crass", "flume", "offal", "agree", "error", "swirl", "argue", "bleed", "delta", "flick", "totem", "wooer", "front", "shrub", "parry", "biome", "lapel", "start", "greet", "goner", "golem", "lusty", "loopy", "round", "audit", "lying", "gamma", "labor", "islet", "civic", "forge", "corny", "moult", "basic", "salad", "agate", "spicy", "spray", "essay", "fjord", "spend", "kebab", "guild", "aback", "motor", "alone", "hatch", "hyper", "thumb", "dowry", "ought", "belch", "dutch", "pilot", "tweed", "comet", "jaunt", "enema", "steed", "abyss", "growl", "fling", "dozen", "boozy", "erode", "world", "gouge", "click", "briar", "great", "altar", "pulpy", "blurt", "coast", "duchy", "groin", "fixer", "group", "rogue", "badly", "smart", "pithy", "gaudy", "chill", "heron", "vodka", "finer", "surer", "radio", "rouge", "perch", "retch", "wrote", "clock", "tilde", "store", "prove", "bring", "solve", "cheat", "grime", "exult", "usher", "epoch", "triad", "break", "rhino", "viral", "conic", "masse", "sonic", "vital", "trace", "using", "peach", "champ", "baton", "brake", "pluck", "craze", "gripe", "weary", "picky", "acute", "ferry", "aside", "tapir", "troll", "unify", "rebus", "boost", "truss", "siege", "tiger", "banal", "slump", "crank", "gorge", "query", "drink", "favor", "abbey", "tangy", "panic", "solar", "shire", "proxy", "point", "robot", "prick", "wince", "crimp", "knoll", "sugar", "whack", "mount", "perky", "could", "wrung", "light", "those", "moist", "shard", "pleat", "aloft", "skill", "elder", "frame", "humor", "pause", "ulcer", "ultra", "robin", "cynic", "aroma", "caulk", "shake", "dodge", "swill", "tacit", "other", "thorn", "trove", "bloke", "vivid", "spill", "chant", "choke", "rupee", "nasty", "mourn", "ahead", "brine", "cloth", "hoard", "sweet", "month", "lapse", "watch", "today", "focus", "smelt", "tease", "cater", "movie", "saute", "allow", "renew", "their", "slosh", "purge", "chest", "depot", "epoxy", "nymph", "found", "shall", "harry", "stove", "lowly", "snout", "trope", "fewer", "shawl", "natal", "comma", "foray", "scare", "stair", "black", "squad", "royal", "chunk", "mince", "shame", "cheek", "ample", "flair", "foyer", "cargo", "oxide", "plant", "olive", "inert", "askew", "heist", "shown", "zesty", "hasty", "trash", "fella", "larva", "forgo", "story", "hairy", "train", "homer", "badge", "midst", "canny", "fetus", "butch", "farce", "slung", "tipsy", "metal", "yield", "delve", "being", "scour", "glass", "gamer", "scrap", "money", "hinge", "album", "vouch", "asset", "tiara", "crept", "bayou", "atoll", "manor", "creak", "showy", "phase", "froth", "depth", "gloom", "flood", "trait", "girth", "piety", "payer", "goose", "float", "donor", "atone", "primo", "apron", "blown", "cacao", "loser", "input", "gloat", "awful", "brink", "smite", "beady", "rusty", "retro", "droll", "gawky", "hutch", "pinto", "gaily", "egret", "lilac", "sever", "field", "fluff", "hydro", "flack", "agape", "voice", "stead", "stalk", "berth", "madam", "night", "bland", "liver", "wedge", "augur", "roomy", "wacky", "flock", "angry", "bobby", "trite", "aphid", "tryst", "midge", "power", "elope", "cinch", "motto", "stomp", "upset", "bluff", "cramp", "quart", "coyly", "youth", "rhyme", "buggy", "alien", "smear", "unfit", "patty", "cling", "glean", "label", "hunky", "khaki", "poker", "gruel", "twice", "twang", "shrug", "treat", "unlit", "waste", "merit", "woven", "octal", "needy", "clown", "widow", "irony", "ruder", "gauze", "chief", "onset", "prize", "fungi", "charm", "gully", "inter", "whoop", "taunt", "leery", "class", "theme", "lofty", "tibia", "booze", "alpha", "thyme", "eclat", "doubt", "parer", "chute", "stick", "trice", "alike", "sooth", "recap", "saint", "liege", "glory", "grate", "admit", "brisk", "soggy", "usurp", "scald", "scorn", "leave", "twine", "sting", "bough", "marsh", "sloth", "dandy", "vigor", "howdy", "enjoy", "valid", "ionic", "equal", "unset", "floor", "catch", "spade", "stein", "exist", "quirk", "denim", "grove", "spiel", "mummy", "fault", "foggy", "flout", "carry", "sneak", "libel", "waltz", "aptly", "piney", "inept", "aloud", "photo", "dream", "stale", "vomit", "ombre", "fanny", "unite", "snarl", "baker", "there", "glyph", "pooch", "hippy", "spell", "folly", "louse", "gulch", "vault", "godly", "threw", "fleet", "grave", "inane", "shock", "crave", "spite", "valve", "skimp", "claim", "rainy", "musty", "pique", "daddy", "quasi", "arise", "aging", "valet", "opium", "avert", "stuck", "recut", "mulch", "genre", "plume", "rifle", "count", "incur", "total", "wrest", "mocha", "deter", "study", "lover", "safer", "rivet", "funny", "smoke", "mound", "undue", "sedan", "pagan", "swine", "guile", "gusty", "equip", "tough", "canoe", "chaos", "covet", "human", "udder", "lunch", "blast", "stray", "manga", "melee", "lefty", "quick", "paste", "given", "octet", "risen", "groan", "leaky", "grind", "carve", "loose", "sadly", "spilt", "apple", "slack", "honey", "final", "sheen", "eerie", "minty", "slick", "derby", "wharf", "spelt", "coach", "erupt", "singe", "price", "spawn", "fairy", "jiffy", "filmy", "stack", "chose", "sleep", "ardor", "nanny", "niece", "woozy", "handy", "grace", "ditto", "stank", "cream", "usual", "diode", "valor", "angle", "ninja", "muddy", "chase", "reply", "prone", "spoil", "heart", "shade", "diner", "arson", "onion", "sleet", "dowel", "couch", "palsy", "bowel", "smile", "evoke", "creek", "lance", "eagle", "idiot", "siren", "built", "embed", "award", "dross", "annul", "goody", "frown", "patio", "laden", "humid", "elite", "lymph", "edify", "might", "reset", "visit", "gusto", "purse", "vapor", "crock", "write", "sunny", "loath", "chaff", "slide", "queer", "venom", "stamp", "sorry", "still", "acorn", "aping", "pushy", "tamer", "hater", "mania", "awoke", "brawn", "swift", "exile", "birch", "lucky", "freer", "risky", "ghost", "plier", "lunar", "winch", "snare", "nurse", "house", "borax", "nicer", "lurch", "exalt", "about", "savvy", "toxin", "tunic", "pried", "inlay", "chump", "lanky", "cress", "eater", "elude", "cycle", "kitty", "boule", "moron", "tenet", "place", "lobby", "plush", "vigil", "index", "blink", "clung", "qualm", "croup", "clink", "juicy", "stage", "decay", "nerve", "flier", "shaft", "crook", "clean", "china", "ridge", "vowel", "gnome", "snuck", "icing", "spiny", "rigor", "snail", "flown", "rabid", "prose", "thank", "poppy", "budge", "fiber", "moldy", "dowdy", "kneel", "track", "caddy", "quell", "dumpy", "paler", "swore", "rebar", "scuba", "splat", "flyer", "horny", "mason", "doing", "ozone", "amply", "molar", "ovary", "beset", "queue", "cliff", "magic", "truce", "sport", "fritz", "edict", "twirl", "verse", "llama", "eaten", "range", "whisk", "hovel", "rehab", "macaw", "sigma", "spout", "verve", "sushi", "dying", "fetid", "brain", "buddy", "thump", "scion", "candy", "chord", "basin", "march", "crowd", "arbor", "gayly", "musky", "stain", "dally", "bless", "bravo", "stung", "title", "ruler", "kiosk", "blond", "ennui", "layer", "fluid", "tatty", "score", "cutie", "zebra", "barge", "matey", "bluer", "aider", "shook", "river", "privy", "betel", "frisk", "bongo", "begun", "azure", "weave", "genie", "sound", "glove", "braid", "scope", "wryly", "rover", "assay", "ocean", "bloom", "irate", "later", "woken", "silky", "wreck", "dwelt", "slate", "smack", "solid", "amaze", "hazel", "wrist", "jolly", "globe", "flint", "rouse", "civil", "vista", "relax", "cover", "alive", "beech", "jetty", "bliss", "vocal", "often", "dolly", "eight", "joker", "since", "event", "ensue", "shunt", "diver", "poser", "worst", "sweep", "alley", "creed", "anime", "leafy", "bosom", "dunce", "stare", "pudgy", "waive", "choir", "stood", "spoke", "outgo", "delay", "bilge", "ideal", "clasp", "seize", "hotly", "laugh", "sieve", "block", "meant", "grape", "noose", "hardy", "shied", "drawl", "daisy", "putty", "strut", "burnt", "tulip", "crick", "idyll", "vixen", "furor", "geeky", "cough", "naive", "shoal", "stork", "bathe", "aunty", "check", "prime", "brass", "outer", "furry", "razor", "elect", "evict", "imply", "demur", "quota", "haven", "cavil", "swear", "crump", "dough", "gavel", "wagon", "salon", "nudge", "harem", "pitch", "sworn", "pupil", "excel", "stony", "cabin", "unzip", "queen", "trout", "polyp", "earth", "storm", "until", "taper", "enter", "child", "adopt", "minor", "fatty", "husky", "brave", "filet", "slime", "glint", "tread", "steal", "regal", "guest", "every", "murky", "share", "spore", "hoist", "buxom", "inner", "otter", "dimly", "level", "sumac", "donut", "stilt", "arena", "sheet", "scrub", "fancy", "slimy", "pearl", "silly", "porch", "dingo", "sepia", "amble", "shady", "bread", "friar", "reign", "dairy", "quill", "cross", "brood", "tuber", "shear", "posit", "blank", "villa", "shank", "piggy", "freak", "which", "among", "fecal", "shell", "would", "algae", "large", "rabbi", "agony", "amuse", "bushy", "copse", "swoon", "knife", "pouch", "ascot", "plane", "crown", "urban", "snide", "relay", "abide", "viola", "rajah", "straw", "dilly", "crash", "amass", "third", "trick", "tutor", "woody", "blurb", "grief", "disco", "where", "sassy", "beach", "sauna", "comic", "clued", "creep", "caste", "graze", "snuff", "frock", "gonad", "drunk", "prong", "lurid", "steel", "halve", "buyer", "vinyl", "utile", "smell", "adage", "worry", "tasty", "local", "trade", "finch", "ashen", "modal", "gaunt", "clove", "enact", "adorn", "roast", "speck", "sheik", "missy", "grunt", "snoop", "party", "touch", "mafia", "emcee", "array", "south", "vapid", "jelly", "skulk", "angst", "tubal", "lower", "crest", "sweat", "cyber", "adore", "tardy", "swami", "notch", "groom", "roach", "hitch", "young", "align", "ready", "frond", "strap", "puree", "realm", "venue", "swarm", "offer", "seven", "dryer", "diary", "dryly", "drank", "acrid", "heady", "theta", "junto", "pixie", "quoth", "bonus", "shalt", "penne", "amend", "datum", "build", "piano", "shelf", "lodge", "suing", "rearm", "coral", "ramen", "worth", "psalm", "infer", "overt", "mayor", "ovoid", "glide", "usage", "poise", "randy", "chuck", "prank", "fishy", "tooth", "ether", "drove", "idler", "swath", "stint", "while", "begat", "apply", "slang", "tarot", "radar", "credo", "aware", "canon", "shift", "timer", "bylaw", "serum", "three", "steak", "iliac", "shirk", "blunt", "puppy", "penal", "joist", "bunny", "shape", "beget", "wheel", "adept", "stunt", "stole", "topaz", "chore", "fluke", "afoot", "bloat", "bully", "dense", "caper", "sneer", "boxer", "jumbo", "lunge", "space", "avail", "short", "slurp", "loyal", "flirt", "pizza", "conch", "tempo", "droop", "plate", "bible", "plunk", "afoul", "savoy", "steep", "agile", "stake", "dwell", "knave", "beard", "arose", "motif", "smash", "broil", "glare", "shove", "baggy", "mammy", "swamp", "along", "rugby", "wager", "quack", "squat", "snaky", "debit", "mange", "skate", "ninth", "joust", "tramp", "spurn", "medal", "micro", "rebel", "flank", "learn", "nadir", "maple", "comfy", "remit", "gruff", "ester", "least", "mogul", "fetch", "cause", "oaken", "aglow", "meaty", "gaffe", "shyly", "racer", "prowl", "thief", "stern", "poesy", "rocky", "tweet", "waist", "spire", "grope", "havoc", "patsy", "truly", "forty", "deity", "uncle", "swish", "giver", "preen", "bevel", "lemur", "draft", "slope", "annoy", "lingo", "bleak", "ditty", "curly", "cedar", "dirge", "grown", "horde", "drool", "shuck", "crypt", "cumin", "stock", "gravy", "locus", "wider", "breed", "quite", "chafe", "cache", "blimp", "deign", "fiend", "logic", "cheap", "elide", "rigid", "false", "renal", "pence", "rowdy", "shoot", "blaze", "envoy", "posse", "brief", "never", "abort", "mouse", "mucky", "sulky", "fiery", "media", "trunk", "yeast", "clear", "skunk", "scalp", "bitty", "cider", "koala", "duvet", "segue", "creme", "super", "grill", "after", "owner", "ember", "reach", "nobly", "empty", "speed", "gipsy", "recur", "smock", "dread", "merge", "burst", "kappa", "amity", "shaky", "hover", "carol", "snort", "synod", "faint", "haunt", "flour", "chair", "detox", "shrew", "tense", "plied", "quark", "burly", "novel", "waxen", "stoic", "jerky", "blitz", "beefy", "lyric", "hussy", "towel", "quilt", "below", "bingo", "wispy", "brash", "scone", "toast", "easel", "saucy", "value", "spice", "honor", "route", "sharp", "bawdy", "radii", "skull", "phony", "issue", "lager", "swell", "urine", "gassy", "trial", "flora", "upper", "latch", "wight", "brick", "retry", "holly", "decal", "grass", "shack", "dogma", "mover", "defer", "sober", "optic", "crier", "vying", "nomad", "flute", "hippo", "shark", "drier", "obese", "bugle", "tawny", "chalk", "feast", "ruddy", "pedal", "scarf", "cruel", "bleat", "tidal", "slush", "semen", "windy", "dusty", "sally", "igloo", "nerdy", "jewel", "shone", "whale", "hymen", "abuse", "fugue", "elbow", "crumb", "pansy", "welsh", "syrup", "terse", "suave", "gamut", "swung", "drake", "freed", "afire", "shirt", "grout", "oddly", "tithe", "plaid", "dummy", "broom", "blind", "torch", "enemy", "again", "tying", "pesky", "alter", "gazer", "noble", "ethos", "bride", "extol", "decor", "hobby", "beast", "idiom", "utter", "these", "sixth", "alarm", "erase", "elegy", "spunk", "piper", "scaly", "scold", "hefty", "chick", "sooty", "canal", "whiny", "slash", "quake", "joint", "swept", "prude", "heavy", "wield", "femme", "lasso", "maize", "shale", "screw", "spree", "smoky", "whiff", "scent", "glade", "spent", "prism", "stoke", "riper", "orbit", "cocoa", "guilt", "humus", "shush", "table", "smirk", "wrong", "noisy", "alert", "shiny", "elate", "resin", "whole", "hunch", "pixel", "polar", "hotel", "sword", "cleat", "mango", "rumba", "puffy", "filly", "billy", "leash", "clout", "dance", "ovate", "facet", "chili", "paint", "liner", "curio", "salty", "audio", "snake", "fable", "cloak", "navel", "spurt", "pesto", "balmy", "flash", "unwed", "early", "churn", "weedy", "stump", "lease", "witty", "wimpy", "spoof", "saner", "blend", "salsa", "thick", "warty", "manic", "blare", "squib", "spoon", "probe", "crepe", "knack", "force", "debut", "order", "haste", "teeth", "agent", "widen", "icily", "slice", "ingot", "clash", "juror", "blood", "abode", "throw", "unity", "pivot", "slept", "troop", "spare", "sewer", "parse", "morph", "cacti", "tacky", "spool", "demon", "moody", "annex", "begin", "fuzzy", "patch", "water", "lumpy", "admin", "omega", "limit", "tabby", "macho", "aisle", "skiff", "basis", "plank", "verge", "botch", "crawl", "lousy", "slain", "cubic", "raise", "wrack", "guide", "foist", "cameo", "under", "actor", "revue", "fraud", "harpy", "scoop", "climb", "refer", "olden", "clerk", "debar", "tally", "ethic", "cairn", "tulle", "ghoul", "hilly", "crude", "apart", "scale", "older", "plain", "sperm", "briny", "abbot", "rerun", "quest", "crisp", "bound", "befit", "drawn", "suite", "itchy", "cheer", "bagel", "guess", "broad", "axiom", "chard", "caput", "leant", "harsh", "curse", "proud", "swing", "opine", "taste", "lupus", "gumbo", "miner", "green", "chasm", "lipid", "topic", "armor", "brush", "crane", "mural", "abled", "habit", "bossy", "maker", "dusky", "dizzy", "lithe", "brook", "jazzy", "fifty", "sense", "giant", "surly", "legal", "fatal", "flunk", "began", "prune", "small", "slant", "scoff", "torus", "ninny", "covey", "viper", "taken", "moral", "vogue", "owing", "token", "entry", "booth", "voter", "chide", "elfin", "ebony", "neigh", "minim", "melon", "kneed", "decoy", "voila", "ankle", "arrow", "mushy", "tribe", "cease", "eager", "birth", "graph", "odder", "terra", "weird", "tried", "clack", "color", "rough", "weigh", "uncut", "ladle", "strip", "craft", "minus", "dicey", "titan", "lucid", "vicar", "dress", "ditch", "gypsy", "pasta", "taffy", "flame", "swoop", "aloof", "sight", "broke", "teary", "chart", "sixty", "wordy", "sheer", "leper", "nosey", "bulge", "savor", "clamp", "funky", "foamy", "toxic", "brand", "plumb", "dingy", "butte", "drill", "tripe", "bicep", "tenor", "krill", "worse", "drama", "hyena", "think", "ratio", "cobra", "basil", "scrum", "bused", "phone", "court", "camel", "proof", "heard", "angel", "petal", "pouty", "throb", "maybe", "fetal", "sprig", "spine", "shout", "cadet", "macro", "dodgy", "satyr", "rarer", "binge", "trend", "nutty", "leapt", "amiss", "split", "myrrh", "width", "sonar", "tower", "baron", "fever", "waver", "spark", "belie", "sloop", "expel", "smote", "baler", "above", "north", "wafer", "scant", "frill", "awash", "snack", "scowl", "frail", "drift", "limbo", "fence", "motel", "ounce", "wreak", "revel", "talon", "prior", "knelt", "cello", "flake", "debug", "anode", "crime", "salve", "scout", "imbue", "pinky", "stave", "vague", "chock", "fight", "video", "stone", "teach", "cleft", "frost", "prawn", "booty", "twist", "apnea", "stiff", "plaza", "ledge", "tweak", "board", "grant", "medic", "bacon", "cable", "brawl", "slunk", "raspy", "forum", "drone", "women", "mucus", "boast", "toddy", "coven", "tumor", "truer", "wrath", "stall", "steam", "axial", "purer", "daily", "trail", "niche", "mealy", "juice", "nylon", "plump", "merry", "flail", "papal", "wheat", "berry", "cower", "erect", "brute", "leggy", "snipe", "sinew", "skier", "penny", "jumpy", "rally", "umbra", "scary", "modem", "gross", "avian", "greed", "satin", "tonic", "parka", "sniff", "livid", "stark", "trump", "giddy", "reuse", "taboo", "avoid", "quote", "devil", "liken", "gloss", "gayer", "beret", "noise", "gland", "dealt", "sling", "rumor", "opera", "thigh", "tonga", "flare", "wound", "white", "bulky", "etude", "horse", "circa", "paddy", "inbox", "fizzy", "grain", "exert", "surge", "gleam", "belle", "salvo", "crush", "fruit", "sappy", "taker", "tract", "ovine", "spiky", "frank", "reedy", "filth", "spasm", "heave", "mambo", "right", "clank", "trust", "lumen", "borne", "spook", "sauce", "amber", "lathe", "carat", "corer", "dirty", "slyly", "affix", "alloy", "taint", "sheep", "kinky", "wooly", "mauve", "flung", "yacht", "fried", "quail", "brunt", "grimy", "curvy", "cagey", "rinse", "deuce", "state", "grasp", "milky", "bison", "graft", "sandy", "baste", "flask", "hedge", "girly", "swash", "boney", "coupe", "endow", "abhor", "welch", "blade", "tight", "geese", "miser", "mirth", "cloud", "cabal", "leech", "close", "tenth", "pecan", "droit", "grail", "clone", "guise", "ralph", "tango", "biddy", "smith", "mower", "payee", "serif", "drape", "fifth", "spank", "glaze", "allot", "truck", "kayak", "virus", "testy", "tepee", "fully", "zonal", "metro", "curry", "grand", "banjo", "axion", "bezel", "occur", "chain", "nasal", "gooey", "filer", "brace", "allay", "pubic", "raven", "plead", "gnash", "flaky", "munch", "dully", "eking", "thing", "slink", "hurry", "theft", "shorn", "pygmy", "ranch", "wring", "lemon", "shore", "mamma", "froze", "newer", "style", "moose", "antic", "drown", "vegan", "chess", "guppy", "union", "lever", "lorry", "image", "cabby", "druid", "exact", "truth", "dopey", "spear", "cried", "chime", "crony", "stunk", "timid", "batch", "gauge", "rotor", "crack", "curve", "latte", "witch", "bunch", "repel", "anvil", "soapy", "meter", "broth", "madly", "dried", "scene", "known", "magma", "roost", "woman", "thong", "punch", "pasty", "downy", "knead", "whirl", "rapid", "clang", "anger", "drive", "goofy", "email", "music", "stuff", "bleep", "rider", "mecca", "folio", "setup", "verso", "quash", "fauna", "gummy", "happy", "newly", "fussy", "relic", "guava", "ratty", "fudge", "femur", "chirp", "forte", "alibi", "whine", "petty", "golly", "plait", "fleck", "felon", "gourd", "brown", "thrum", "ficus", "stash", "decry", "wiser", "junta", "visor", "daunt", "scree", "impel", "await", "press", "whose", "turbo", "stoop", "speak", "mangy", "eying", "inlet", "crone", "pulse", "mossy", "staid", "hence", "pinch", "teddy", "sully", "snore", "ripen", "snowy", "attic", "going", "leach", "mouth", "hound", "clump", "tonal", "bigot", "peril", "piece", "blame", "haute", "spied", "undid", "intro", "basal", "shine", "gecko", "rodeo", "guard", "steer", "loamy", "scamp", "scram", "manly", "hello", "vaunt", "organ", "feral", "knock", "extra", "condo", "adapt", "willy", "polka", "rayon", "skirt", "faith", "torso", "match", "mercy", "tepid", "sleek", "riser", "twixt", "peace", "flush", "catty", "login", "eject", "roger", "rival", "untie", "refit", "aorta", "adult", "judge", "rower", "artsy", "rural", "shave"]

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

    const column1 = useRef(null)
    const column2 = useRef(null)
    const column3 = useRef(null)
    const column4 = useRef(null)
    const column5 = useRef(null)
    const column6 = useRef(null)
    const column7 = useRef(null)
    const column8 = useRef(null)
    const column9 = useRef(null)
    const column10 = useRef(null)
    const column11 = useRef(null)
    const column12 = useRef(null)
    const column13 = useRef(null)
    const column14 = useRef(null)
    const column15 = useRef(null)
    const column16 = useRef(null)
    const column17 = useRef(null)
    const column18 = useRef(null)
    const column19 = useRef(null)
    const column20 = useRef(null)
    const column21 = useRef(null)
    const column22 = useRef(null)
    const column23 = useRef(null)
    const column24 = useRef(null)
    const column25 = useRef(null)
    const column26 = useRef(null)
    const column27 = useRef(null)
    const column28 = useRef(null)
    const column29 = useRef(null)
    const column30 = useRef(null)

    const ArrayOfColumns = [ column1, column2, column3, column4, column5, column6, column7, column8, column9, column10, column11, column12, column13, column14, column15, column16, column17, column18, column19, column20, column21, column22, column23, column24, column25, column26, column27, column28, column29, column30]
    window.onkeydown = (e)=>{DefineTiles(e)}
    const KeyType = (e) => {DefineTiles(e); console.log(e)}
    function Start(){
        setGameState(true)
        if(!gameState){
            setRow(1)
            setTiles(0)
            setScore(0)
            setAnswer(wordList[Math.floor(Math.random() * Math.floor(Math.random() * (wordList.length - 1)) )].toUpperCase())
        } 
    }

    useEffect(()=>{console.log(answer, row)},[answer, row])

    function DefineTiles(e){
        const letters = [
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
            's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
        ]
        let key
        e.key != null ? key = e.key : key = e

        console.log(key)
        for(let i = 0; i < letters.length; i++){
            if(row == 1){
                if((key == letters[i]) && tile != 5){
                    ArrayOfColumns[tile].current.classList.add("highlightColumn")
                    ArrayOfColumns[tile].current.textContent = key.toUpperCase()
                    setTiles(tile + 1)
                } else if((key == "⌫" || key == "Backspace") && tile != 0) {
                    ArrayOfColumns[tile-1].current.textContent = ""
                    ArrayOfColumns[tile-1].current.classList.remove("highlightColumn")
                    setTiles(tile-1)
                }
            } else if(row == 2){
                if((key == letters[i]) && tile != 10){
                    ArrayOfColumns[tile].current.classList.add("highlightColumn")
                    setTiles(tile + 1)
                    ArrayOfColumns[tile].current.textContent = key.toUpperCase()
                } else if((key == "⌫" || key == "Backspace") && tile != 5) {
                    ArrayOfColumns[tile-1].current.classList.remove("highlightColumn")
                    ArrayOfColumns[tile-1].current.textContent = ""
                    setTiles(tile-1)
                }
            } else if(row == 3){
                if((key == letters[i]) && tile != 15){
                    ArrayOfColumns[tile].current.classList.add("highlightColumn")
                    setTiles(tile + 1)
                    ArrayOfColumns[tile].current.textContent = key.toUpperCase()
                } else if((key == "⌫" || key == "Backspace") && tile != 10) {
                    ArrayOfColumns[tile-1].current.classList.remove("highlightColumn")
                    ArrayOfColumns[tile-1].current.textContent = ""
                    setTiles(tile-1)
                }
            } else if(row == 4){
                if((key == letters[i]) && tile != 20){
                    ArrayOfColumns[tile].current.classList.add("highlightColumn")
                    setTiles(tile + 1)
                    ArrayOfColumns[tile].current.textContent = key.toUpperCase()
                } else if((key == "⌫" || key == "Backspace") && tile != 15) {
                    ArrayOfColumns[tile-1].current.classList.remove("highlightColumn")
                    ArrayOfColumns[tile-1].current.textContent = ""
                    setTiles(tile-1)
                }
            } else if(row == 5){
                if((key == letters[i]) && tile != 25){
                    ArrayOfColumns[tile].current.classList.add("highlightColumn")
                    setTiles(tile + 1)
                    ArrayOfColumns[tile].current.textContent = key.toUpperCase()
                } else if((key == "⌫" || key == "Backspace") && tile != 20) {
                    ArrayOfColumns[tile-1].current.classList.remove("highlightColumn")
                    ArrayOfColumns[tile-1].current.textContent = ""
                    setTiles(tile-1)
                }
            } else if(row == 6){
                if((key == letters[i]) && tile != 30){
                    ArrayOfColumns[tile].current.classList.add("highlightColumn")
                    if(tile<30){setTiles(tile + 1)}
                    ArrayOfColumns[tile].current.textContent = key.toUpperCase()
                } else if((key == "⌫" || key == "Backspace") && tile != 25 && gameState == true) {
                    ArrayOfColumns[tile-1].current.classList.remove("highlightColumn")
                    ArrayOfColumns[tile-1].current.textContent = ""
                    setTiles(tile-1)
                }
            }
        }
        
    }

    window.addEventListener("keydown", (e)=>CheckAnswer(e))
    function CheckAnswer(e){
        let Guess = ""
        if(e.key === "Enter" || e == "Enter"){
            if(row === 1 && tile === 5){
                for(let i = tile-5; i < tile; i++){
                    if(ArrayOfColumns[i].current != null){Guess += ArrayOfColumns[i].current.textContent}
                    Guess.toString()
                }
                setGuess(Guess)
            } else if(row === 2 && tile === 10){
                for(let i = tile-5; i < tile; i++){
                    if(ArrayOfColumns[i].current != null){Guess += ArrayOfColumns[i].current.textContent}
                    Guess.toString()
                }
                setGuess(Guess)
            } else if(row === 3 && tile === 15){
                for(let i = tile-5; i < tile; i++){
                    if(ArrayOfColumns[i].current != null){Guess += ArrayOfColumns[i].current.textContent}
                    Guess.toString()
                }
                setGuess(Guess)
            } else if(row === 4 && tile === 20){
                for(let i = tile-5; i < tile; i++){
                    if(ArrayOfColumns[i].current != null){Guess += ArrayOfColumns[i].current.textContent}
                    Guess.toString()
                }
                setGuess(Guess)
            } else if(row === 5 && tile === 25){
                for(let i = tile-5; i < tile; i++){
                    if(ArrayOfColumns[i].current != null){Guess += ArrayOfColumns[i].current.textContent}
                    Guess.toString()
                }
                setGuess(Guess)
            } else if(row === 6 && tile === 30){
                for(let i = tile-5; i < tile; i++){
                    if(ArrayOfColumns[i].current != null){Guess += ArrayOfColumns[i].current.textContent}
                    Guess.toString()
                }
                setGuess(Guess)
            }

            if(row != 7){setRow(row + 1)}
            CheckGuess()
        }


        function CheckGuess(){
            let indexTile = tile - 5
            let indexAns = 0
            if(Guess.length == 5){

                if(Guess == answer){
                    for(let j = tile-5; j < tile; j++){
                        ArrayOfColumns[j].current.classList.add("flipGreen")
                    }

                } else if (Guess != answer){
                    for(var j = tile - 5; j < tile; j++){
                        
                        for(let i = 0; i < 5; i++){
                            if(answer[indexAns] == ArrayOfColumns[indexTile].current.textContent && answer[i] == Guess[i]){
                                if(ArrayOfColumns[indexTile].current.classList.contains("flipGray")){
                                    ArrayOfColumns[indexTile].current.classList.replace("flipGray", "flipGreen")
                                } else {
                                    ArrayOfColumns[indexTile].current.classList.add("flipGreen")
                                }
                                redo()
                                break
                            } else if(ArrayOfColumns[j].current.textContent == answer[i]){
                                
                                if(ArrayOfColumns[j].current.classList.contains("flipGray")){
                                    ArrayOfColumns[j].current.classList.replace("flipGray", "flipYellow")
                                } else {
                                    ArrayOfColumns[j].current.classList.add("flipYellow")
                                }
                                redo()
                                break
                            } else {
                                ArrayOfColumns[j].current.classList.add("flipGray")
                            }


                            function redo(){indexTile = tile - 5}
                            if(indexTile != tile - 1){indexTile++}else{indexTile = tile - 5}
                            
                        }
                        if(indexAns != 4){indexAns++}
                    }
                }

                
                GameUpdate(Guess)
            }
        }
    }

    function GameUpdate(string){
        if(string == answer){
            switch(row){
                case 1:
                    setScore(6)
                    break;
                case 2:
                    setScore(5)
                    break;
                case 3:
                    setScore(4)
                    break;
                case 4:
                    setScore(3)
                    break;
                case 5:
                    setScore(2)
                    break;
                case 6:
                    setScore(1)
                    break;
            } 
            clearBoard()
            setRow(1)
            setTiles(0)
        } else if(string != answer && row == 7) {
            window.alert("The Answer is: " + answer)
            setGameState(false)
            clearBoard()
        }

        function clearBoard(){
            setTimeout(() => {
                for(let i=0; i<ArrayOfColumns.length; i++){
                    if(ArrayOfColumns[i].current.classList.contains("highlightColumn")){
                        if(ArrayOfColumns[i].current.classList.contains("flipGreen")){
                            ArrayOfColumns[i].current.classList.remove("flipGreen")
                        } else if(ArrayOfColumns[i].current.classList.contains("flipYellow")){
                            ArrayOfColumns[i].current.classList.remove("flipYellow")
                        } else if(ArrayOfColumns[i].current.classList.contains("flipGray")){
                            ArrayOfColumns[i].current.classList.remove("flipGray")
                        }
                        ArrayOfColumns[i].current.classList.add("Unhighlight")
                        ArrayOfColumns[i].current.classList.remove("highlightColumn")
                        ArrayOfColumns[i].current.textContent = ""
                        setTimeout(() => {
                            ArrayOfColumns[i].current.classList.remove("Unhighlight")
                        }, 1000);
                    } 
                    
                }
            }, 1000);
            
        }
    }

    return(
        <>
            <div className={!gameState ? "Wrapper" : "HideWrapper"}>
                <button className="StartButton" ref={StartButton} onClick={()=>Start()}>
                    Start
                </button>
            </div>  

            <div className="Container">
            <h1>WORDLE</h1>
            <div className="row row1" ref={row1}>
                <div className={"column"} ref={column1}></div>
                <div className={"column"} ref={column2}></div>
                <div className={"column"} ref={column3}></div>
                <div className={"column"} ref={column4}></div>
                <div className={"column"} ref={column5}></div>
            </div>
            <div className="row row1" ref={row2}>
                <div className={"column"} ref={column6}></div>
                <div className={"column"} ref={column7}></div>
                <div className={"column"} ref={column8}></div>
                <div className={"column"} ref={column9}></div>
                <div className={"column"} ref={column10}></div>
            </div>
            <div className="row row1" ref={row3}>
                <div className={"column"} ref={column11}></div>
                <div className={"column"} ref={column12}></div>
                <div className={"column"} ref={column13}></div>
                <div className={"column"} ref={column14}></div>
                <div className={"column"} ref={column15}></div>
            </div>
            <div className="row row1" ref={row4}>
                <div className={"column"} ref={column16}></div>
                <div className={"column"} ref={column17}></div>
                <div className={"column"} ref={column18}></div>
                <div className={"column"} ref={column19}></div>
                <div className={"column"} ref={column20}></div>
            </div>
            <div className="row row1" ref={row5}>
                <div className={"column"} ref={column21}></div>
                <div className={"column"} ref={column22}></div>
                <div className={"column"} ref={column23}></div>
                <div className={"column"} ref={column24}></div>
                <div className={"column"} ref={column25}></div>
            </div>
            <div className="row row1" ref={row6}>
                <div className={"column"} ref={column26}></div>
                <div className={"column"} ref={column27}></div>
                <div className={"column"} ref={column28}></div>
                <div className={"column"} ref={column29}></div>
                <div className={"column"} ref={column30}></div>
            </div>

            <button className="Submit" ref={SubmitButton} onClick={()=>CheckAnswer("Enter")}>Submit</button>
        </div>
        
        <div className="Keyboard_Wrapper">
            <div className="Row-Keys">
                {Keys[0].map(key=>(
                    <li key={key} onClick={()=>KeyType(key)}>{key}</li>
                ))}
            </div>
            <div className="Row-Keys">
                {Keys[1].map(key=>(
                    <li key={key} onClick={()=>KeyType(key)}>{key}</li>
                ))}
            </div>
            <div className="Row-Keys">
                {Keys[2].map(key=>(
                    <li key={key} onClick={()=>KeyType(key)}>{key}</li>
                ))}
                <li onClick={()=>CheckAnswer("Enter")}>Enter</li>
            </div>
        </div>
        </>

        
    )
}

export default Container