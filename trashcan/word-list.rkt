#lang sicp
(#%require "data-structures.rkt")

;; I copied the list from https://en.oxforddictionaries.com/explore/literary-words
;; and wrote a python script to generate the expression below

(define wl
  (list 
   (make-entry 'abode "a home")
   (make-entry 'access "an outburst of an emotion")
   (make-entry 'adieu "goodbye")
   (make-entry 'afar "at a distance")
   (make-entry 'apace "quickly")
   (make-entry 'argosy "a large merchant ship")
   (make-entry 'arrant "utter")
   (make-entry 'asunder "into pieces")
   (make-entry 'atrabilious "melancholy or bad-tempered")
   (make-entry 'aurora "the dawn")
   (make-entry 'bard "a poet")
   (make-entry 'barque "a boat")
   (make-entry 'bedizen "dress gaudily")
   (make-entry 'beget "produce (a child)")
   (make-entry 'behold "see")
   (make-entry 'beseech "ask urgently and fervently")
   (make-entry 'bestrew "scatter")
   (make-entry 'betake "oneself	go to")
   (make-entry 'betide "happen")
   (make-entry 'betoken "be a warning of")
   (make-entry 'blade "sword")
   (make-entry 'blithe "happy")
   (make-entry 'bosky "covered by trees or bushes")
   (make-entry 'brand "a sword")
   (make-entry 'brume "mist or fog")
   (make-entry 'celerity "swiftness")
   (make-entry 'circumvallate "surround with a rampart or wall")
   (make-entry 'clarion "loud and clear")
   (make-entry 'cleave "to	stick fast to")
   (make-entry 'cockcrow "dawn")
   (make-entry 'coruscate "flash or sparkle")
   (make-entry 'crapulent "relating to the drinking of alcohol")
   (make-entry 'crescent "growing")
   (make-entry 'darkling "relating to growing darkness")
   (make-entry 'deep "the	the sea")
   (make-entry 'dell "a small valley")
   (make-entry 'dingle "a deep wooded valley")
   (make-entry 'divers "of varying types")
   (make-entry 'Dives "a rich man")
   (make-entry 'dolour "great sorrow")
   (make-entry 'dome "a stately building")
   (make-entry 'dulcify "sweeten")
   (make-entry 'effulgent "shining brightly")
   (make-entry 'eld "old age")
   (make-entry 'eminence "a piece of rising ground")
   (make-entry 'empyrean "the sky")
   (make-entry 'ere "before")
   (make-entry 'erne "a sea eagle")
   (make-entry 'espy "catch sight of")
   (make-entry 'ether "the clear sky")
   (make-entry 'evanescent "quickly fading")
   (make-entry 'farewell "goodbye")
   (make-entry 'fervid "hot or glowing")
   (make-entry 'fidus "Achates	a faithful friend")
   (make-entry 'finny "relating to fish")
   (make-entry 'firmament "the sky")
   (make-entry 'flaxen "pale yellow")
   (make-entry 'fleer "jeer or laugh disrespectfully")
   (make-entry 'flexuous "full of bends and curves")
   (make-entry 'fulgent "shining brightly")
   (make-entry 'fulguration "a flash like lightning")
   (make-entry 'fuliginous "sooty; dusky")
   (make-entry 'fulminate "explode violently")
   (make-entry 'furbelow "adorn with trimmings")
   (make-entry 'gird "secure with a belt")
   (make-entry 'glaive "a sword")
   (make-entry 'gloaming "dusk")
   (make-entry 'greensward "grassy ground")
   (make-entry 'gyre "whirl or gyrate")
   (make-entry 'hark "listen")
   (make-entry 'horripilation "gooseflesh; hair standing on end")
   (make-entry 'hymeneal "relating to marriage")
   (make-entry 'ichor "blood, or a fluid likened to it")
   (make-entry 'illude "trick someone")
   (make-entry 'imbrue "stain one's hand or sword with blood")
   (make-entry 'impuissant "powerless")
   (make-entry 'incarnadine "colour (something) crimson")
   (make-entry 'ingrate "ungrateful")
   (make-entry 'inhume "bury")
   (make-entry 'inly "inwardly")
   (make-entry 'ire "anger")
   (make-entry 'isle "an island")
   (make-entry 'knell "the sound of a bell")
   (make-entry 'lachrymal "connected with weeping or tears")
   (make-entry 'lacustrine "associated with lakes")
   (make-entry 'lambent "softly glowing or flickering")
   (make-entry 'lave "wash or wash over")
   (make-entry 'lay "a song")
   (make-entry 'lea "an area of grassy land")
   (make-entry 'lenity "kindness or gentleness")
   (make-entry 'lightsome "nimble")
   (make-entry 'limn "represent in painting or words")
   (make-entry 'lucent "shining")
   (make-entry 'madding "acting madly; frenzied")
   (make-entry 'mage "a magician or learned person")
   (make-entry 'main "the	the open ocean")
   (make-entry 'malefic "causing harm")
   (make-entry 'manifold "many and various")
   (make-entry 'marge "a margin")
   (make-entry 'mead "a meadow")
   (make-entry 'mephitic "foul-smelling")
   (make-entry 'mere "a lake or pond")
   (make-entry 'moon "a month")
   (make-entry 'morrow "the	the following day")
   (make-entry 'muliebrity "womanliness")
   (make-entry 'nescient "lacking knowledge; ignorant")
   (make-entry 'nigh "near")
   (make-entry 'niveous "snowy")
   (make-entry 'nocuous "noxious, harmful, or poisonous")
   (make-entry 'noisome "foul-smelling")
   (make-entry 'nymph "a beautiful young woman")
   (make-entry 'orb "an eye")
   (make-entry 'orgulous "proud or haughty")
   (make-entry 'pellucid "translucent")
   (make-entry 'perchance "by some chance")
   (make-entry 'perfervid "intense and impassioned")
   (make-entry 'perfidious "deceitful and untrustworthy")
   (make-entry 'philippic "a bitter verbal attack")
   (make-entry 'plangent "loud and mournful")
   (make-entry 'plash "a splashing sound")
   (make-entry 'plenteous "plentiful")
   (make-entry 'plumbless "extremely deep")
   (make-entry 'poesy "poetry")
   (make-entry 'prothalamium "a song or poem celebrating a wedding")
   (make-entry 'puissant "powerful or influential")
   (make-entry 'pulchritude "beauty")
   (make-entry 'purl "flow with a babbling sound")
   (make-entry 'quidnunc "an inquisitive and gossipy person")
   (make-entry 'realm "a kingdom")
   (make-entry 'refulgent "shining brightly")
   (make-entry 'rend "tear to pieces")
   (make-entry 'repine "be discontented")
   (make-entry 'Rhadamanthine "stern and incorruptible in judgement")
   (make-entry 'roundelay "a short, simple song with a refrain")
   (make-entry 'rubescent "reddening")
   (make-entry 'rutilant "glowing or glittering with red or golden light")
   (make-entry 'sans "without")
   (make-entry 'scribe "write")
   (make-entry 'sea-girt "surrounded by sea")
   (make-entry 'sempiternal "everlasting")
   (make-entry 'serpent "a snake")
   (make-entry 'shade "a ghost")
   (make-entry 'ship "of the desert	a camel")
   (make-entry 'shore "country by the sea")
   (make-entry 'slay "kill")
   (make-entry 'slumber "sleep")
   (make-entry 'star-crossed "ill-fated")
   (make-entry 'steed "a horse")
   (make-entry 'stilly "still and quiet")
   (make-entry 'storied "celebrated in stories")
   (make-entry 'strand "a shore")
   (make-entry 'Stygian "very dark")
   (make-entry 'summer "a year of a person's age")
   (make-entry 'supernal "relating to the sky or the heavens")
   (make-entry 'susurration "a whispering or rustling sound")
   (make-entry 'swain "a young lover or suitor")
   (make-entry 'sword "the	military power; violence")
   (make-entry 'sylvan "wooded")
   (make-entry 'tarry "delay leaving")
   (make-entry 'temerarious "rash or reckless")
   (make-entry 'tenebrous "dark; shadowy")
   (make-entry 'threescore "sixty")
   (make-entry 'thrice "three times")
   (make-entry 'tidings "news; information")
   (make-entry 'toilsome "involving hard work")
   (make-entry 'tope "drink alcohol to excess")
   (make-entry 'travail "painful or laborious effort")
   (make-entry 'troublous "full of troubles")
   (make-entry 'tryst "a rendezvous between lovers")
   (make-entry 'unman "deprive of manly qualities")
   (make-entry 'vestal "chaste; pure")
   (make-entry 'vesture "clothing")
   (make-entry 'virescent "greenish")
   (make-entry 'viridescent "greenish or becoming green")
   (make-entry 'visage "a person's face")
   (make-entry 'want "lack or be short of")
   (make-entry 'wax "become larger or stronger")
   (make-entry 'wayfarer "a person who travels on foot")
   (make-entry 'wed "marry")
   (make-entry 'welkin "the	the sky or heaven")
   (make-entry 'whited "sepulchre	a hypocrite")
   (make-entry 'wind "blow (a bugle)")
   (make-entry 'without "outside")
   (make-entry 'wondrous "inspiring wonder")
   (make-entry 'wont "accustomed")
   (make-entry 'wonted "usual")
   (make-entry 'wrathful "extremely angry")
   (make-entry 'wreathe "twist or entwine")
   (make-entry 'yon "yonder; that")
   (make-entry 'yore "of former ties or long ago")
   (make-entry 'youngling "a young person or animal")
   (make-entry 'zephyr "a soft, gentle breeze")
   ))

(define dict
  (list->dict wl))

(define (meaning word)
  (display
   (entry-content
    (lookup word dict))))

;; it works perfectly, faster than I expected