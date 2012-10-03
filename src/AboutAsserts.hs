module HaskellKoans.AboutAsserts where

import HaskellKoans.KoanBase

aboutAsserts = 
    Koans "HaskellKoans.AboutAsserts"
        [ koanAboutQuestion
        , koanAboutBool
        , koanAboutEquality
        ]

koanAboutQuestion = Koan "about (???)" $
    assertBool 
        "Open AboutAsserts.hs and find 'koanAboutQuestion'. Replace (???) with True to pass" 
        (???)

koanAboutBool = Koan "assertBool" $
    assertBool "False should be True to pass" False

koanAboutEquality = Koan "assertEqual" $
    assertEqual "What does this sum equal? Replace (???) with the answer"
        (1+1)
        (???)

