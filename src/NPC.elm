module NPC exposing (allNPC, npcMap)

{-| This file fills functions related to NPC.


# Function

@docs allNPC, npcMap

-}

import Type exposing (Dir(..), NPC, Scene(..), Task(..))


{-| This function will map between NPC's index and the NPC.
-}
npcMap : Int -> NPC
npcMap idx =
    case idx of
        1 ->
            npcElder

        2 ->
            npcWarrior

        3 ->
            npcArcher

        4 ->
            npcAssassin

        5 ->
            npcMage

        6 ->
            npcHealer

        7 ->
            npcEngineer

        8 ->
            npcDarkKnight1

        9 ->
            npcDarkKnight2

        10 ->
            npcSkullKnight1

        11 ->
            npcSkullKnight2

        12 ->
            npcSkullKnight3

        _ ->
            npcBoss


{-| This function will return a List of all NPCs.
-}
allNPC : List NPC
allNPC =
    List.map npcMap (List.range 1 13)


npcElder : NPC
npcElder =
    { scene = CastleScene
    , name = "Elder"
    , dialogue = "Elder: Welcome to the tutorial! The warrior and archer will help you on this arduous journey, choose one more hero to join you in this tutorial. Click anywhere to continue."
    , image = "ElderNPC"
    , faceDir = Left
    , position = ( 1300, 610 )
    , size = ( 64, 64 )
    , beaten = False
    , talkRange = ( ( 1200, 1427 ), ( 500, 720 ) )
    , task = MeetElder
    , level = 0
    }


npcDarkKnight1 : NPC
npcDarkKnight1 =
    { scene = CastleScene
    , name = "DarkKnight 1"
    , dialogue = "Zandalore was destined for failure, you are going down weakling! In this level, the board is a big hexagon. The recommended team composition is one melee, one ranged and one utility hero.  Select 3 heroes and kill all enemies. Click anywhere to continue."
    , image = "EvilNPC"
    , faceDir = Right
    , position = ( 630, 320 )
    , size = ( 64, 64 )
    , beaten = False
    , talkRange = ( ( 572, 667 ), ( 260, 390 ) )
    , task = Level 1
    , level = 1
    }


npcDarkKnight2 : NPC
npcDarkKnight2 =
    { scene = CastleScene
    , name = "DarkKnight 2"
    , dialogue = "Wow you seem to be stronger than the others Zandalorian, but you are going to die in my hands! Tip: Use the warrior to protect your ranged and utility heroes. The board is a big hexagon and there are 2 waves of enemies. Kill them all! Click anywhere to continue."
    , image = "EvilNPC"
    , faceDir = Right
    , position = ( 315, 810 )
    , size = ( 64, 64 )
    , beaten = False
    , talkRange = ( ( 315, 415 ), ( 750, 850 ) )
    , task = Level 2
    , level = 2
    }


npcSkullKnight1 : NPC
npcSkullKnight1 =
    { scene = DungeonScene
    , name = "SkullKnight 1"
    , dialogue = "Seeking revenge for your filthy kingdom eh? You are too weak kid! Tip: You should use more ranged heroes in this level. The board is a big hexagon with holes. Kill all 3 waves of enemies. Click anywhere to continue."
    , image = "SkullKnight"
    , faceDir = Right
    , position = ( 900, 350 )
    , size = ( 64, 64 )
    , beaten = False
    , talkRange = ( ( 800, 1000 ), ( 250, 450 ) )
    , task = Level 3
    , level = 3
    }


npcSkullKnight2 : NPC
npcSkullKnight2 =
    { scene = DungeonScene
    , name = "SkullKnight 2"
    , dialogue = "How dare you kill my fellow knights! Die!!! Tip: Make sure that your ranged heroes are far away from the enemies. The board is a big hexagon with holes and there are 3 waves of enemies. Kill them all! Click anywhere to continue."
    , image = "SkullKnight"
    , faceDir = Left
    , position = ( 1100, 350 )
    , size = ( 64, 64 )
    , beaten = False
    , talkRange = ( ( 1000, 1200 ), ( 250, 450 ) )
    , task = Level 4
    , level = 4
    }


npcSkullKnight3 : NPC
npcSkullKnight3 =
    { scene = Dungeon2Scene
    , name = "SkullKnight 3"
    , dialogue = "What?! A Zandalorian killed my knights? You shall perish!! Tip: This map will rotate so you will have to time your attacks and escapes well. There are 7 hexagons at the center with 2 rows of hexagons surrounding it and they will rotate. Kill all 3 waves of enemies! Click anywhere to continue."
    , image = "SkullKnight"
    , faceDir = Right
    , position = ( 900, 350 )
    , size = ( 64, 64 )
    , beaten = False
    , talkRange = ( ( 800, 1000 ), ( 250, 450 ) )
    , task = Level 5
    , level = 5
    }


npcBoss : NPC
npcBoss =
    { scene = Dungeon2Scene
    , name = "Boss"
    , dialogue = "How dare you! It was my mistake... You shouldn't be alive and I WILL KILL YOU! Tip: the boss can use the skills of all heroes, so do not get too close. The board contains 7 sets of 7 hexagons that will rotate. Kill the skull knights boss and bring glory! Click anywhere to continue."
    , image = "SkullKnight"
    , faceDir = Left
    , position = ( 1100, 350 )
    , size = ( 64, 64 )
    , beaten = False
    , talkRange = ( ( 1000, 1200 ), ( 250, 450 ) )
    , task = Level 6
    , level = 6
    }


npcWarrior : NPC
npcWarrior =
    { scene = CastleScene
    , name = "Warrior"
    , dialogue = ""
    , image = "WarriorBlue"
    , faceDir = Left
    , position = ( 1350, 550 )
    , size = ( 64, 64 )
    , beaten = True
    , talkRange = ( ( 0, 0 ), ( 0, 0 ) )
    , task = MeetElder
    , level = 0
    }


npcArcher : NPC
npcArcher =
    { scene = CastleScene
    , name = "Archer"
    , dialogue = ""
    , image = "ArcherBlue"
    , faceDir = Left
    , position = ( 1350, 670 )
    , size = ( 64, 64 )
    , beaten = True
    , talkRange = ( ( 0, 0 ), ( 0, 0 ) )
    , task = MeetElder
    , level = 0
    }


npcAssassin : NPC
npcAssassin =
    { scene = ShopScene
    , name = "Assassin"
    , dialogue = ""
    , image = "AssassinBlue"
    , faceDir = Left
    , position = ( 920, 370 )
    , size = ( 100, 100 )
    , beaten = True
    , talkRange = ( ( 0, 0 ), ( 0, 0 ) )
    , task = GoToShop
    , level = 0
    }


npcMage : NPC
npcMage =
    { scene = ShopScene
    , name = "Mage"
    , dialogue = ""
    , image = "MageBlue"
    , faceDir = Left
    , position = ( 830, 430 )
    , size = ( 100, 100 )
    , beaten = True
    , talkRange = ( ( 0, 0 ), ( 0, 0 ) )
    , task = GoToShop
    , level = 0
    }


npcHealer : NPC
npcHealer =
    { scene = ShopScene
    , name = "Healer"
    , dialogue = ""
    , image = "HealerBlue"
    , faceDir = Right
    , position = ( 740, 370 )
    , size = ( 100, 100 )
    , beaten = True
    , talkRange = ( ( 0, 0 ), ( 0, 0 ) )
    , task = GoToShop
    , level = 0
    }


npcEngineer : NPC
npcEngineer =
    { scene = ShopScene
    , name = "Engineer"
    , dialogue = ""
    , image = "EngineerBlue"
    , faceDir = Right
    , position = ( 650, 430 )
    , size = ( 100, 100 )
    , beaten = True
    , talkRange = ( ( 0, 0 ), ( 0, 0 ) )
    , task = GoToShop
    , level = 0
    }
