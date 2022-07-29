module NPC exposing (..)

import Data exposing (Dir(..), NPC, Scene(..), Task(..))


allNPC : List NPC
allNPC =
    [ npcElder
    , npcDarkKnight1
    , npcDarkKnight2
    , npcSkullKnight1
    , npcSkullKnight2
    , npcSkullKnight3
    , npcBoss
    , npcWarrior
    , npcArcher
    , npcAssassin
    , npcMage
    , npcHealer
    , npcEngineer
    ]


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
    , dialogue = ""
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
    , dialogue = ""
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
    , dialogue = ""
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
    , dialogue = ""
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
    , dialogue = ""
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
    , dialogue = ""
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
