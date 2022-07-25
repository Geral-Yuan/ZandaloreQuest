module NPC exposing (..)

import Data exposing (..)


allNPC : List NPC
allNPC =
    [ npcElder
    , npcDarkKnight1
    , npcDarkKnight2
    , npcSkullKnight1
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
    , dialogue = []
    , image = "ElderNPC"
    , faceDir = Left
    , position = ( 1300, 610 )
    , size = ( 64, 64 )
    , beaten = False
    , talkRange = ( ( 1200, 1427 ), ( 500, 720 ) )
    , level = 0
    }


npcDarkKnight1 : NPC
npcDarkKnight1 =
    { scene = CastleScene
    , name = "DarkKnight 1"
    , dialogue = []
    , image = "EvilNPC"
    , faceDir = Right
    , position = ( 630, 320 )
    , size = ( 64, 64 )
    , beaten = False
    , talkRange = ( ( 572, 667 ), ( 260, 390 ) )
    , level = 1
    }


npcDarkKnight2 : NPC
npcDarkKnight2 =
    { scene = CastleScene
    , name = "DarkKnight 2"
    , dialogue = []
    , image = "EvilNPC"
    , faceDir = Right
    , position = ( 315, 810 )
    , size = ( 64, 64 )
    , beaten = False
    , talkRange = ( ( 315, 415 ), ( 750, 850 ) )
    , level = 2
    }


npcSkullKnight1 : NPC
npcSkullKnight1 =
    { scene = DungeonScene
    , name = "SkullKnight 1"
    , dialogue = []
    , image = "SkullKnight"
    , faceDir = Right
    , position = ( 900, 350 )
    , size = ( 64, 64 )
    , beaten = False
    , talkRange = ( ( 800, 1000 ), ( 250, 450 ) )
    , level = 3
    }


npcSkullKnight2 : NPC
npcSkullKnight2 =
    { scene = DungeonScene
    , name = "SkullKnight 2"
    , dialogue = []
    , image = "SkullKnight"
    , faceDir = Left
    , position = ( 1100, 350 )
    , size = ( 64, 64 )
    , beaten = False
    , talkRange = ( ( 1000, 1200 ), ( 250, 450 ) )
    , level = 4
    }


npcWarrior : NPC
npcWarrior =
    { scene = CastleScene
    , name = "Warrior"
    , dialogue = []
    , image = "WarriorBlue"
    , faceDir = Left
    , position = ( 1350, 550 )
    , size = ( 64, 64 )
    , beaten = True
    , talkRange = ( ( 0, 0 ), ( 0, 0 ) )
    , level = 0
    }


npcArcher : NPC
npcArcher =
    { scene = CastleScene
    , name = "Archer"
    , dialogue = []
    , image = "ArcherBlue"
    , faceDir = Left
    , position = ( 1350, 670 )
    , size = ( 64, 64 )
    , beaten = True
    , talkRange = ( ( 0, 0 ), ( 0, 0 ) )
    , level = 0
    }


npcAssassin : NPC
npcAssassin =
    { scene = ShopScene
    , name = "Assassin"
    , dialogue = []
    , image = "AssassinBlue"
    , faceDir = Left
    , position = ( 920, 370 )
    , size = ( 100, 100 )
    , beaten = True
    , talkRange = ( ( 0, 0 ), ( 0, 0 ) )
    , level = 0
    }


npcMage : NPC
npcMage =
    { scene = ShopScene
    , name = "Mage"
    , dialogue = []
    , image = "MageBlue"
    , faceDir = Left
    , position = ( 830, 430 )
    , size = ( 100, 100 )
    , beaten = True
    , talkRange = ( ( 0, 0 ), ( 0, 0 ) )
    , level = 0
    }


npcHealer : NPC
npcHealer =
    { scene = ShopScene
    , name = "Healer"
    , dialogue = []
    , image = "HealerBlue"
    , faceDir = Right
    , position = ( 740, 370 )
    , size = ( 100, 100 )
    , beaten = True
    , talkRange = ( ( 0, 0 ), ( 0, 0 ) )
    , level = 0
    }


npcEngineer : NPC
npcEngineer =
    { scene = ShopScene
    , name = "Engineer"
    , dialogue = []
    , image = "EngineerBlue"
    , faceDir = Right
    , position = ( 650, 430 )
    , size = ( 100, 100 )
    , beaten = True
    , talkRange = ( ( 0, 0 ), ( 0, 0 ) )
    , level = 0
    }
