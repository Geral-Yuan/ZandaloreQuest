module NPC exposing (..)

import Data exposing (..)


npcElder : NPC
npcElder =
    { scene = CastleScene
    , name = "Elder"
    , dialogue = []
    , image = "EvilNPC" -- To be modified to "Elder"
    , faceDir = Left
    , position = ( 1300, 610)
    , size = ( 64, 64 )
    , beaten = False
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
    , beaten = False
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
    , beaten = False
    }


npcMage : NPC
npcMage =
    { scene = ShopScene
    , name = "Mage"
    , dialogue = []
    , image = "MageBlue"
    , faceDir = Left
    , position = ( 880, 420 )
    , size = ( 84, 84 )
    , beaten = False
    }


npcHealer : NPC
npcHealer =
    { scene = ShopScene
    , name = "Healer"
    , dialogue = []
    , image = "HealerBlue"
    , faceDir = Right
    , position = ( 790, 420 )
    , size = ( 84, 84 )
    , beaten = False
    }


npcEngineer : NPC
npcEngineer =
    { scene = ShopScene
    , name = "Engineer"
    , dialogue = []
    , image = "EngineerBlue"
    , faceDir = Right
    , position = ( 700, 420 )
    , size = ( 84, 84 )
    , beaten = False
    }
