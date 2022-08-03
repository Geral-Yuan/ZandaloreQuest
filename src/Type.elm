module Type exposing (Bag, Board, BoardState(..), Class(..), Critical(..), Dir(..), Enemy, FailToDo(..), GameMode(..), Hero, HeroState(..), Item, ItemType(..), Model, NPC, Obstacle, ObstacleType(..), Pos, RpgCharacter, Scene(..), Side(..), Spa_row, Task(..), Turn(..))

{-| This file fills every types related to the game.


# Type

@docs Bag, Board, BoardState, Class, Critical, Dir, Enemy, FailToDo, GameMode, Hero, HeroState, Item, ItemType, Model, NPC, Obstacle, ObstacleType, Pos, RpgCharacter, Scene, Side, Spa_row, Task, Turn

-}

-- For Type definition


{-| This is the Pos type
-}
type alias Pos =
    ( Int, Int )


{-| This is the Scene type
-}
type Scene
    = CastleScene
    | ShopScene
    | DungeonScene
    | Dungeon2Scene


{-| This is the Task type
-}
type Task
    = MeetElder
    | FinishTutorial
    | GoToShop
    | Level Int
    | BeatBoss


{-| This is the Critical type
-}
type Critical
    = Less
    | None
    | Low
    | Medium
    | High


{-| This is the Turn type
-}
type Turn
    = PlayerTurn
    | TurretTurn
    | EnemyTurn


{-| This is the Boardstate type
-}
type BoardState
    = NoActions
    | EnemyAttack
    | TurretAttack
    | HeroAttack
    | HeroMoving
    | HeroHealth
    | HeroEnergy
    | Healing


{-| This is the FailToDo type
-}
type FailToDo
    = FailtoEnter Scene
    | FailtoTalk NPC
    | FailtoBuild
    | LackEnergy
    | Noop


{-| This is the Class type
-}
type Class
    = Warrior
    | Archer
    | Assassin
    | Healer
    | Mage
    | Engineer
    | Turret


{-| This is the ObstacleType type
-}
type ObstacleType
    = MysteryBox
    | Unbreakable


{-| This is the IteamType type
-}
type ItemType
    = HealthPotion
    | EnergyPotion
    | Gold Int
    | NoItem


{-| This is the HeroState type
-}
type HeroState
    = Waiting
    | Attacking
    | Attacked Int
    | Moving
    | TakingHealth Int
    | TakingEnergy
    | GettingHealed Int


{-| This is Obstacle type
-}
type alias Obstacle =
    { obstacleType : ObstacleType
    , pos : Pos
    , itemType : ItemType
    }


{-| This is Item type
-}
type alias Item =
    { itemType : ItemType
    , pos : Pos
    }


{-| This is the Hero type
-}
type alias Hero =
    { class : Class
    , pos : Pos
    , maxHealth : Int
    , health : Int
    , damage : Int
    , energy : Int
    , selected : Bool
    , state : HeroState
    , indexOnBoard : Int --give an index to the heroes on the board
    }


{-| This is the Enemy type
-}
type alias Enemy =
    { class : Class
    , pos : Pos
    , maxHealth : Int
    , health : Int
    , damage : Int
    , steps : Int
    , done : Bool
    , state : HeroState
    , justAttack : Bool
    , indexOnBoard : Int --give an index to the enemies on the board
    , boss : Bool
    , bossState : Int
    }


{-| This is the NPC type
-}
type alias NPC =
    { scene : Scene
    , name : String
    , dialogue : String
    , image : String
    , faceDir : Dir
    , position : ( Float, Float )
    , size : ( Float, Float )
    , beaten : Bool
    , talkRange : ( ( Float, Float ), ( Float, Float ) )
    , task : Task
    , level : Int
    }


{-| This is the Dir type
-}
type Dir
    = Left
    | Right
    | Up
    | Down


{-| This is the Side type
-}
type Side
    = Hostile
    | Friend


{-| This is the GameMode type
-}
type GameMode
    = Castle
    | Shop
    | Dungeon
    | Dungeon2
    | BuyingItems
    | UpgradePage
    | DrawHero Class
    | HeroChoose
    | BoardGame
    | Summary
    | Logo
    | Tutorial Int
    | Dialog Task
    | Encyclopedia Class


{-| This is the Bag type
-}
type alias Bag =
    { coins : Int
    }


{-| This is the RpgCHaracter type
-}
type alias RpgCharacter =
    { pos : ( Float, Float )
    , moveLeft : Bool
    , moveRight : Bool
    , moveUp : Bool
    , moveDown : Bool
    , faceDir : Dir
    , height : Float
    , width : Float
    , speed : Float
    , move_range : ( Float, Float ) -- right bound and bottom bound
    }


{-| This is the Spa\_row type
-}
type alias Spa_row =
    { pos : Pos
    , path_length : Int
    , pre_pos : Maybe Pos
    }


{-| This is the Board type
-}
type alias Board =
    { map : List Pos
    , obstacles : List Obstacle
    , enemies : List Enemy
    , heroes : List Hero
    , totalHeroNumber : Int
    , turn : Turn
    , cntEnemy : Int
    , cntTurret : Int
    , boardState : BoardState
    , critical : Int
    , attackable : List Pos
    , enemyAttackable : List Pos
    , skillable : List Pos
    , target : List Pos
    , item : List Item
    , timeTurn : Float
    , timeBoardState : Float
    , spawn : Int -- number of times group of enemies will be spawned
    , index : Int -- highest enemies index
    , pointPos : ( Float, Float )
    , coins : Int
    , level : Int
    , mapRotating : ( Bool, Float )
    , popUpHint : ( FailToDo, Float )
    , hintOn : Bool
    }


{-| This is the Model type
-}
type alias Model =
    { mode : GameMode
    , indexedheroes : List ( Hero, Int ) -- each hero linked to an index where 0 means not obtained so far
    , upgradePageIndex : Int
    , board : Board
    , size : ( Float, Float )
    , character : RpgCharacter
    , chosenHero : List Int
    , bag : Bag
    , previousMode : GameMode
    , level : Int
    , time : Float
    , cntTask : Task
    , npclist : List NPC
    , unlockShop : Bool
    , unlockDungeon : Bool
    , unlockDungeon2 : Bool
    , popUpHint : ( FailToDo, Float )
    , test : Bool
    , isDisplayUpgrade : Bool

    -- , time : Float
    }
