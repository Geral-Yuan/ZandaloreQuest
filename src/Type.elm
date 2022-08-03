module Type exposing (Bag, Board, BoardState(..), Class(..), Critical(..), Dir(..), Enemy, FailToDo(..), GameMode(..), Hero, HeroState(..), Item, ItemType(..), Model, NPC, Obstacle, ObstacleType(..), Pos, RpgCharacter, Scene(..), Side(..), Spa_row, Task(..), Turn(..))

{-| This file fills every types related to the game.


# Type

@docs Bag, Board, BoardState, Class, Critical, Dir, Enemy, FailToDo, GameMode, Hero, HeroState, Item, ItemType, Model, NPC, Obstacle, ObstacleType, Pos, RpgCharacter, Scene, Side, Spa_row, Task, Turn

-}

-- For Type definition


type alias Pos =
    ( Int, Int )


type Scene
    = CastleScene
    | ShopScene
    | DungeonScene
    | Dungeon2Scene


type Task
    = MeetElder
    | FinishTutorial
    | GoToShop
    | Level Int
    | BeatBoss


type Critical
    = Less
    | None
    | Low
    | Medium
    | High


type Turn
    = PlayerTurn
    | TurretTurn
    | EnemyTurn


type BoardState
    = NoActions
    | EnemyAttack
    | TurretAttack
    | HeroAttack
    | HeroMoving
    | HeroHealth
    | HeroEnergy
    | Healing


type FailToDo
    = FailtoEnter Scene
    | FailtoTalk NPC
    | FailtoBuild
    | LackEnergy
    | Noop


type Class
    = Warrior
    | Archer
    | Assassin
    | Healer
    | Mage
    | Engineer
    | Turret


type ObstacleType
    = MysteryBox
    | Unbreakable


type ItemType
    = HealthPotion
    | EnergyPotion
    | Gold Int
    | NoItem


type HeroState
    = Waiting
    | Attacking
    | Attacked Int
    | Moving
    | TakingHealth Int
    | TakingEnergy
    | GettingHealed Int


type alias Obstacle =
    { obstacleType : ObstacleType
    , pos : Pos
    , itemType : ItemType
    }


type alias Item =
    { itemType : ItemType
    , pos : Pos
    }


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


type Dir
    = Left
    | Right
    | Up
    | Down


type Side
    = Hostile
    | Friend


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


type alias Bag =
    { coins : Int
    }


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


type alias Spa_row =
    { pos : Pos
    , path_length : Int
    , pre_pos : Maybe Pos
    }


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
