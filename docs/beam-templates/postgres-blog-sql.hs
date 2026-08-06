{-# LANGUAGE MultiParamTypeClasses #-}

-- ! BUILD_COMMAND: runhaskell -XStandaloneDeriving -XTypeSynonymInstances -XDeriveGeneric -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XAllowAmbiguousTypes -XDeriveAnyClass -XPartialTypeSignatures -fno-warn-partial-type-signatures
-- ! BUILD_DIR: cookbook-examples.
-- ! FORMAT: sql
module Main where

import Control.Exception (bracket)
import Control.Monad
import Data.Int
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import Database.Beam
import Database.Beam.Postgres hiding (runBeamPostgresDebug)
import qualified Database.Beam.Postgres as Pg
import Database.PostgreSQL.Simple (close, connectPostgreSQL, execute_)

data AuthorT f = Author
  { _authorId :: Columnar f Int32,
    _authorName :: Columnar f Text,
    _authorEmail :: Columnar f Text,
    _authorBio :: Columnar f (Maybe Text)
  }
  deriving (Generic)

type Author = AuthorT Identity

deriving instance Show Author

deriving instance Eq Author

instance Beamable AuthorT

instance Table AuthorT where
  data PrimaryKey AuthorT f = AuthorId (Columnar f Int32) deriving (Generic)
  primaryKey = AuthorId . _authorId

instance Beamable (PrimaryKey AuthorT)

type AuthorId = PrimaryKey AuthorT Identity

deriving instance Show AuthorId

deriving instance Eq AuthorId

deriving instance Show (PrimaryKey AuthorT (Nullable Identity))

deriving instance Eq (PrimaryKey AuthorT (Nullable Identity))

data PostT f = Post
  { _postId :: Columnar f Int32,
    _postTitle :: Columnar f Text,
    _postPublishedOn :: Columnar f (Maybe Day), -- Nothing while still a draft
    _postAuthor :: PrimaryKey AuthorT f,
    _postEditor :: PrimaryKey AuthorT (Maybe f) -- not every post is edited
  }
  deriving (Generic)

type Post = PostT Identity

deriving instance Show Post

deriving instance Eq Post

instance Beamable PostT

instance Table PostT where
  data PrimaryKey PostT f = PostId (Columnar f Int32) deriving (Generic)
  primaryKey = PostId . _postId

instance Beamable (PrimaryKey PostT)

type PostId = PrimaryKey PostT Identity

deriving instance Show PostId

deriving instance Eq PostId

data CommentT f = Comment
  { _commentId :: Columnar f Int32,
    _commentPost :: PrimaryKey PostT f,
    _commentAuthor :: PrimaryKey AuthorT (Maybe f), -- Nothing for anonymous comments
    _commentContent :: Columnar f Text,
    _commentPostedOn :: Columnar f Day
  }
  deriving (Generic)

type Comment = CommentT Identity

deriving instance Show Comment

deriving instance Eq Comment

instance Beamable CommentT

instance Table CommentT where
  data PrimaryKey CommentT f = CommentId (Columnar f Int32) deriving (Generic)
  primaryKey = CommentId . _commentId

instance Beamable (PrimaryKey CommentT)

type CommentId = PrimaryKey CommentT Identity

deriving instance Show CommentId

deriving instance Eq CommentId

-- * Database

data BlogDb f = BlogDb
  { _blogAuthors :: f (TableEntity AuthorT),
    _blogPosts :: f (TableEntity PostT),
    _blogComments :: f (TableEntity CommentT)
  }
  deriving (Generic, Database Postgres)

blogDb :: DatabaseSettings Postgres BlogDb
blogDb = defaultDbSettings

-- * Relationships

authorPosts :: OneToMany Postgres BlogDb s AuthorT PostT
authorPosts = oneToMany_ (_blogPosts blogDb) _postAuthor

editedPosts :: OneToManyOptional Postgres BlogDb s AuthorT PostT
editedPosts = oneToManyOptional_ (_blogPosts blogDb) _postEditor

postComments :: OneToMany Postgres BlogDb s PostT CommentT
postComments = oneToMany_ (_blogComments blogDb) _commentPost

authorComments :: OneToManyOptional Postgres BlogDb s AuthorT CommentT
authorComments = oneToManyOptional_ (_blogComments blogDb) _commentAuthor

main :: IO ()
main =
  bracket (connectPostgreSQL "host=localhost port=5432 dbname=postgres") close $ \conn -> do
    mapM_
      (execute_ conn)
      [ "CREATE TABLE authors \
        \  ( id    INT PRIMARY KEY \
        \  , name  VARCHAR NOT NULL \
        \  , email VARCHAR NOT NULL \
        \  , bio   VARCHAR \
        \  )",
        "CREATE TABLE posts \
        \  ( id           INT PRIMARY KEY \
        \  , title        VARCHAR NOT NULL \
        \  , published_on DATE \
        \  , author__id   INT NOT NULL REFERENCES authors(id) \
        \  , editor__id   INT REFERENCES authors(id) \
        \  )",
        "CREATE TABLE comments \
        \  ( id         INT PRIMARY KEY \
        \  , post__id   INT NOT NULL REFERENCES posts(id) \
        \  , author__id INT REFERENCES authors(id) \
        \  , content    VARCHAR NOT NULL \
        \  , posted_on  DATE NOT NULL \
        \  )"
      ]

    -- Seed data. Inserted silently, so that only the SQL of the example
    -- snippet is shown in the documentation.
    runBeamPostgres conn $ do
      runInsert $
        insert (_blogAuthors blogDb) $
          insertValues
            [ Author 1 "Ada Lovelace" "ada@example.com" (Just "Mathematician and first programmer."),
              Author 2 "Grace Hopper" "grace@example.com" Nothing,
              Author 3 "Alan Turing" "alan@example.com" (Just "Computer scientist and cryptanalyst.")
            ]

      runInsert $
        insert (_blogPosts blogDb) $
          insertValues
            [ Post 1 "Notes on the Analytical Engine" (Just (fromGregorian 2024 1 15)) (AuthorId 1) (AuthorId (Just 2)),
              Post 2 "Compilers from scratch" (Just (fromGregorian 2024 2 1)) (AuthorId 2) (AuthorId Nothing),
              Post 3 "On computable numbers" (Just (fromGregorian 2024 3 10)) (AuthorId 3) (AuthorId (Just 1)),
              Post 4 "Thoughts on the future" Nothing (AuthorId 1) (AuthorId Nothing),
              Post 5 "Debugging stories" (Just (fromGregorian 2024 4 5)) (AuthorId 2) (AuthorId (Just 3))
            ]

      runInsert $
        insert (_blogComments blogDb) $
          insertValues
            [ Comment 1 (PostId 1) (AuthorId (Just 2)) "Fascinating read!" (fromGregorian 2024 1 16),
              Comment 2 (PostId 1) (AuthorId Nothing) "Where can I learn more?" (fromGregorian 2024 1 17),
              Comment 3 (PostId 2) (AuthorId (Just 3)) "A great introduction." (fromGregorian 2024 2 2),
              Comment 4 (PostId 3) (AuthorId (Just 2)) "A classic." (fromGregorian 2024 3 11),
              Comment 5 (PostId 3) (AuthorId Nothing) "Mind-blowing." (fromGregorian 2024 3 12),
              Comment 6 (PostId 5) (AuthorId (Just 1)) "The moth story never gets old." (fromGregorian 2024 4 6)
            ]

    let runBeamPostgresDebug _ = Pg.runBeamPostgresDebug putStrLn

    ( do
        -- Don't print the result
        let print :: (Show a) => a -> IO ()
            print _ = pure ()

        BEAM_PLACEHOLDER
      )
