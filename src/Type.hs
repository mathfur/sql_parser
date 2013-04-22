{-# LANGUAGE DeriveDataTypeable #-}

module Type where

import Data.Data

data SQL = SQL [SelectStmt] deriving (Data, Typeable, Show, Eq)

data SelectStmt = SelectStmt SelectCore [OrderingTerm] (Maybe LimitTerm) deriving (Data, Typeable, Show, Eq)

data OrderingTerm = OrderingTerm Expr Order deriving (Data, Typeable, Show, Eq)

data Order = Asc | Desc deriving (Data, Typeable, Show, Eq)

data LimitTerm = LimitTerm Expr (Maybe Expr) deriving (Data, Typeable, Show, Eq)

data SelectCore = UnionAllOp SelectCore SelectCore
                | UnionOp SelectCore SelectCore
                | SelectCore [ResultColumn] JoinSource (Maybe WhereTerm) (Maybe GroupTerm)
                deriving (Data, Typeable, Show, Eq)

data ResultColumn = ResultColumn (Maybe TableName)
                  | ResultColumnExpr Expr (Maybe ColumnAlias)
                  deriving (Data, Typeable, Show, Eq)

data JoinSource = JoinSource SingleSource [LatterSource] deriving (Data, Typeable, Show, Eq)

data WhereTerm = WhereTerm Expr deriving (Data, Typeable, Show, Eq)

data GroupTerm = GroupTerm [Expr] (Maybe Expr) deriving (Data, Typeable, Show, Eq)

data LatterSource = LatterSource JoinOp SingleSource JoinConstraint deriving (Data, Typeable, Show, Eq)

data JoinOp = Outer | Inner deriving (Data, Typeable, Show, Eq)

data SingleSource = TableNameSingleSource DbNameAndTableName (Maybe TableAlias) | JoinSingleSource JoinSource deriving (Data, Typeable, Show, Eq)

data TableAlias = TableAlias String deriving (Data, Typeable, Show, Eq)
data ColumnAlias = ColumnAlias String deriving (Data, Typeable, Show, Eq)

data JoinConstraint =
  OnConstraint Expr |
  UsingConstraint [ColumnName]
    deriving (Data, Typeable, Show, Eq)


--------------------------------------------------
-- | about Expr

data Expr = LiteralValue LiteralValue
          | ColumnNameExpr (Maybe DbName) (Maybe TableName) ColumnName
          | UnaryOperatoredExpr UnaryOperator Expr
          | PlusOp Expr Expr
          | MinusOp Expr Expr
          | MultipleOp Expr Expr
          | DivideOp Expr Expr
          | NullExpr Expr
          | NotNullExpr Expr
          | EqualExpr Expr Expr
          | InExpr Expr (Maybe UnaryOperator) InnerInExpr
          | FunctionCall String [Expr]
          | LikeExpr (Maybe UnaryOperator) Expr Expr
          | BetweenExpr Expr (Maybe UnaryOperator) Expr Expr
            deriving (Data, Typeable, Show, Eq)

data InnerInExpr = InnerInExprs [Expr]
                 | InnerInTableName DbNameAndTableName
                 deriving (Data, Typeable, Show, Eq)

data LiteralValue = NumericLiteral String
                 | StringLiteral String
                 | Null
                 deriving (Data, Typeable, Show, Eq)

data UnaryOperator = NotOp deriving (Data, Typeable, Show, Eq)

data DbNameAndTableName = DbNameAndTableName (Maybe DbName) TableName deriving (Data, Typeable, Show, Eq)

data DbName = DbName String deriving (Data, Typeable, Show, Eq)
data TableName = TableName String deriving (Data, Typeable, Show, Eq)
data ColumnName = ColumnName String deriving (Data, Typeable, Show, Eq)

{-
AnalyzeStmt
AttachStmt
BeginStmt
CommitStmt
RollbackStmt
SavepointStmt
ReleaseStmt
CreateIndexStmt
IndexedColumn
CreateTableStmt
ColumnDef
TypeName
ColumnConstraint
SignedNumber
TableConstraint
ForeignKeyClause
ConflictClause
CreateTriggerStmt
CreateViewStmt
CreateVirtualTableStmt
DeleteStmt
DeleteStmtLimited
DetachStmt
DropIndexStmt
DropTableStmt
DropTriggerStmt
DropViewStmt
RaiseFunction
InsertStmt
PragmaStmt
PragmaValue
ReindexStmt
UpdateStmt
UpdateStmtLimited
QualifiedTableName
VacuumStmt
CommentSyntax
-}
