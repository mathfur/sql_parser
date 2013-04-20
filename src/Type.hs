module Type where

data SQL = SQL [SelectStmt] deriving (Show, Eq)

data SelectStmt = SelectStmt SelectCore [OrderingTerm] (Maybe LimitTerm) deriving (Show, Eq)

data OrderingTerm = OrderingTerm Expr Order deriving (Show, Eq)

data Order = Asc | Desc deriving (Show, Eq)

data LimitTerm = LimitTerm Expr (Maybe Expr) deriving (Show, Eq)

data SelectCore = UnionAllOp SelectCore SelectCore
                | UnionOp SelectCore SelectCore
                | SelectCore [ResultColumn] JoinSource (Maybe WhereTerm) (Maybe GroupTerm)
                deriving (Show, Eq)

data ResultColumn = ResultColumn (Maybe TableName)
                  | ResultColumnExpr Expr (Maybe ColumnAlias)
                  deriving (Show, Eq)

data JoinSource = JoinSource SingleSource [LatterSource] deriving (Show, Eq)

data WhereTerm = WhereTerm Expr deriving (Show, Eq)

data GroupTerm = GroupTerm [Expr] (Maybe Expr) deriving (Show, Eq)

data LatterSource = LatterSource JoinOp SingleSource JoinConstraint deriving (Show, Eq)

data JoinOp = Outer | Inner deriving (Show, Eq)

data SingleSource = TableNameSingleSource DbNameAndTableName (Maybe TableAlias) | JoinSingleSource JoinSource deriving (Show, Eq)

data TableAlias = TableAlias String deriving (Show, Eq)
data ColumnAlias = ColumnAlias String deriving (Show, Eq)


data JoinConstraint =
  OnConstraint Expr |
  UsingConstraint [ColumnName]
    deriving (Show, Eq)


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
          | InExpr Expr (Maybe UnaryOperator) InnerInExpr
          | FunctionCall String [Expr]
          | LikeExpr (Maybe UnaryOperator) Expr Expr
          | BetweenExpr Expr (Maybe UnaryOperator) Expr Expr
            deriving (Show, Eq)

data InnerInExpr = InnerInExprs [Expr]
                 | InnerInTableName DbNameAndTableName
                 deriving (Show, Eq)

data LiteralValue = NumericLiteral String
                 | StringLiteral String
                 | Null
                 deriving (Show, Eq)

data UnaryOperator = NotOp deriving (Show, Eq)

data DbNameAndTableName = DbNameAndTableName (Maybe DbName) TableName deriving (Show, Eq)

data DbName = DbName String deriving (Show, Eq)
data TableName = TableName String deriving (Show, Eq)
data ColumnName = ColumnName String deriving (Show, Eq)

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
