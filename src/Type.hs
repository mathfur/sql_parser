module Type where

data SQL = SQL [SelectStmt] deriving (Show, Eq)

data SelectStmt = SelectStmt SelectCore [OrderingTerm] (Maybe LimitTerm) deriving (Show, Eq)

data OrderingTerm = OrderingTerm Expr Order deriving (Show, Eq)

data Order = Asc | Desc deriving (Show, Eq)

data LimitTerm = LimitTerm Expr (Maybe Expr) deriving (Show, Eq)

data SelectCore = SelectCore [ResultColumn] JoinSource (Maybe WhereTerm) (Maybe GroupTerm) deriving (Show, Eq)

data ResultColumn = ResultColumn String deriving (Show, Eq)

data JoinSource = JoinSource SingleSource [LatterSource] deriving (Show, Eq)

data WhereTerm = WhereTerm Expr deriving (Show, Eq)

data GroupTerm = GroupTerm [Expr] (Maybe Expr) deriving (Show, Eq)

data LatterSource = LatterSource JoinOp SingleSource JoinConstraint deriving (Show, Eq)

data JoinOp = Outer | Inner deriving (Show, Eq) -- OK

data SingleSource = TableNameSingleSource TableName (Maybe TableAlias) | JoinSingleSource JoinSource deriving (Show, Eq)
  -- TODO: SelectSingleSource SelectStmt (Maybe TableAlias) |

data TableAlias = TableAlias String deriving (Show, Eq)


data JoinConstraint =
  OnConstraint Expr |
  UsingConstraint [ColumnName]
    deriving (Show, Eq)


--------------------------------------------------
-- | about Expr

data Expr = LiteralValue LiteralValue
          | ColumnNameExpr (Maybe DbName) (Maybe TableName_) ColumnName
          | UnaryOperatoredExpr UnaryOperator Expr
          | PlusOp Expr Expr
          | MinusOp Expr Expr
          | MultipleOp Expr Expr
          | DivideOp Expr Expr
          | NullExpr Expr
          | NotNullExpr Expr
          | InExpr Expr (Maybe UnaryOperator) InnerInExpr
            deriving (Show, Eq)

data InnerInExpr = InnerInExprs [Expr]
                 | InnerInTableName (Maybe DbName) TableName_
                 deriving (Show, Eq)


data LiteralValue = NumericLiteral String
                 | StringLiteral String
                 | Null
                 deriving (Show, Eq)

data UnaryOperator = NotOp deriving (Show, Eq)

data TableName = TableName (Maybe String) String deriving (Show, Eq) -- DatabaseName + TableName

data DbName = DbName String deriving (Show, Eq)
data TableName_ = TableName_ String deriving (Show, Eq)
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
CompoundOperator
UpdateStmt
UpdateStmtLimited
QualifiedTableName
VacuumStmt
CommentSyntax
-}
