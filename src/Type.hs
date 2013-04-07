module Type where

data SQL = SQL [SelectStmt] deriving (Show, Eq)

data SelectStmt = SelectStmt [ResultColumn] JoinSource deriving (Show, Eq)

data ResultColumn = ResultColumn String deriving (Show, Eq)

data JoinSource = JoinSource SingleSource [LatterSource] deriving (Show, Eq)

data LatterSource = LatterSource JoinOp SingleSource JoinConstraint deriving (Show, Eq)

data JoinOp = Outer | Inner deriving (Show, Eq) -- OK

data SingleSource = TableNameSingleSource TableName (Maybe TableAlias) | JoinSingleSource JoinSource deriving (Show, Eq)
  -- TODO: SelectSingleSource SelectStmt (Maybe TableAlias) |

data TableAlias = TableAlias String deriving (Show, Eq)

data TableName = TableName (Maybe String) String deriving (Show, Eq) -- DatabaseName + TableName

data JoinConstraint =
  OnConstraint String | -- TODO: change to expr
  UsingConstraint [ColumnName]
    deriving (Show, Eq)

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
Expr
RaiseFunction
LiteralValue
NumericLiteral
InsertStmt
PragmaStmt
PragmaValue
ReindexStmt
SelectCore
OrderingTerm
CompoundOperator
UpdateStmt
UpdateStmtLimited
QualifiedTableName
VacuumStmt
CommentSyntax
-}
