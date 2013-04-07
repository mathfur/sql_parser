module Type where

data SQL = SQL [SelectStmt] deriving (Show, Eq)

data SelectStmt = SelectStmt [Column] deriving (Show, Eq)

data Column = Column String deriving (Show, Eq)

--  data SelectStmt = SelectStmt [SelectCore] [OrderingTerm] (Just LimitExpr)
--  
--  data SelectCore = SelectCore SelectDistinct [ResultColumn]
--     data SelectDistinct = All | Distinct
--     data ResultColumn = All

-- data OrderingTerm = OrderingTerm String
-- data LimitExpr = LimitExpr Int (Maybe Int)

-- data SqlStmtList = SqlStmtList [SqlStmt] deriving (Show, Eq)
-- data SqlStmt = SqlStmt String deriving (Show, Eq)
-- 
-- AlterTableStmt

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
SelectStmt
SelectCore
ResultColumn
JoinSource
SingleSource
JoinOp
JoinConstraint
OrderingTerm
CompoundOperator
UpdateStmt
UpdateStmtLimited
QualifiedTableName
VacuumStmt
CommentSyntax
-}
